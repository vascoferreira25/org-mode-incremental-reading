;;; incremental-reading.el --- SuperMemo inspired incremental reading for org-mode and Anki -*- lexical-binding: t -*-

;; Author: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Maintainer: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Created: 10 Sep 2021
;; Version: 0.3
;; Keywords: anki anki-editor incremental-reading supermemo
;; Homepage: https://github.com/vascoferreira25/incremental-reading
;; Package-Requires: ((org) (anki-editor "0.3.3") (request "0.3.0") (emacs "26.6"))

;; This file is not part of GNU Emacs.


;;; Commentary:

;; Read your notes and create anki cards. Inspired by SuperMemo Incremental
;; Reading (IR). Your cards stay close to your notes so you don't forget to
;; updated the cards when you add and update your notes. There is no need to
;; use a tree structure like anki-editor.

;;; Dependencies:
(require 'org)
(require 'ox-html)
(require 'org-protocol)
(require 'anki-editor)
(require 'request)


(defgroup incremental-reading nil
  "Customizations for incremental-reading."
  :group 'org)


(defcustom incremental-reading-default-deck
  "Default"
  "The default deck to be used in the
`incremental-reading-extract-basic' and
`incremental-reading-extract-cloze' functions."
  :type '(string))


(defcustom incremental-reading-default-tags
  "incremental-reading"
  "The default tags to be used in the
`incremental-reading-extract-basic' and
`incremental-reading-extract-cloze' functions."
  :type '(string))

(defcustom incremental-reading-hide-default-display
  ">>>>>>\n"
  "The default string displayed when properties are hidden."
  :type '(string))

(defcustom incremental-reading--basic-template
  ":ANKI-CARD:
#+ATTR_DECK: %s
#+ATTR_TYPE: Basic
#+ATTR_TAGS: %s
#+BEGIN_ANKI org
#+ATTR_FIELD: Front
#+BEGIN_FIELD
%s
#+END_FIELD

#+ATTR_FIELD: Back
#+BEGIN_FIELD
#+END_FIELD
#+END_ANKI
:END:
\n"
  "The default template for the basic card extract."
  :type '(string))


(defcustom incremental-reading--basic-template-no-back
  ":ANKI-CARD:
#+ATTR_DECK: %s
#+ATTR_TYPE: Basic
#+ATTR_TAGS: %s
#+BEGIN_ANKI org
#+ATTR_FIELD: Front
#+BEGIN_FIELD
%s
#+END_FIELD
#+END_ANKI
:END:
\n"
  "The default template for the basic card extract without the
back field."
  :type '(string))


(defcustom incremental-reading--cloze-template
  ":ANKI-CARD:
#+ATTR_DECK: %s
#+ATTR_TYPE: Cloze
#+ATTR_TAGS: %s
#+BEGIN_ANKI org
#+ATTR_FIELD: Text
#+BEGIN_FIELD
%s
#+END_FIELD

#+ATTR_FIELD: Back Extra
#+BEGIN_FIELD
#+END_FIELD
#+END_ANKI
:END:
\n"
  "The default template for the cloze card extract."
  :type '(string))


(defcustom incremental-reading--cloze-template-no-back
  ":ANKI-CARD:
#+ATTR_DECK: %s
#+ATTR_TYPE: Cloze
#+ATTR_TAGS: %s
#+BEGIN_ANKI org
#+ATTR_FIELD: Text
#+BEGIN_FIELD
%s
#+END_FIELD
#+END_ANKI
:END:
\n"
  "The default template for the cloze card extract without the
back field."
  :type '(string))


(defun incremental-reading--transform-field-content (content)
  "Transform the CONTENT of a field into html."
  (let* ((contents-begin (org-element-property :contents-begin content))
         (contents-end (org-element-property :contents-end content)))
    (cond
     ((cl-equalp 'src-block (org-element-type content))
      (org-html-src-block content nil nil))
     ((and contents-begin contents-end)
      (or (org-export-string-as 
           (buffer-substring-no-properties
            contents-begin
            contents-end)
           anki-editor--ox-anki-html-backend
           t
           anki-editor--ox-export-ext-plist)
          ;; Return empty string because empty fields return nil
          ""))
     (t ""))))


(defun incremental-reading--transform-field (field)
  "Transform a special block FIELD to a field in the anki card
and return a list with the field-name and the parsed-contents."
  (let* ((field-name (car (org-element-property :attr_field field)))
         (field-contents (org-element-contents field))
         (parsed-contents
          ;; Transform all the separate paragraphs and elements to a single
          ;; string.
          (cl-reduce (lambda (a b) (format "%s\n%s" a b))
                     (mapcar #'incremental-reading--transform-field-content
                             field-contents))))
    `(,field-name . ,parsed-contents)))


(defun incremental-reading--get-fields (anki-block)
  "Return all the fields inside an ANKI-BLOCK."
  (org-element-map anki-block 'special-block
    (lambda (field)
      (when (string= "FIELD" (upcase (org-element-property :type field)))
        (incremental-reading--transform-field field)))))


(defun incremental-reading/add-note-id (anki-block id)
  "Add the card ID to the ANKI-BLOCK."
  (save-excursion
    (goto-char (org-element-property :begin anki-block))
    (insert (format "#+ATTR_ID: %s\n" id))))


(defun incremental-reading--request-add-card (deck card-type fields tags anki-block)
  "Send an http request to the anki-connect addon with DECK,
CARD-TYPE, FIELDS, TAGS and ANKI-BLOCK of the card to add. If it
is successful, update the ANKI-BLOCK id."
  (request
    "http://127.0.0.1:8765"
    :type "POST"
    :sync t
    :data (json-encode `(("action" . "addNote")
                         ("version" . "6")
                         ("params" . (("note" . (("deckName" . ,deck)
                                                 ("modelName" . ,card-type)
                                                 ("fields" . ,fields)
                                                 ("tags" . (,tags))))))))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key response &allow-other-keys)
                (message "Added card.")
                (incremental-reading/add-note-id
                 anki-block
                 (cdr (assoc 'result (request-response-data response))))))))


(defun incremental-reading--request-update-card (id fields tags)
  "Send an http request to the anki-connect addon with the ID,
FIELDS and TAGS of the card."
  (request
    "http://127.0.0.1:8765"
    :type "POST"
    :sync t
    :data (json-encode `(("action" . "updateNoteFields")
                         ("version" . "6")
                         ("params" . (("note" . (("id" . ,(string-to-number id))
                                                 ("fields" . ,fields)
                                                 ("tags" . (,tags))))))))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key response &allow-other-keys)
                (message "Updated card.")))))


;;;###autoload
(defun incremental-reading-parse-cards ()
  "Parse all the Anki special-blocks in the current buffer and
send them to Anki through http to the anki-connect addon."
  (interactive)
  ;; Store a reversed list of the special blocks.
  ;; This is used to write the ids from bottom to top
  (setq anki-blocks (list))
  ;; Parse org-buffer and get all the special-block elements
  (org-element-map (org-element-parse-buffer) 'special-block
    (lambda (special-block)
      ;; If it is a anki block, get the fields of the card
      (when (string= "ANKI" (s-upcase (org-element-property :type special-block)))
        (setq anki-blocks (cons special-block anki-blocks)))))
  ;; Avoid errors when inserting the id by sorting the elements from bottom to
  ;; top
  (setq anki-blocks (sort (copy-sequence anki-blocks)
                          (lambda (a b)
                            (let* ((a-begin (org-element-property :begin a))
                                   (b-begin (org-element-property :begin b)))
                              (> a-begin b-begin)))))
  ;; Process each anki-block
  (mapcar #'incremental-reading-parse-card
          anki-blocks))

(defun incremental-reading-parse-card (anki-block)
  "Parse one card"
  (let* ((anki-card-fields (incremental-reading--get-fields anki-block))
         (id (car (org-element-property :attr_id anki-block)))
         (deck (car (org-element-property :attr_deck anki-block)))
         (card-type (car (org-element-property :attr_type anki-block)))
         (tags (car (org-element-property :attr_tags anki-block))))
    (if-let*
        ((current-file
          (when-let ((f (buffer-file-name)))
            (abbreviate-file-name f)))
         (field-name
          (cond
           ((string= card-type "Basic") "Back")
           ((string= card-type "Basic (and reversed card)") "Note")
           ((string= card-type "Cloze") "Back Extra")
           ((string= card-type "Cloze with typed text") "Back Extra")
           (t nil)))
         (source-string
          (format "<div><br><p><a href=\"org-protocol://open-file?file=%s\">Source</p></div>"
                  (url-hexify-string current-file)
                  (org-html-encode-plain-text current-file)))
         (field (assoc field-name anki-card-fields)))
        (setf (alist-get field-name anki-card-fields nil nil #'equal)
              (concat (cdr field) source-string))
      (nconc anki-card-fields `((,field-name . ,source-string))))
    (if id
        (incremental-reading--request-update-card id anki-card-fields tags)
      (incremental-reading--request-add-card deck
                                             card-type
                                             anki-card-fields
                                             tags
                                             anki-block))))


(defun incremental-reading--extract-text (selection-start selection-end)
  "Extract the substring on region with SELECTION-START and
  SELECTION-END and return a substring without properties."
  (buffer-substring-no-properties selection-start
                                  selection-end))


(defun incremental-reading-get-tags ()
  "Get the tags of the current heading."
  (message "%S" (org-roam-node-tags (org-roam-node-at-point)))
  (concat incremental-reading-default-tags
          " "
          (if (org-roam-node-at-point)
              (mapconcat 'identity
                         (org-roam-node-tags (org-roam-node-at-point)) " ")
            (mapconcat 'identity (split-string (org-get-tags-string) ":" t) " ")
            )))


;;;###autoload
(defun incremental-reading-extract-basic ()
  "Extract current region into a basic anki card."
  (interactive)
  (let* ((element (org-element-at-point))
         (selection-start (region-beginning))
         (selection-end (region-end))
         (tags (incremental-reading-get-tags)))
    (goto-char (org-element-property :end element))
    (insert (format incremental-reading--basic-template
                    incremental-reading-default-deck
                    tags
                    (incremental-reading--extract-text selection-start
                                                       selection-end)))))


;;;###autoload
(defun incremental-reading-extract-basic-no-back ()
  "Extract current region into a basic anki card without the back
field."
  (interactive)
  (let* ((element (org-element-at-point))
         (selection-start (region-beginning))
         (selection-end (region-end))
         (tags (incremental-reading-get-tags)))
    (goto-char (org-element-property :end element))
    (insert (format incremental-reading--basic-template-no-back
                    incremental-reading-default-deck
                    tags
                    (incremental-reading--extract-text selection-start
                                                       selection-end)))))


;;;###autoload
(defun incremental-reading-extract-cloze ()
"Extract current region into a cloze anki card."
(interactive)
(let* ((element (org-element-at-point))
       (selection-start (region-beginning))
       (selection-end (region-end))
       (tags (incremental-reading-get-tags)))
  (goto-char (org-element-property :end element))
  (insert (format incremental-reading--cloze-template
                  incremental-reading-default-deck
                  tags
                  (incremental-reading--extract-text selection-start
                                                     selection-end)))))


;;;###autoload
(defun incremental-reading-extract-cloze-no-back ()
"Extract current region into a cloze anki card without the back
field."
(interactive)
(let* ((element (org-element-at-point))
       (selection-start (region-beginning))
       (selection-end (region-end))
       (tags (incremental-reading-get-tags)))
  (goto-char (org-element-property :end element))
  (insert (format incremental-reading--cloze-template-no-back
                  incremental-reading-default-deck
                  tags
                  (incremental-reading--extract-text selection-start
                                                     selection-end)))))

;;;###autoload
(defun incremental-reading-hide-properties ()
  "Hide all mess properties."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'special-block
    (lambda (special-block)
      (when (string= "ANKI" (s-upcase (org-element-property :type special-block)))
        (let ((drawer-begin (org-element-property :begin special-block))
              (drawer-end (org-element-property :end special-block))
              (context-bg-eds (list))
              (produce-list (list)))
          (org-element-map special-block 'special-block
            (lambda (field)
              (when (string= "FIELD" (s-upcase (org-element-property :type field)))
                ;; Get the each field's context begin and end position list.
                ;; Should looks like: '(1 3 8 19), 1 3 is the first field's begin-end, and etc.
                (push (org-element-property :contents-begin field) context-bg-eds)
                (push (org-element-property :contents-end field) context-bg-eds))))
          (push drawer-end produce-list)
          (while context-bg-eds
            (push (pop context-bg-eds) produce-list))
          (push drawer-begin produce-list)
          (while produce-list
            (let ((ol (make-overlay (pop produce-list) (pop produce-list))))
              (overlay-put ol 'display incremental-reading-hide-default-display)
              (overlay-put ol 'hidden-anki-prop t)))
          (put 'incremental-reading-properties-hide-state 'state 'hidden))))))

;;;###autoload
(defun incremental-reading-show-properties ()
  "Show all properties."
  (interactive)
  (remove-overlays
   (point-min) (point-max) 'hidden-anki-prop t)
  (put 'incremental-reading-properties-hide-state 'state 'shown))

;;;###autoload
(defun incremental-reading-toggle-properties-display ()
  "Toggle properties hidden/shown state."
  (interactive)
  (if (eq (get 'incremental-reading-properties-hide-state 'state) 'hidden)
      (incremental-reading-show-properties)
    (incremental-reading-hide-properties)))

;; Create a minor mode to make it easier to load incremental-reading through
;; hooks. This was sugested by `czqhurricnae'.
;;;###autoload
(define-minor-mode incremental-reading-mode
  "incremental-reading-mode"
  :lighter " incremental-reading")

(provide 'incremental-reading)

;;; incremental-reading.el ends here

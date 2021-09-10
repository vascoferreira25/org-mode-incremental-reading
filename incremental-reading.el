;; incremental-reading.el --- SuperMemo inspired incremental reading for org-mode and Anki -*- lexical-binding: t -*-

;; Author: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Maintainer: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Created: 10 Sep 2021
;; Keywords: anki anki-editor incremental-reading supermemo
;; Homepage: https://github.com/vascoferreira25/incremental-reading
;; Package-Requires: ((org) (ox-html) (anki-editor) (dash) (s) (request))

;; This file is not part of GNU Emacs.


;;; Commentary:

;; Read your notes and create anki cards. Inspired by SuperMemo Incremental
;; Reading (IR). Your cards stay close to your notes so you don't forget to
;; updated the cards when you add and update your notes. There is no need to
;; use a tree structure like anki-editor.

;;; Dependencies:
(require 'org)
(require 'ox-html)
(require 'anki-editor)
(require 'dash)
(require 's)
(require 'request)


(defun incremental-reading/transform-field (field)
  "Transform a special block to a field to be used in anki cards."
  (let* ((field-name (-first-item (org-element-property :attr_field field)))
         (field-contents (org-element-contents field))
         (parsed-contents
          (--reduce (format "%s\n%s" acc it)
                    (-map (lambda (content)
                            (if (equalp 'src-block (org-element-type content))
                                (org-html-src-block content nil nil)
                              (org-export-string-as 
                               (buffer-substring-no-properties
                                (org-element-property :contents-begin content)
                                (org-element-property :contents-end content))
                               anki-editor--ox-anki-html-backend
                               t
                               anki-editor--ox-export-ext-plist)))
                          field-contents))))
    `(,field-name . ,parsed-contents)))


(defun incremental-reading/get-fields (anki-block)
  (org-element-map anki-block 'special-block
    (lambda (field)
      (when (s-equals? "FIELD" (s-upcase (org-element-property :type field)))
        (incremental-reading/transform-field field)))))


(defun incremental-reading/request-update-card (id fields tags)
  (request
    "http://127.0.0.1:8765"
    :type "POST"
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


(defun incremental-reading/add-note-id (anki-block id)
  (goto-char (org-element-property :begin anki-block))
  (insert (format "#+ATTR_ID: %s\n" id)))


(defun incremental-reading/request-add-card (deck card-type fields tags anki-block)
  (request
    "http://127.0.0.1:8765"
    :type "POST"
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


;;;###autoload
(defun incremental-reading/parse-cards ()
  (interactive)
  ;; Store a reversed list of the special blocks.
  ;; This is used to write the ids from bottom to top
  (setq anki-blocks (list))
  ;; Parse org-buffer and get all the special-block elements
  (org-element-map (org-element-parse-buffer) 'special-block
    (lambda (special-block)
      ;; If it is a anki block, get the fields of the card
      (when (s-equals? "ANKI" (s-upcase (org-element-property :type special-block)))
        (setq anki-blocks (cons special-block anki-blocks)))))
  (-map (lambda (anki-block)
          (let* ((anki-card-fields (incremental-reading/get-fields anki-block))
                 (id (-first-item (org-element-property :attr_id anki-block)))
                 (deck (-first-item (org-element-property :attr_deck anki-block)))
                 (card-type (-first-item (org-element-property :attr_type anki-block)))
                 (tags (-first-item (org-element-property :attr_tags anki-block))))
            (if id
                (incremental-reading/request-update-card id anki-card-fields tags)
              (incremental-reading/request-add-card deck card-type anki-card-fields tags anki-block)))
          )
        anki-blocks))


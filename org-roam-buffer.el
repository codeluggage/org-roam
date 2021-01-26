;;; org-roam-buffer.el --- Metadata buffer -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides the org-roam-buffer functionality for org-roam
;;; Code:
;;;; Library Requires
(require 'eieio)
(require 'magit-section)

;; Faces
(defface org-roam-title
  '((t :weight bold))
  "Face for Org-roam titles."
  :group 'org-roam-faces)

;;; Widget Class Definition
(cl-deftype function ()
  '(satisfies fboundp))

(defclass org-roam-widget ()
  ((name :initarg :name
         :type symbol
         :custom symbol
         :documentation "The symbol for the widget.")
   (header :initarg :header
           :initform ""
           :type string
           :custom string
           :documentation "The header used for this widget")
   (items :initarg :items
          :type function
          :custom function
          :documentation "The function to call to obtain the items.
Items are of the form: ((key (list of values for key)))")
   (render :initarg :render
           :type function
           :custom function
           :documentation "The function used to render the items.")
   (show-p :initarg :show-p
           :type function
           :custom function
           :documentation "The predicate function to use to see if the widget should be used.")))

(cl-defmethod org-roam-widget-show ((widget org-roam-widget) node)
  "Return t if WIDGET is to be shown, nil otherwise."
  (funcall (oref widget show-p) node))

(cl-defmethod org-roam-widget-render ((widget org-roam-widget) node)
  "Render items in WIDGET."
  (magit-insert-section (widget-root)
    (magit-insert-heading (oref widget header))
    (if-let* ((items (funcall (oref widget items) node))
              (not-empty (not (funcall (oref widget render) items))))
        (insert "\n")
      (magit-cancel-section))))

;;; Widgets
;;;; Backlinks Widget
(defun org-roam-widget-get-backlinks (node)
  (org-roam-db-query
   [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
    :from links
    :left :join nodes s :on (= links:source s:id)
    :left :join nodes d :on (= links:dest d:id)
    :where (= dest $v1)]
   (vector node)))

(defun org-roam-widget-show-backlinks-p (node)
  t)

(defun org-roam-buffer--preview (file point)
  "Get preview content for FILE at POINT."
  (org-roam--with-temp-buffer file
    (goto-char point)
    (let ((elem (org-element-at-point)))
      (or (org-element-property :raw-value elem)
          (when-let ((begin (org-element-property :begin elem))
                     (end (org-element-property :end elem)))
            (string-trim (buffer-substring-no-properties begin end)))))))

(defun org-roam-widget-render-backlinks (backlinks)
  (setq backlinks (seq-group-by #'car backlinks))
  (let (source values)
    (dolist (backlink backlinks)
      (setq source (car backlink)
            values (cdr backlink))
      (magit-insert-section (backlinks-file)
        (pcase-dolist (`(,source ,source-file ,pos ,source-title ,dest ,dest-title ,props) values)
          (propertize source-title 'font-lock-face 'org-roam-title)
          (magit-insert-heading source-title)
          (let ((outline (or (plist-get props :outline) '("Top"))))
            (magit-insert-section (backlink-outline)
              (magit-insert-heading (org-fontify-like-in-org-mode
                                     (-> outline
                                         (string-join " > "))))
              (magit-insert-section (backlink-preview)
                (insert (org-fontify-like-in-org-mode
                         (org-roam-buffer--preview source-file pos)) "\n"))))))))
  nil)

(defvar org-roam-widget-backlinks
  (org-roam-widget :name 'backlinks
                   :header "Backlinks:"
                   :items #'org-roam-widget-get-backlinks
                   :render #'org-roam-widget-render-backlinks
                   :show-p #'org-roam-widget-show-backlinks-p)
  "Widget for backlinks.")

;;;; Citelinks Widget
(defun org-roam-widget-show-reflinks-p (node)
  ;; TODO
  nil)

(defun org-roam-widget-get-reflinks (node)
  ;; TODO
  (org-roam-db-query
   [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
    :from links
    :left :join nodes s :on (= links:source s:id)
    :left :join nodes d :on (= links:dest d:id)
    :where (= dest $v1)]
   (vector node)))

(defun org-roam-widget-render-reflinks (items)
  ;; TODO
  (let ((empty t)
        file-from reflinks content)
    (dolist (item items)
      (setq file-from (car item)
            reflinks (cdr item)
            empty nil)
      (dolist (reflink reflinks)
        (magit-insert-section (reflinks-file)
          (pcase-let ((`(_ ,source-file ,pos ,dest-title _ ,props) reflink))
            (magit-insert-heading dest-title)
            (magit-insert-section (reflink)
              (insert (org-roam-buffer--preview source-file pos) "\n"))))))
    empty))

(defvar org-roam-widget-reflinks
  (org-roam-widget :name 'reflinks
                   :header "Reflinks:"
                   :items #'org-roam-widget-get-reflinks
                   :render #'org-roam-widget-render-reflinks
                   :show-p #'org-roam-widget-show-reflinks-p)
  "Widget for reflinks.")

;;;; Unlinked References Widget
(defface org-roam-rowcol
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the rowcol part of the widgets."
  :group 'org-roam-faces)

(defvar org-roam-unlinked-reference-result-re
  (rx (group (one-or-more anything))
      ":"
      (group (one-or-more digit))
      ":"
      (group (one-or-more digit))
      ":"
      (group (zero-or-more anything)))
  "Regex for the return result of a ripgrep query.")

(defun org-roam-widget-show-unlinked-references-p (node)
  (and (executable-find "rg")
       (not (string-match "PCRE2 is not available" (shell-command-to-string "rg --pcre2-version")))))

(defun org-roam-widget-get-unlinked-references (node)
  ;; TODO
  (let* ((title (caar (org-roam-db-query [:select [title] :from nodes
                                          :where (= id $v1)
                                          :limit 1]
                                         (vector node))))
         (titles (list title))
         (rg-command (concat "rg -o --vimgrep -P -i "
                             (string-join (mapcar (lambda (glob) (concat "-g " glob))
                                                  (org-roam--list-files-search-globs org-roam-file-extensions)) " ")
                             (format " '\\[([^[]]++|(?R))*\\]%s' "
                                     (mapconcat (lambda (title)
                                                  (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                titles ""))
                             org-roam-directory)))
    (split-string (shell-command-to-string rg-command) "\n")))

(defun org-roam-widget-render-unlinked-references (lines)
  (let ((empty t)
        file row col match)
    (dolist (line lines)
      (save-match-data
        (when (string-match org-roam-unlinked-reference-result-re line)
          (setq file (match-string 1 line)
                row (match-string 2 line)
                col (match-string 3 line)
                match (match-string 4 line))
          (when (and match
                     ;; (member (downcase match) (mapcar #'downcase titles))
                     ;; (not (f-equal-p (expand-file-name file org-roam-directory)
                     ;;                 (buffer-file-name org-roam-buffer--current)))
                     )
            (magit-insert-section (unlinked-reference)
              (insert (propertize (format
                                   "%-8s"
                                   (format "%s:%s" row col))
                                  'font-lock-face 'org-roam-rowcol)
                      " "
                      file
                      "\n"))
            (setq empty nil)))))
    empty))

(defvar org-roam-widget-unlinked-references
  (org-roam-widget :name 'unlinked-refs
                   :header "Unlinked References:"
                   :items #'org-roam-widget-get-unlinked-references
                   :render #'org-roam-widget-render-unlinked-references
                   :show-p #'org-roam-widget-show-unlinked-references-p)
  "Widget for unlinked references.")

;;;
(defconst org-roam-widgets
  (list org-roam-widget-backlinks
        org-roam-widget-reflinks
        org-roam-widget-unlinked-references)
  "List of Org-roam widgets.")

;; Current Test Function
(defun org-roam-buffer ()
  (interactive)
  (let ((buffer (get-buffer-create
                 (concat "org-roam: "
                         (buffer-file-name))))
        (node (org-roam-current-node)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (magit-section-mode)
         (magit-insert-section (demo-buffer)
           (dolist (widget org-roam-widgets)
             (when (org-roam-widget-show widget node)
               (org-roam-widget-render widget node))))))
     (switch-to-buffer-other-window buffer)))

(provide 'org-roam-buffer)
;;; org-roam-buffer.el ends here

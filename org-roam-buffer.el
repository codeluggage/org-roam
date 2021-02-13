;;; org-roam-buffer.el --- Metadata buffer -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (magit-section "2.90.1"))

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
(defface org-roam-header-line
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for the `header-line' in some Org-roam modes."
  :group 'org-roam-faces)

(defface org-roam-title
  '((t :weight bold))
  "Face for Org-roam titles."
  :group 'org-roam-faces)

;; Buffer mode
(defvar org-roam-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map [C-return]  'org-roam-visit-thing)
    (define-key map (kbd "C-m") 'org-roam-visit-thing)
    map)
  "Parent keymap for all keymaps of modes derived from `org-roam-mode'.")

(define-derived-mode org-roam-mode magit-section-mode "Org-roam"
  "Major mode for Org-roam's buffer."
  :group 'org-roam
  (face-remap-add-relative 'header-line 'magit-header-line))

(defun org-roam-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

;; Custom sections
(defvar org-roam-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-visit-node)
    map)
  "Keymap for Org-roam node sections.")

(defvar org-roam-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-visit-file)
    map)
  "Keymap for Org-roam grep result sections.")

(defvar org-roam-olp-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-visit-olp)
    map)
  "Keymap for Org-roam grep result sections.")

(defclass org-roam-node (magit-section)
  ((keymap :initform org-roam-node-map)
   (node :initform nil)))

(defclass org-roam-grep (magit-section)
  ((keymap :initform org-roam-grep-map)
   (file :initform nil)
   (row :initform nil)
   (col :initform nil)))

(defclass org-roam-olp (magit-section)
  ((keymap :initform org-roam-olp-map)
   (file :initform nil)
   (olp :initform nil)))

(defun org-roam-buffer--preview (file point)
  "Get preview content for FILE at POINT."
  (org-roam--with-temp-buffer file
    (goto-char point)
    (let ((elem (org-element-at-point)))
      (or (org-element-property :raw-value elem)
          (when-let ((begin (org-element-property :begin elem))
                     (end (org-element-property :end elem)))
            (string-trim (buffer-substring-no-properties begin end)))))))

;;;; Unlinked References Widget
(defface org-roam-dim
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the dimmer part of the widgets."
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

;;;
(defvar org-roam-widgets
  (list #'org-roam-widget-backlinks
        #'org-roam-widget-reflinks
        #'org-roam-widget-unlinked-references)
  "List of Org-roam widgets. These are functions that render into
  magit sections.")

(cl-defun org-roam-widget-backlinks (&key node file)
  "Render backlinks for NODE."
  (when t                               ; TODO: whether to show backlinks
    (let* ((backlinks (org-roam-db-query
                       [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
                        :from links
                        :left :join nodes s :on (= links:source s:id)
                        :left :join nodes d :on (= links:dest d:id)
                        :where (= dest $s1)]
                       node))
           (backlinks (seq-group-by #'car backlinks))
           source values)
      (magit-insert-section (backlinks-section)
        (magit-insert-heading "Backlinks:")
        (dolist (backlink backlinks)
          (setq source (car backlink)
                values (cdr backlink))
          (pcase-dolist (`(,source ,source-file ,pos ,source-title ,dest ,dest-title ,props) values)
            (magit-insert-section section (org-roam-node)
              (magit-insert-heading (propertize source-title 'font-lock-face 'org-roam-title))
              (oset section node source)
              (let ((outline (plist-get props :outline)))
                (magit-insert-section section (org-roam-olp)
                  (oset section file source-file)
                  (oset section olp outline)
                  (magit-insert-heading (if outline
                                            (string-join (mapcar #'org-link-display-format outline)
                                                         " > ")
                                          "Top Level"))
                  (magit-insert-section (backlink-preview)
                    (insert (org-fontify-like-in-org-mode
                             (org-roam-buffer--preview source-file pos)) "\n")))))))))))

(cl-defun org-roam-widget-reflinks (&key node file)
  "Render ref links for NODE."
  ;; TODO
  (when nil                               ; TODO: whether to show reflinks
    (let* ((reflinks (org-roam-db-query
                      [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
                       :from links
                       :left :join nodes s :on (= links:source s:id)
                       :left :join nodes d :on (= links:dest d:id)
                       :where (= dest $s1)]
                      node))
           (reflinks (seq-group-by #'car reflinks))
           source values)
      (magit-insert-section section (org-roam-reflinks-section)
        (magit-insert-heading "Reflinks:")
        (dolist (reflink reflinks)
          (setq source (car reflink)
                values (cdr reflink))
          (magit-insert-section section (org-roam-node)
            (pcase-dolist (`(,source ,source-file ,pos ,source-title ,dest ,dest-title ,props) values)
              (magit-insert-heading (propertize source-title 'font-lock-face 'org-roam-title))
              (let ((outline (or (plist-get props :outline) '("Top"))))
                (oset section node source)
                (magit-insert-section (backlink-outline)
                  (magit-insert-heading (org-fontify-like-in-org-mode
                                         (-> outline
                                             (string-join " > "))))
                  (magit-insert-section (backlink-preview)
                    (insert (org-fontify-like-in-org-mode
                             (org-roam-buffer--preview source-file pos)) "\n")))))))))))

(cl-defun org-roam-widget-unlinked-references (&key node file)
  "Render unlinked references for NODE."
  (when (and (executable-find "rg")
             (not (string-match "PCRE2 is not available" (shell-command-to-string "rg --pcre2-version"))))
    (let* ((title (caar (org-roam-db-query [:select [title] :from nodes
                                            :where (= id $s1)
                                            :limit 1]
                                           node)))
           (aliases (mapcar #'car
                            (org-roam-db-query [:select [alias] :from aliases
                                                :where (= node_id $s1)]
                                               node)))
           (titles (cons title aliases))
           (rg-command (concat "rg -o --vimgrep -P -i "
                               (string-join (mapcar (lambda (glob) (concat "-g " glob))
                                                    (org-roam--list-files-search-globs org-roam-file-extensions)) " ")
                               (format " '\\[([^[]]++|(?R))*\\]%s' "
                                       (mapconcat (lambda (title)
                                                    (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                  titles ""))
                               org-roam-directory))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           f row col match)
      (magit-insert-section (unlinked-references)
        (magit-insert-heading "Unlinked References:")
        (dolist (line results)
          (save-match-data
            (when (string-match org-roam-unlinked-reference-result-re line)
              (setq f (match-string 1 line)
                    row (string-to-number (match-string 2 line))
                    col (string-to-number (match-string 3 line))
                    match (match-string 4 line))
              (when (and match
                         (not (f-equal-p file f))
                         (member (downcase match) (mapcar #'downcase titles)))
                (magit-insert-section section (org-roam-grep)
                  (oset section file f)
                  (oset section row row)
                  (oset section col col)
                  (insert (propertize (format "%s:%s:%s"
                                              (truncate-string-to-width (file-name-base f) 15 nil nil "...")
                                              row col) 'font-lock-face 'org-roam-dim)
                          " "
                          (org-fontify-like-in-org-mode (org-roam-buffer-line-preview f row))
                          "\n"))))))))))

(defun org-roam-buffer-line-preview (file row)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (forward-line (1- row))
    (buffer-substring-no-properties (save-excursion
                                      (beginning-of-line)
                                      (point))
                                    (save-excursion
                                      (end-of-line)
                                      (point)))))

;; current Test Function
(defun org-roam-buffer ()
  (interactive)
  (unless (org-roam--org-file-p (buffer-file-name (buffer-base-buffer)))
    (user-error "Not in Org-roam file"))
  (let ((file (buffer-file-name))
        (buffer (get-buffer-create
                 (concat "org-roam: "
                         (buffer-file-name))))
        (node (org-roam-current-node)))
    (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (org-roam-mode)
         (org-roam-set-header-line-format
          (org-roam-node-title node))
         (magit-insert-section (demo-buffer)
           (magit-insert-heading)
           (dolist (widget org-roam-widgets)
             (funcall widget :node node :file file)))))
    (switch-to-buffer-other-window buffer)))

(provide 'org-roam-buffer)
;;; org-roam-buffer.el ends here

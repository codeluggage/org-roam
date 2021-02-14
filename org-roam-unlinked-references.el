;;; org-roam-unlinked-references.el --- create and refresh Org-roam buffers -*- lexical-binding: t -*-
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
;; This library provides functionality dealing with unlinked references.
;;
;;; Code:
;;;; Library Requires
(require 'eieio)
(require 'magit-section)
(require 'org-roam-mode)

(defvar org-roam-mode-sections)

;;; Section
;;;; Faces

;;;; Definition
(defvar org-roam-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-visit-file)
    map)
  "Keymap for Org-roam grep result sections.")

(defclass org-roam-grep (magit-section)
  ((keymap :initform org-roam-grep-map)
   (file :initform nil)
   (row :initform nil)
   (col :initform nil)))

;;; Functions
(defvar org-roam-unlinked-references-result-re
  (rx (group (one-or-more anything))
      ":"
      (group (one-or-more digit))
      ":"
      (group (one-or-more digit))
      ":"
      (group (zero-or-more anything)))
  "Regex for the return result of a ripgrep query.")

;;; Section inserter
(defun org-roam-unlinked-references-preview-line (file row)
  "Return the preview line from FILE.
This is the ROW within FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (forward-line (1- row))
    (buffer-substring-no-properties
     (save-excursion
       (beginning-of-line)
       (point))
     (save-excursion
       (end-of-line)
       (point)))))

(cl-defun org-roam-unlinked-references-insert-section (&key node file)
  "Render unlinked references for NODE.
References from FILE are excluded."
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
                                                    (org-roam--list-files-search-globs
                                                     org-roam-file-extensions)) " ")
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
            (when (string-match org-roam-unlinked-references-result-re line)
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
                          (org-fontify-like-in-org-mode (org-roam-unlinked-references-preview-line f row))
                          "\n"))))))
        (insert ?\n)))))

(provide 'org-roam-unlinked-references)
;;; org-roam-unlinked-references.el ends here

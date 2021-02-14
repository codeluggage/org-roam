;;; org-roam-node.el --- create and refresh Org-roam buffers -*- lexical-binding: t -*-
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
;; This library provides functionality dealing with nodes.
;;
;;; Code:
;;;; Library Requires
(require 'eieio)
(require 'magit-section)
(require 'org-roam-mode)

(defvar org-roam-mode-sections)

;;; Section
;;;; Definition
(defvar org-roam-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-node-visit)
    map)
  "Keymap for Org-roam node sections.")

(defclass org-roam-node (magit-section)
  ((keymap :initform org-roam-node-map)
   (node :initform nil)))

;; TODO: move to own files
(defvar org-roam-olp-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-visit-olp)
    map)
  "Keymap for Org-roam grep result sections.")

(defvar org-roam-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-visit-preview)
    map)
  "Keymap for Org-roam preview.")

(defclass org-roam-olp (magit-section)
  ((keymap :initform org-roam-olp-map)
   (file :initform nil)
   (olp :initform nil)
   (heading-highlight-face :initform org-roam-preview-heading-highlight)))

(defclass org-roam-preview (magit-section)
  ((keymap :initform org-roam-preview-map)
   (file :initform nil)
   (begin :initform nil)
   (end :initform nil)))

;;; Functions
(defun org-roam-node-backlinks (node)
  "Return the backlinks for NODE."
  (org-roam-db-query
   [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
    :from links
    :left :join nodes s :on (= links:source s:id)
    :left :join nodes d :on (= links:dest d:id)
    :where (= dest $s1)]
   node))

(defun org-roam-node-title (node)
  "Return the title for a given NODE."
  (caar (org-roam-db-query [:select title :from nodes
                            :where (= id $s1)]
                           node)))

(defun org-roam-node-preview (file point)
  "Get preview content for FILE at POINT."
  (org-roam--with-temp-buffer file
    (goto-char point)
    (let ((elem (org-element-at-point)))
      (or (org-element-property :raw-value elem)
          (when-let ((begin (org-element-property :begin elem))
                     (end (org-element-property :end elem)))
            (list begin end
                  (string-trim (buffer-substring-no-properties begin end))))))))

;;; Section inserter
;;;; TODO: Move to own backlinks widget
(cl-defun org-roam-node-insert-section (&key node _file)
  "Insert section for NODE."
  (when t                               ; TODO: whether to show backlinks
    (let* ((backlinks (seq-group-by #'car (org-roam-node-backlinks node)))
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
                  (insert (propertize
                           (concat
                            (if outline
                                (string-join (mapcar #'org-link-display-format outline)
                                             " > ")
                              "Top Level")
                            "\n")
                           'font-lock-face 'org-roam-preview-heading))
                  (magit-insert-heading)
                  (oset section file source-file)
                  (oset section olp outline)
                  (magit-insert-section section (org-roam-preview)
                    (pcase-let ((`(,begin ,end ,s) (org-roam-node-preview source-file pos)))
                      (insert (org-fontify-like-in-org-mode s) "\n")
                      (oset section file source-file)
                      (oset section begin begin)
                      (oset section end end))))))))
        (insert ?\n)))))

(provide 'org-roam-node)
;;; org-roam-node.el ends here

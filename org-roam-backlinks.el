;;; org-roam-backlinks.el --- The backlinks section -*- lexical-binding: t -*-
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
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
(require 'magit-section)

(defvar org-roam-mode-sections)
(defvar org-roam-mode-map)

;;; Section
;;;; Definition

;;; Functions
(defun org-roam-backlinks-get (node)
  "Return the backlinks for NODE."
  (org-roam-db-query
   [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
    :from links
    :left :join nodes s :on (= links:source s:id)
    :left :join nodes d :on (= links:dest d:id)
    :where (= dest $s1)]
   node))

;;; Section inserter
(cl-defun org-roam-backlinks-insert-section (&key node _file)
  "Insert backlinks section for NODE."
  (let* ((backlinks (seq-group-by #'car (org-roam-backlinks-get node)))
         values)
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (backlink backlinks)
        (setq values (cdr backlink))
        (pcase-dolist (`(,source ,source-file ,pos ,source-title ,dest ,dest-title ,props) values)
          (org-roam-node-insert-section :source source
                                        :source-file source-file
                                        :pos pos
                                        :source-title source-title
                                        :dest dest
                                        :dest-title dest-title
                                        :props props)))
      (insert ?\n))))

(provide 'org-roam-backlinks)
;;; org-roam-backlinks.el ends here

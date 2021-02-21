;;; org-roam-node.el --- create and refresh Org-roam buffers -*- lexical-binding: t -*-
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
(require 'org-roam-structs)

(defvar org-roam-mode-sections)
(defvar org-roam-mode-map)
;;; Section
;;;; Definition
(defvar org-roam-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-node-visit)
    map)
  "Keymap for Org-roam node sections.")

(defclass org-roam-node-section (magit-section)
  ((keymap :initform org-roam-node-map)
   (node :initform nil)))

;; TODO: move to own files
(defvar org-roam-olp-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-olp-visit)
    map)
  "Keymap for Org-roam grep result sections.")

(defvar org-roam-preview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-mode-map)
    (define-key map [remap org-roam-visit-thing] 'org-roam-preview-visit)
    map)
  "Keymap for Org-roam preview.")

(defclass org-roam-olp-section (magit-section)
  ((keymap :initform org-roam-olp-map)
   (file :initform nil)
   (olp :initform nil)
   (heading-highlight-face :initform org-roam-preview-heading-highlight)))

(defclass org-roam-preview-section (magit-section)
  ((keymap :initform org-roam-preview-map)
   (file :initform nil)
   (begin :initform nil)
   (end :initform nil)))

;;; Functions
(cl-defmethod org-roam-populate ((node org-roam-node))
  "Populate NODE from database."
  (let ((node-info (car (org-roam-db-query [:select [file level pos todo priority scheduled deadline title]
                                            :from nodes
                                            :where (= id $s1)
                                            :limit 1]
                                           (org-roam-node-id node))))
        (tag-info (mapcar #'car (org-roam-db-query [:select [tag] :from tags
                                                    :where (= node-id $s1)]
                                                   (org-roam-node-id node))))
        (alias-info (mapcar #'car (org-roam-db-query [:select [alias] :from aliases
                                                      :where (= node-id $s1)]
                                                     (org-roam-node-id node))))
        (refs-info (mapcar #'car (org-roam-db-query [:select [ref] :from refs
                                                     :where (= node-id $s1)]
                                                    (org-roam-node-id node)))))
    (pcase-let ((`(,file ,level ,pos ,todo ,priority ,scheduled ,deadline ,title) node-info))
      (setf (org-roam-node-file node) file
            (org-roam-node-level node) level
            (org-roam-node-point node) pos
            (org-roam-node-todo node) todo
            (org-roam-node-priority node) priority
            (org-roam-node-scheduled node) scheduled
            (org-roam-node-deadline node) deadline
            (org-roam-node-title node) title
            (org-roam-node-tags node) tag-info
            (org-roam-node-refs node) refs-info
            (org-roam-node-aliases node) alias-info))
    node))

(defun org-roam-node-backlinks (node)
  "Return the backlinks for NODE."
  (org-roam-db-query
   [:select [links:source links:file links:pos s:title links:dest d:title links:properties]
    :from links
    :left :join nodes s :on (= links:source s:id)
    :left :join nodes d :on (= links:dest d:id)
    :where (= dest $s1)]
   node))

(defun org-roam-node-preview (file point)
  "Get preview content for FILE at POINT."
  (org-roam-with-temp-buffer file
    (goto-char point)
    (let ((elem (org-element-at-point)))
      (or (org-element-property :raw-value elem)
          (when-let ((begin (org-element-property :begin elem))
                     (end (org-element-property :end elem)))
            (list begin end
                  (string-trim (buffer-substring-no-properties begin end))))))))

(defun org-roam-node-at-point (&optional assert)
  "Return the node at point.
If ASSERT, throw an error."
  (if-let ((node-id (magit-section-case
                      (org-roam-node-section (oref it node))
                      (t (let (id)
                           (org-with-wide-buffer
                            (while (and (not (setq id (org-id-get)))
                                        (not (bobp)))
                              (org-up-heading-or-point-min))
                            id))))))
      (org-roam-populate (org-roam-node-create :id node-id))
    (when assert
      (user-error "No node at point"))))

(defun org-roam-node--find (node)
  "Navigate to the point for NODE, and return the buffer."
  (unless (org-roam-node-file node)
    (user-error "Node does not have corresponding file."))
  (let ((buf (find-file-noselect (org-roam-node-file node))))
    (with-current-buffer buf
      (goto-char (org-roam-node-point node)))
    buf))

(defun org-roam-node-visit (node &optional other-window)
  "From the buffer, visit NODE.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead."
  (interactive (list (org-roam-node-at-point t) current-prefix-arg))
  (let ((buf (org-roam-node--find node)))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defun org-roam-node--completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is a plist containing the properties of the node."
  (let* ((rows (org-roam-db-query [:select [nodes:file nodes:id nodes:title nodes:pos]
                                   :from nodes]))
         (alias-rows (org-roam-db-query [:select [nodes:file nodes:id aliases:alias nodes:pos] :from aliases
                                         :left :join nodes :on (= aliases:node_id nodes:id)]))
         (tag-table (org-roam--tags-table))
         completions)
    (dolist (row rows)
      (pcase-let ((`(,file-path ,id ,title ,pos) row))
        (let* ((tags (gethash id tag-table))
               (s (propertize title 'meta (list :file file-path :title title :point pos :id id :tags tags)))
               (node (org-roam-node-create :id id
                                           :file file-path
                                           :title title
                                           :point pos
                                           :tags tags)))
          (push (cons s node) completions))))
    (dolist (row alias-rows completions)
      (pcase-let ((`(,file-path ,id ,alias ,pos) row))
        (let* ((tags (gethash id tag-table))
               (s (propertize alias 'meta (list :file file-path :title alias :point pos :id id :tags tags)))
               (node (org-roam-node-create :id id
                                           :file file-path
                                           :title alias
                                           :point pos
                                           :tags tags)))
          (push (cons s node) completions))))))

(defun org-roam-node-read (&optional initial-input filter-fn)
  "Read and return an `org-roam-node'.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function applied to the completion list."
  (let* ((nodes (org-roam-node--completions))
         (nodes (funcall (or filter-fn #'identity) nodes))
         (node (completing-read "Node: "
                                (lambda (string pred action)
                                  (if (eq action 'metadata)
                                      '(metadata
                                        (annotation-function . org-roam-node--annotation)
                                        (category . org-roam-node))
                                    (complete-with-action action nodes string pred)))
                                nil nil initial-input)))
    (or (cdr (assoc node nodes))
        (org-roam-node-create :title node))))

(defun org-roam-node--annotation (node-title)
  "Return the annotation string for a NODE-TITLE."
  (let* ((meta (get-text-property 0 'meta node-title))
         (tags (plist-get meta :tags)))
    (when tags
      (format " (%s)" (string-join tags ", ")))))

;; TODO: move to own file
(defun org-roam-olp-at-point (&optional assert)
  "Return the olp at point.
If ASSERT, throw an error."
  (magit-section-case
    (org-roam-olp-section (oref it olp))
    (t (when assert
         (user-error "No olp at point")))))

(defun org-roam-olp-visit (file olp &optional other-window)
  "Visit OLP in FILE.
With prefix argument OTHER-WINDOW, visit the olp in another
window instead."
  (interactive (list (org-roam-file-at-point t)
                     (org-roam-olp-at-point t)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (when olp
        (condition-case err
            (let ((m (org-find-olp olp t)))
              (goto-char (org-find-olp olp t))
              (set-marker m nil))
          (error
           (error "Could not find OLP")))))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

(defun org-roam-preview-visit (file point &optional other-window)
  "Visit FILE at POINT.
With prefix argument OTHER-WINDOW, visit the olp in another
window instead."
  (interactive (list (org-roam-file-at-point t)
                     (oref (magit-current-section) begin)
                     current-prefix-arg))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (goto-char point))
    (funcall (if other-window
                 #'switch-to-buffer-other-window
               #'pop-to-buffer-same-window) buf)))

;;; Section inserter
(cl-defun org-roam-node-insert-section (&key source source-file source-title pos _dest _dest-title props)
  "Insert section for NODE.
SOURCE is the node id.
SOURCE-FILE is the file for the node.
SOURCE-TITLE is the title of the source node.
POS is the position in the file for the link to node.
PROPS contains additional properties about the link."
  (magit-insert-section section (org-roam-node-section)
    (magit-insert-heading (propertize source-title 'font-lock-face 'org-roam-title))
    (oset section node source)
    (let ((outline (plist-get props :outline)))
      (magit-insert-section section (org-roam-olp-section)
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
        (magit-insert-section section (org-roam-preview-section)
          (pcase-let ((`(,begin ,end ,s) (org-roam-node-preview source-file pos)))
            (insert (org-fontify-like-in-org-mode s) "\n")
            (oset section file source-file)
            (oset section begin begin)
            (oset section end end)))))))

;;;Interactives
;;;###autoload
(defun org-roam-node-find (&optional other-window initial-input filter-fn)
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (let ((org-roam-capture--info `((title . (org-roam-node-title node))
                                      (slug  . ,(funcall org-roam-title-to-slug-function (org-roam-node-title node)))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))

(defun org-roam-node-insert (&optional filter-fn)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-title node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (let ((org-roam-capture--info
                   `((title . ,(org-roam-node-title node))
                     (slug . ,(funcall org-roam-title-to-slug-function (org-roam-node-title node)))))
                  (org-roam-capture--context 'title))
              (setq org-roam-capture-additional-template-props
                    (list :region (when (and beg end)
                                    (cons beg end))
                          :insert-at (point-marker)
                          :link-description description
                          :finalize 'insert-link))
              (org-roam-capture--capture)))))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-node-random (&optional other-window)
  "Find a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead."
  (interactive current-prefix-arg)
  (let ((random-id (seq-random-elt
                    (mapcar #'car (org-roam-db-query [:select [id] :from nodes])))))
    (org-roam-node-visit (org-roam-populate
                          (org-roam-node-create :id random-id)))))


(provide 'org-roam-node)
;;; org-roam-node.el ends here

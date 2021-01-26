;;; org-roam-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library is provides the underlying database api to org-roam
;;
;;; Code:
;;;; Library Requires
(eval-when-compile (require 'subr-x))
(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'seq)

(eval-and-compile
  (require 'org-roam-macs)
  ;; For `org-with-wide-buffer'
  (require 'org-macs))

(defvar org-roam-directory)
(defvar org-roam-verbose)
(defvar org-roam-file-name)

(defvar org-agenda-files)

(declare-function org-roam--org-roam-file-p                "org-roam")
(declare-function org-roam--extract-titles                 "org-roam")
(declare-function org-roam--extract-refs                   "org-roam")
(declare-function org-roam--extract-tags                   "org-roam")
(declare-function org-roam--extract-links                  "org-roam")
(declare-function org-roam--list-all-files                 "org-roam")
(declare-function org-roam--path-to-slug                   "org-roam")
(declare-function org-roam--file-name-extension            "org-roam")

;;;; Options
(defcustom org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
  "The full path to file where the Org-roam database is stored.
If this is non-nil, the Org-roam sqlite database is saved here.

It is the user's responsibility to set this correctly, especially
when used with multiple Org-roam instances."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-db-gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During large, heavy operations like `org-roam-db-build-cache',
many GC operations happen because of the large number of
temporary structures generated (e.g. parsed ASTs). Temporarily
increasing `gc-cons-threshold' will help reduce the number of GC
operations, at the cost of temporary memory usage.

This defaults to the original value of `gc-cons-threshold', but
tweaking this number may lead to better overall performance. For
example, to reduce the number of GCs, one may set it to a large
value like `most-positive-fixnum'."
  :type 'int
  :group 'org-roam)

(defconst org-roam-db--version 12)

(defvar org-roam-db--connection (make-hash-table :test #'equal)
  "Database connection to Org-roam database.")

;;;; Core Functions

(defun org-roam-db--get-connection ()
  "Return the database connection, if any."
  (gethash (expand-file-name org-roam-directory)
           org-roam-db--connection))

(defun org-roam-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (org-roam-db--get-connection)
               (emacsql-live-p (org-roam-db--get-connection)))
    (let ((init-db (not (file-exists-p org-roam-db-location))))
      (make-directory (file-name-directory org-roam-db-location) t)
      (let ((conn (emacsql-sqlite3 org-roam-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (expand-file-name org-roam-directory)
                 conn
                 org-roam-db--connection)
        (when init-db
          (org-roam-db--init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (org-roam-db--upgrade-maybe conn version)))
          (cond
           ((> version org-roam-db--version)
            (emacsql-close conn)
            (user-error
             "The Org-roam database was created with a newer Org-roam version.  "
             "You need to update the Org-roam package"))
           ((< version org-roam-db--version)
            (emacsql-close conn)
            (error "BUG: The Org-roam database scheme changed %s"
                   "and there is no upgrade path")))))))
  (org-roam-db--get-connection))

;;;; Entrypoint: (org-roam-db-query)
(defun org-roam-db-query (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if  (stringp sql)
      (emacsql (org-roam-db) (apply #'format sql args))
    (apply #'emacsql (org-roam-db) sql args)))

;;;; Schemata
(defconst org-roam-db--table-schemata
  '((files
     [(file :unique :primary-key)
      (hash :not-null)
      atime
      mtime])

    (nodes
     [(id :primary-key :not-null)
      (file :not-null)
      (level :not-null)
      (pos :not-null)
      tags
      title
      ref])

    (links
     [(file :not-null)
      (pos :not-null)
      (source :not-null)
      (dest :not-null)
      (type :not-null)
      (properties :not-null)])))

(defun org-roam-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) org-roam-db--table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" org-roam-db--version))))

(defun org-roam-db--upgrade-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version org-roam-db--version)
        (progn
          (org-roam-message (format "Upgrading the Org-roam database from version %d to version %d"
                                    version org-roam-db--version))
          (org-roam-db-build-cache t))))
  version)

(defun org-roam-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-roam-directory'."
  (unless db
    (setq db (org-roam-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun org-roam-db--close-all ()
  "Closes all database connections made by Org-roam."
  (dolist (conn (hash-table-values org-roam-db--connection))
    (org-roam-db--close conn)))

;;;; Database API
;;;;; Initialization
(defun org-roam-db--initialized-p ()
  "Whether the Org-roam cache has been initialized."
  (and (file-exists-p org-roam-db-location)
       (> (caar (org-roam-db-query [:select (funcall count) :from titles]))
          0)))

;;;;; Clearing
(defun org-roam-db-clear ()
  "Clears all entries in the Org-roam cache."
  (interactive)
  (when (file-exists-p org-roam-db-location)
    (dolist (table (mapcar #'car org-roam-db--table-schemata))
      (org-roam-db-query `[:delete :from ,table]))))

(defun org-roam-db-clear-file (&optional file)
  "Remove any related links to the FILE.
This is equivalent to removing the node from the graph.
If FILE is nil, clear the current buffer."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (dolist (table (mapcar #'car org-roam-db--table-schemata))
    (org-roam-db-query `[:delete :from ,table
                         :where (= ,(if (eq table 'links) 'source 'file) $s1)]
                       file)))

;;;;; Inserting
(defun org-roam-db-insert-file (&optional update-p)
  "Update the files table for the current buffer.
If UPDATE-P is non-nil, first remove the file in the database."
  (let* ((file (or org-roam-file-name (buffer-file-name)))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (org-roam-db--file-hash)))
    (when update-p
      (org-roam-db-query [:delete :from files
                          :where (= file $s1)]
                         file))
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file hash atime mtime)))))

(defun org-roam-db-insert-nodes (&optional update-p)
  (let ((file (or org-roam-file-name
                  (buffer-file-name (buffer-base-buffer))))
        nodes
        id level pos tags title ref)
    (when update-p
      (org-roam-db-query [:delete :from nodes
                          :where (= file $s1)]
                         file))
    (org-with-point-at 1
      ;; First, we get the file-level ID
      (when (setq id (org-id-get-create))
        (setq title (cadr (assoc "TITLE" (org-collect-keywords '("title"))
                                 #'string-equal))
              ;; TODO handle tags
              tags nil
              pos (point)
              level (org-outline-level)
              ;; TODO handle ref
              ref nil)
        (push (vector id file level pos tags title ref) nodes))
      ;; Then we loop over all headlines
      (org-map-entries
       (lambda ()
         (when (setq id (org-id-get))
           (setq title (nth 4 (org-heading-components))
              ;; TODO handle tags
              tags nil
              pos (point)
              level (org-outline-level)
              ;; TODO handle ref
              ref nil)
           (push (vector id file level pos tags title ref) nodes)))))
    (when nodes
      (org-roam-db-query
       [:insert :into nodes
        :values $v1]
       nodes))))

(defun org-roam-db-insert-links (&optional update-p)
  (let ((file (or org-roam-file-name
                  (buffer-file-name (buffer-base-buffer))))
        links source dest type poperties)
    (when update-p
      (org-roam-db-query [:delete :from links
                          :where (= file $s1)]
                         file))
    (org-roam-with-file file nil
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (goto-char (org-element-property :begin link))
          (setq type (org-element-property :type link)
                dest (org-element-property :path link)
                properties (list :outline (org-roam--get-outline-path)))
          (save-excursion
            (while (not (setq source (org-id-get)))
              (org-up-heading-or-point-min)))
          (push (vector file (point) source dest type properties) links))))
    (when links
      (org-roam-db-query
       [:insert :into links
        :values $v1]
       links))))

;;;;; Fetching
(defun org-roam-db-has-file-p (file)
  "Return t if FILE is in the database, nil otherwise."
  (> (caar (org-roam-db-query [:select (funcall count) :from files
                              :where (= file $s1)]
                              file))
     0))

(defun org-roam-db--get-current-files ()
  "Return a hash-table of file to the hash of its file contents."
  (let ((current-files (org-roam-db-query [:select [file hash] :from files]))
        (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-roam-db--get-title (file)
  "Return the main title of FILE from the cache."
  (caar (org-roam-db-query [:select [title] :from titles
                            :where (= file $s1)
                            :limit 1]
                           file)))

(defun org-roam-db--get-tags ()
  "Return all distinct tags from the cache."
  (let ((rows (org-roam-db-query [:select :distinct [tags] :from tags]))
        acc)
    (dolist (row rows)
      (dolist (tag (car row))
        (unless (member tag acc)
          (push tag acc))))
    acc))

(defun org-roam-db--connected-component (file)
  "Return all files reachable from/connected to FILE, including the file itself.
If the file does not have any connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (WITH filelinks AS (SELECT * FROM links WHERE NOT \"type\" = '\"cite\"'),
                           citelinks AS (SELECT * FROM links
                                                  JOIN refs ON links.\"dest\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"source\", \"dest\" FROM filelinks UNION
                      SELECT \"dest\", \"source\" FROM filelinks UNION
                      SELECT \"file\", \"source\" FROM citelinks UNION
                      SELECT \"dest\", \"file\" FROM citelinks),
                   connected_component(file) AS
                     (SELECT link FROM links_of WHERE file = $s1
                      UNION
                      SELECT link FROM links_of JOIN connected_component USING(file))
                   SELECT * FROM connected_component;")
         (files (mapcar 'car-safe (emacsql (org-roam-db) query file))))
    files))

(defun org-roam-db--links-with-max-distance (file max-distance)
  "Return all files connected to FILE in at most MAX-DISTANCE steps.
This includes the file itself. If the file does not have any
connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (WITH filelinks AS (SELECT * FROM links WHERE NOT \"type\" = '\"cite\"'),
                           citelinks AS (SELECT * FROM links
                                                  JOIN refs ON links.\"dest\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"source\", \"dest\" FROM filelinks UNION
                      SELECT \"dest\", \"source\" FROM filelinks UNION
                      SELECT \"file\", \"source\" FROM citelinks UNION
                      SELECT \"source\", \"file\" FROM citelinks),
                   -- Links are traversed in a breadth-first search.  In order to calculate the
                   -- distance of nodes and to avoid following cyclic links, the visited nodes
                   -- are tracked in 'trace'.
                   connected_component(file, trace) AS
                     (VALUES($s1, json_array($s1))
                      UNION
                      SELECT lo.link, json_insert(cc.trace, '$[' || json_array_length(cc.trace) || ']', lo.link) FROM
                      connected_component AS cc JOIN links_of AS lo USING(file)
                      WHERE (
                        -- Avoid cycles by only visiting each file once.
                        (SELECT count(*) FROM json_each(cc.trace) WHERE json_each.value == lo.link) == 0
                        -- Note: BFS is cut off early here.
                        AND json_array_length(cc.trace) < ($s2 + 1)))
                   SELECT DISTINCT file, min(json_array_length(trace)) AS distance
                   FROM connected_component GROUP BY file ORDER BY distance;")
         ;; In principle the distance would be available in the second column.
         (files (mapcar 'car-safe (emacsql (org-roam-db) query file max-distance))))
    files))

(defun org-roam-db--file-hash (&optional file-path)
  "Compute the hash of FILE-PATH, a file or current buffer."
  (if file-path
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file-path)
        (secure-hash 'sha1 (current-buffer)))
    (org-with-wide-buffer
     (secure-hash 'sha1 (current-buffer)))))

;;;;; Updating
(defun org-roam-db-build-cache (&optional force)
  "Build the cache for `org-roam-directory'.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file org-roam-db-location))
  (org-roam-db--close) ;; Force a reconnect
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-files (org-roam--list-all-files))
         (current-files (org-roam-db--get-current-files))
         (modified-files nil))
    (dolist (file org-roam-files)
      (let ((contents-hash (org-roam-db--file-hash file)))
        (unless (string= (gethash file current-files)
                         contents-hash)
          (push file modified-files)))
      (remhash file current-files))
    ;; These files are no longer around, remove from cache...
    (dolist (file (hash-table-keys current-files))
      (org-roam-db-clear-file file))
    (dolist (file modified-files)
      (org-roam-db-update-file file))))

(defun org-roam-db-update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                              :where (= file $s1)] file-path))))
    (unless (string= content-hash db-hash)
      (org-roam-with-file file-path nil
        (org-roam-db-clear-file 'update)
        (org-roam-db-insert-file 'update)
        (org-roam-db-insert-nodes 'update)
        (org-roam-db-insert-links 'update)))))

(provide 'org-roam-db)

;;; org-roam-db.el ends here

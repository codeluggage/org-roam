;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Dependencies
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 's)
(require 'seq)
(eval-when-compile (require 'subr-x))

;;;; Features
(require 'org-roam-compat)
(require 'org-roam-macs)
;; These features should be able to be loaded order independently.
;; @TODO: implement something akin to `org-modules' that allows
;; selectively loading different sets of features.
;; ~NV [2020-05-22 Fri]

(require 'org-roam-faces)
(require 'org-roam-mode)
(require 'org-roam-completion)
(require 'org-roam-capture)
(require 'org-roam-dailies)
(require 'org-roam-db)
(require 'org-roam-doctor)
(require 'org-roam-graph)
(require 'org-roam-link)

;;;; Declarations
;; From org-ref-core.el
(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
;; From org-id.el
(defvar org-id-link-to-org-use-id)
(declare-function org-id-find-id-in-file "ext:org-id" (id file &optional markerp))

;;;; Customizable variables
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual.html"))

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.
All Org files, at any level of nesting, are considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .gpg extension."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-file-extensions '("org")
  "Detected file extensions to include in the Org-roam ecosystem.
The first item in the list is used as the default file extension.
While the file extensions may be different, the file format needs
to be an `org-mode' file, and it is the user's responsibility to
ensure that."
  :type '(repeat string)
  :group 'org-roam)

(defcustom org-roam-file-exclude-regexp nil
  "Files matching this regular expression are excluded from the Org-roam."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'org-roam)

(defcustom org-roam-include-type-in-ref-path-completions nil
  "When t, include the type in ref-path completions.
Note that this only affects interactive calls.
See `org-roam--get-ref-path-completions' for details."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-index-file "index.org"
  "Path to the Org-roam index file.
The path can be a string or a function.  If it is a string, it
should be the path (absolute or relative to `org-roam-directory')
to the index file.  If it is is a function, the function should
return the path to the index file.  Otherwise, the index is
assumed to be a note in `org-roam-directory' whose title is
'Index'."
  :type '(choice
          (string :tag "Path to index" "%s")
          (function :tag "Function to generate the path"))
  :group 'org-roam)

(defcustom org-roam-link-title-format "%s"
  "The formatter used when inserting Org-roam links that use their title.
Formatter may be a function that takes title as its only argument."
  :type '(choice
          (string :tag "String Format" "%s")
          (function :tag "Custom function"))
  :group 'org-roam)

(defcustom org-roam-list-files-commands
  (if (member system-type '(windows-nt ms-dos cygwin))
      nil
    '(find rg))
  "Commands that will be used to find Org-roam files.

It should be a list of symbols or cons cells representing any of the following
 supported file search methods.

The commands will be tried in order until an executable for a command is found.
The Elisp implementation is used if no command in the list is found.

  `rg'
    Use ripgrep as the file search method.
    Example command: rg /path/to/dir/ --files -g \"*.org\" -g \"*.org.gpg\"

  `find'
    Use find as the file search method.
    Example command:
    find /path/to/dir -type f \( -name \"*.org\" -o -name \"*.org.gpg\" \)

By default, `executable-find' will be used to look up the path to the
executable. If a custom path is required, it can be specified together with the
method symbol as a cons cell. For example: '(find (rg . \"/path/to/rg\"))."
  :type '(set (const :tag "find" find)
              (const :tag "rg" rg)))

(defcustom org-roam-tag-separator ","
  "String to use to separate tags when `org-roam-tag-sources' is non-nil."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-tag-sort nil
  "When non-nil, sort the tags in the completions.
When t, sort the tags alphabetically, regardless of case.
`org-roam-tag-sort' can also be a list of arguments to be applied
to `cl-sort'.  For example, these are the arguments used when
`org-roam-tag-sort' is set to t:
    \('string-lessp :key 'downcase)
Only relevant when `org-roam-tag-sources' is non-nil."
  :type '(choice
          (boolean)
          (list :tag "Arguments to cl-loop"))
  :group 'org-roam)

(defcustom org-roam-title-to-slug-function #'org-roam--title-to-slug
  "Function to be used in converting a title to the filename slug.
Function should return a filename string based on title."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-file-completion-tag-position 'prepend
  "Prepend, append, or omit tags from the file titles during completion."
  :type '(choice (const :tag "Prepend" prepend)
                 (const :tag "Append" append)
                 (const :tag "Omit" omit))
  :group 'org-roam)

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

(defvar org-roam-completion-functions nil
  "List of functions to be used with `completion-at-point' for Org-roam.")

;;;; Dynamic variables
(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

;;;; Utilities
(defun org-roam--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (intern (substring (symbol-name (pop plist)) 1 nil)))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

(defun org-roam-current-node ()
  "Return the current node at point, or nil if not a node."
  (let (id)
    (org-with-wide-buffer
     (while (and (not (setq id (org-id-get)))
                 (not (bobp)))
       (org-up-heading-or-point-min))
     id)))

(defun org-roam--url-p (path)
  "Check if PATH is a URL.
Assume the protocol is not present in PATH; e.g. URL `https://google.com' is
passed as `//google.com'."
  (string-prefix-p "//" path))

;;;; File functions and predicates
(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam--org-file-p (path)
  "Check if PATH is pointing to an org file."
  (let ((ext (org-roam--file-name-extension path)))
    (when (string= ext "gpg")           ; Handle encrypted files
      (setq ext (org-roam--file-name-extension (file-name-sans-extension path))))
    (member ext org-roam-file-extensions)))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let ((path (or file
                       (-> (buffer-base-buffer)
                           (buffer-file-name)))))
    (save-match-data
      (and
       (org-roam--org-file-p path)
       (not (and org-roam-file-exclude-regexp
                 (string-match-p org-roam-file-exclude-regexp path)))
       (f-descendant-of-p path (expand-file-name org-roam-directory))))))

(defun org-roam--shell-command-files (cmd)
  "Run CMD in the shell and return a list of files. If no files are found, an empty list is returned."
  (--> cmd
    (shell-command-to-string it)
    (ansi-color-filter-apply it)
    (split-string it "\n")
    (seq-filter #'s-present? it)))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (append
   (mapcar (lambda (ext) (s-wrap (concat "*." ext) "\"")) exts)
   (mapcar (lambda (ext) (s-wrap (concat "*." ext ".gpg") "\"")) exts)))

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command (s-join " " `(,executable "-L" ,dir "--files"
                                            ,@(mapcar (lambda (glob) (concat "-g " glob)) globs)))))
    (org-roam--shell-command-files command)))

(defun org-roam--list-files-find (executable dir)
  "Return all Org-roam files located recursively within DIR, using find, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (names (s-join " -o " (mapcar (lambda (glob) (concat "-name " glob)) globs)))
         (command (s-join " " `(,executable "-L" ,dir "-type f \\(" ,names "\\)"))))
    (org-roam--shell-command-files command)))

;; Emacs 26 does not have FOLLOW-SYMLINKS in `directory-files-recursively'
(defun org-roam--directory-files-recursively (dir regexp
                                                  &optional include-directories predicate
                                                  follow-symlinks)
  "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (condition-case nil
                               (org-roam--directory-files-recursively
                                full-file regexp include-directories
                                predicate follow-symlinks)
                             (file-error nil))
                         (org-roam--directory-files-recursively
                          full-file regexp include-directories
                          predicate follow-symlinks))))
                  (setq result (nconc result sub-files))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let* ((file-regex (concat "\\.\\(?:"
                             (mapconcat #'regexp-quote org-roam-file-extensions "\\|")
                             "\\)\\(?:\\.gpg\\)?\\'"))
         (files (org-roam--directory-files-recursively dir file-regex nil nil t))
         result)
    (dolist (file files result)
      (when (and (file-readable-p file)
                 (org-roam--org-file-p file))
        (push file result)))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located recursively within DIR.
Use external shell commands if defined in `org-roam-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (seq-filter #'org-roam--org-roam-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (expand-file-name org-roam-directory)))

;;;; Fetchers
(defun org-roam--collate-types (type)
  "Collate TYPE into a parent type.
Packages like `org-ref' introduce many different link prefixes,
but we collate them under the same parent type to clean up
backlinks."
  (cond ((and (boundp 'org-ref-cite-types)
              (member type org-ref-cite-types))
         "cite")
        ((member type '("http" "https"))
         "website")
        (t type)))

;;;; Title/Path/Slug conversion
(defun org-roam--path-to-slug (path)
  "Return a slug from PATH."
  (-> path
      (file-relative-name (expand-file-name org-roam-directory))
      (file-name-sans-extension)))

(defun org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))

(defun org-roam--add-tag-string (str tags)
  "Add TAGS to STR.

Depending on the value of `org-roam-file-completion-tag-position', this function
prepends TAGS to STR, appends TAGS to STR or omits TAGS from STR."
  (pcase org-roam-file-completion-tag-position
    ('prepend (concat
               (when tags (propertize (format "(%s) " (s-join org-roam-tag-separator tags))
                                      'face 'org-roam-tag))
               str))
    ('append (concat
              str
              (when tags (propertize (format " (%s)" (s-join org-roam-tag-separator tags))
                                     'face 'org-roam-tag))))
    ('omit str)))

(defun org-roam--tags-table ()
  "Return a hash table of node ID to list of tags."
  (let ((ht (make-hash-table :test #'equal)))
    (pcase-dolist (`(,node-id ,tag) (org-roam-db-query [:select [node-id tag] :from tags]))
      (puthash node-id (cons tag (gethash node-id ht)) ht))
    ht))

(defun org-roam--node-completions ()
  "Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is a plist containing the properties of the node."
  (let* ((rows (org-roam-db-query [:select [nodes:file nodes:id nodes:title nodes:pos]
                                   :from nodes]))
         (alias-rows (org-roam-db-query [:select [nodes:file nodes:id aliases:alias nodes:pos] :from aliases
                                         :left :join nodes :on (= aliases:node_id nodes:id)]))
         (tag-table (org-roam--tags-table))
         completions tags)
    (dolist (row rows)
      (pcase-let ((`(,file-path ,id ,title ,pos) row))
        (when-let ((tags (gethash id tag-table)))
          (setq title (org-roam--add-tag-string title tags)))
        (push (cons title
                    (list :path file-path :title title :point pos :id id :tags tags))
              completions)))
    (dolist (row alias-rows completions)
      (pcase-let ((`(,file-path ,id ,alias ,pos) row))
        (when-let ((tags (gethash id tag-table)))
          (setq alias (org-roam--add-tag-string alias tags)))
        (push (cons alias
                    (list :path file-path :title alias :point pos :id id :tags tags))
              completions)))))

(defun org-roam--get-index-path ()
  "Return the path to the index in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, it is assumed to be a note in `org-roam-directory'
whose title is 'Index'."
  (let ((path (pcase org-roam-index-file
                ((pred functionp) (funcall org-roam-index-file))
                ((pred stringp) org-roam-index-file)
                ('nil (user-error "You need to set `org-roam-index-file' before you can jump to it"))
                (wrong-type (signal 'wrong-type-argument
                                    `((functionp stringp)
                                      ,wrong-type))))))
    (expand-file-name path org-roam-directory)))

;;;; org-roam-find-ref
(defun org-roam--get-ref-path-completions (&optional arg filter)
  "Return an alist of refs to absolute path of Org-roam files.

When called interactively (i.e. when ARG is 1), formats the car
of the completion-candidates with extra information: title, tags,
and type \(when `org-roam-include-type-in-ref-path-completions'
is non-nil).

When called with a `C-u' prefix (i.e. when ARG is 4), forces the
default format without the formatting.

FILTER can either be a string or a function:

- If it is a string, it should be the type of refs to include as
  candidates \(e.g. \"cite\", \"website\", etc.)

- If it is a function, it should be the name of a function that
  takes three arguments: the type, the ref, and the file of the
  current candidate. It should return t if that candidate is to
  be included as a candidate."
  nil
  ;; (let ((rows (org-roam-db-query
  ;;              [:select [refs:type refs:ref refs:file titles:title tags:tags]
  ;;               :from titles
  ;;               :left :join tags
  ;;               :on (= titles:file tags:file)
  ;;               :left :join refs :on (= titles:file refs:file)
  ;;               :where refs:file :is :not :null]))
  ;;       completions)
  ;;   (setq rows (seq-sort-by (lambda (x)
  ;;                             (plist-get (nth 3 x) :mtime))
  ;;                           #'time-less-p
  ;;                           rows))
  ;;   (dolist (row rows completions)
  ;;     (pcase-let ((`(,type ,ref ,file-path ,title ,tags) row))
  ;;       (when (pcase filter
  ;;               ('nil t)
  ;;               ((pred stringp) (string= type filter))
  ;;               ((pred functionp) (funcall filter type ref file-path))
  ;;               (wrong-type (signal 'wrong-type-argument
  ;;                                   `((stringp functionp)
  ;;                                     ,wrong-type))))
  ;;         (let ((k (if (eq arg 1)
  ;;                      (concat
  ;;                       (when org-roam-include-type-in-ref-path-completions
  ;;                         (format "{%s} " type))
  ;;                       (org-roam--add-tag-string (format "%s (%s)" title ref)
  ;;                                                 tags))
  ;;                    ref))
  ;;               (v (list :path file-path :type type :ref ref)))
  ;;           (push (cons k v) completions))))))
  )

(defun org-roam--find-ref (ref)
  "Find and open and Org-roam file from REF if it exists.
REF should be the value of '#+roam_key:' without any
type-information (e.g. 'cite:').
Return nil if the file does not exist."
  (when-let* ((completions (org-roam--get-ref-path-completions))
              (file (plist-get (cdr (assoc ref completions)) :path)))
    (find-file file)))

(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

;;; Completion at point
(defcustom org-roam-completion-everywhere nil
  "If non-nil, provide completions from the current word at point."
  :group 'org-roam
  :type 'boolean)

;;;; Tags completion
(defun org-roam-complete-tags-at-point ()
  "`completion-at-point' function for Org-roam tags."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (when (looking-back "^#\\+roam_tags:.*" (line-beginning-position))
      (when (looking-at "\\>")
        (setq start (save-excursion (skip-syntax-backward "w")
                                    (point))
              end (point)))
      (setq collection #'org-roam-db--get-tags
            exit-fn (lambda (str _status)
                      (delete-char (- (length str)))
                      (insert "\"" str "\""))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function exit-fn)))))

(defun org-roam--get-titles ()
  "Return all titles and aliases in the Org-roam database."
  (let* ((titles (mapcar #'car (org-roam-db-query [:select title :from nodes])))
         (aliases (mapcar #'car (org-roam-db-query [:select alias :from aliases])))
         (completions (append titles aliases)))
    completions))

(defun org-roam-complete-everywhere ()
  "`completion-at-point' function for word at point.
This is active when `org-roam-completion-everywhere' is non-nil."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (when (and org-roam-completion-everywhere
               (thing-at-point 'word))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (setq start (car bounds)
              end (cdr bounds)
              collection #'org-roam--get-titles
              exit-fn (lambda (str _status)
                        (delete-char (- (length str)))
                        (insert "[[roam:" str "]]")))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function exit-fn)))))

(add-to-list 'org-roam-completion-functions #'org-roam-complete-tags-at-point)
(add-to-list 'org-roam-completion-functions #'org-roam-complete-everywhere)
(add-to-list 'org-roam-completion-functions #'org-roam-link-complete-at-point)

;;; Org-roam-mode
;;;; Function Faces
;; These faces are used by `org-link-set-parameters', which take one argument,
;; which is the path.
(defcustom org-roam-link-use-custom-faces t
  "Define where to apply custom faces to Org-roam links.

Valide values are:

t            Use custom faces inside Org-roam notes (i.e. files in
             `org-roam-directory'.)

everywhere   Apply custom faces everywhere.

Otherwise, do not apply custom faces to Org-roam links."
  :type '(choice
          (const :tag "Use custom faces inside Org-roam notes" t)
          (const :tag "Apply custom faces everywhere" everywhere)
          (const :tag "Do not apply custom faces" nil))
  :group 'org-roam)

;;; Org-roam entry point
(defun org-roam-setup ()
  "Setup Org-roam."
  (interactive)
  (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                   (file-executable-p emacsql-sqlite3-executable))
              (executable-find "sqlite3"))
    (lwarn '(org-roam) :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'. \
M-x info for more information at Org-roam > Installation > Post-Installation Tasks."))
  (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
  (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-add 'rename-file :after #'org-roam--rename-file-advice)
  (advice-add 'delete-file :before #'org-roam--delete-file-advice)
  (advice-add 'org-id-new :after #'org-roam--id-new-advice)
  (org-roam-db-build-cache))

(defun org-roam-teardown ()
  "Teardown Org-roam."
  (interactive)
  (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
  (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (advice-remove 'rename-file #'org-roam--rename-file-advice)
  (advice-remove 'delete-file #'org-roam--delete-file-advice)
  (advice-remove 'org-id-new #'org-roam--id-new-advice)
  (org-roam-db--close-all)
  ;; Disable local hooks for all org-roam buffers
  (dolist (buf (org-roam--get-roam-buffers))
    (with-current-buffer buf
      (remove-hook 'after-save-hook #'org-roam-db-update-file t))))

;;; Hooks and advices
(defun org-roam--find-file-hook-function ()
  "Setup automatic database update."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (run-hooks 'org-roam-file-setup-hook) ; Run user hooks
    (add-hook 'after-save-hook #'org-roam-db-update-file nil t)
    (dolist (fn org-roam-completion-functions)
      (add-hook 'completion-at-point-functions fn nil t))))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (org-roam--org-roam-file-p file))
    (org-roam-db-clear-file (expand-file-name file))))

(defun org-roam--rename-file-advice (old-file new-file-or-dir &rest _args)
  "Rename backlinks of OLD-FILE to refer to NEW-FILE-OR-DIR.
When NEW-FILE-OR-DIR is a directory, we use it to compute the new file path."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir)))
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam--org-roam-file-p old-file))
      (org-roam-db-clear-file old-file)
      (when (org-roam--org-roam-file-p new-file)
        (org-roam-db-update-file new-file)))))

(defun org-roam--id-new-advice (&rest _args)
  "Update the database if a new Org ID is created."
  (when (and (org-roam--org-roam-file-p)
             (not (org-roam-capture-p)))
    (org-roam-db-update-file)))

;;; Visiting functions
;;;; File

;;;; olp

;;;; preview
;;; Interactive Commands
;;;###autoload
(defun org-roam-find-node (&optional initial-prompt filter-fn no-confirm)
  "Find and open an Org-roam node by its title or alias.
INITIAL-PROMPT is the initial prompt.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt."
  (interactive)
  (let* ((completions (org-roam--node-completions))
         (completions (if filter-fn
                          (funcall filter-fn completions)
                        completions))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (completing-read "File: " completions nil nil initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path)))
    (if file-path
        (progn
          (find-file file-path)
          (goto-char (plist-get res :point)))
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title-with-tags))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))

;;;###autoload
(defun org-roam-find-directory ()
  "Find and open `org-roam-directory'."
  (interactive)
  (find-file org-roam-directory))

;;;###autoload
(defun org-roam-find-ref (arg &optional filter)
  "Find and open an Org-roam file from a ref.
ARG is used to forward interactive calls to
`org-roam--get-ref-path-completions'
FILTER can either be a string or a function:
- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)
- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate."
  (interactive "p")
  (let* ((completions (org-roam--get-ref-path-completions arg filter))
         (ref (completing-read "Ref: " completions nil t))
         (file (-> (cdr (assoc ref completions))
                   (plist-get :path))))
    (find-file file)))

;;;###autoload
(defun org-roam-random-note ()
  "Find a random Org-roam file."
  (interactive)
  (find-file (seq-random-elt (org-roam--list-all-files))))

;;;###autoload
(defun org-roam-insert (&optional lowercase completions filter-fn description link-type)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
LINK-TYPE is the type of link to be created. It defaults to \"file\".
COMPLETIONS is a list of completions to be used instead of
`org-roam--node-completions'.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label."
  (interactive "P")
  ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (completions (--> (or completions
                                     (org-roam--node-completions))
                              (if filter-fn
                                  (funcall filter-fn it)
                                it)))
               (title-with-tags (completing-read "File: " completions nil nil region-text))
               (res (cdr (assoc title-with-tags completions)))
               (title (or (plist-get res :title)
                          title-with-tags))
               (target-id (plist-get res :id))
               (target-file (plist-get res :path))
               (description (or description region-text title))
               (description (if lowercase
                                (downcase description)
                              description)))
          (cond ((and target-id (file-exists-p target-file))
                 (when region-text
                   (delete-region beg end)
                   (set-marker beg nil)
                   (set-marker end nil))
                 (insert (org-link-make-string (concat "id:" target-id)
                                               description)))
                (t
                 (let ((org-roam-capture--info
                        `((title . ,title-with-tags)
                          (slug . ,(funcall org-roam-title-to-slug-function title-with-tags))))
                       (org-roam-capture--context 'title))
                   (setq org-roam-capture-additional-template-props
                         (list :region (org-roam-shield-region beg end)
                               :insert-at (point-marker)
                               :link-type link-type
                               :link-description description
                               :finalize 'insert-link))
                   (org-roam-capture--capture))))
          res))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-jump-to-index ()
  "Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one."
  (interactive)
  (let ((index (org-roam--get-index-path)))
    (if (and index
             (file-exists-p index))
        (find-file index)
      (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
        (org-roam-find-file "Index")))))

;;; Diagnostics
;;;###autoload
(defun org-roam-version (&optional message)
  "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area."
  (interactive)
  (let* ((version
          (with-temp-buffer
            (insert-file-contents-literally (locate-library "org-roam.el"))
            (goto-char (point-min))
            (save-match-data
              (if (re-search-forward "\\(?:;; Version: \\([^z-a]*?$\\)\\)" nil nil)
                  (substring-no-properties (match-string 1))
                "N/A")))))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

;;;###autoload
(defun org-roam-diagnostics ()
  "Collect and print info for `org-roam' issues."
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*org-roam diagnostics*"))
    (erase-buffer)
    (insert (propertize "Copy info below this line into issue:\n" 'face '(:weight bold)))
    (insert (format "- Emacs: %s\n" (emacs-version)))
    (insert (format "- Framework: %s\n"
                    (condition-case _
                        (completing-read "I'm using the following Emacs framework:"
                                         '("Doom" "Spacemacs" "N/A" "I don't know"))
                      (quit "N/A"))))
    (insert (format "- Org: %s\n" (org-version nil 'full)))
    (insert (format "- Org-roam: %s" (org-roam-version)))))

(provide 'org-roam)
;;; org-roam.el ends here

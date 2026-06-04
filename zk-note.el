;;; zk-note.el --- Note model and file operations for zk -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; File-first org note operations for zk. Permanent notes use Luhmann
;; addresses as canonical filenames; draft notes use generated filenames.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defgroup zk nil
  "Zk-style note workflow for Emacs."
  :group 'org)

(defcustom zk-directory (expand-file-name "zk" user-emacs-directory)
  "Directory for permanent addressed zk notes."
  :type 'directory
  :group 'zk)

(defcustom zk-inbox-directory (expand-file-name "inbox" zk-directory)
  "Directory for draft zk notes."
  :type 'directory
  :group 'zk)

(defcustom zk-file-extension "org"
  "File extension for zk notes, without leading dot."
  :type 'string
  :group 'zk)

(defcustom zk-id-function #'org-id-new
  "Function called with no arguments to produce a stable org ID."
  :type 'function
  :group 'zk)

(defcustom zk-draft-filename-function #'zk-note-default-draft-filename
  "Function called with SLUG to produce a draft filename."
  :type 'function
  :group 'zk)

(defcustom zk-link-format 'zk
  "Default link format for zk links.
The value `zk' uses the Luhmann address. The value `file' uses the
canonical addressed filename. The value `id' uses the org ID property."
  :type '(choice (const :tag "Zk address links" zk)
                 (const :tag "File links" file)
                 (const :tag "ID links" id))
  :group 'zk)

(cl-defstruct zk-note
  path title id address tags aliases source created modified)

(defun zk-note-slugify (s)
  "Return a filesystem-friendly slug for S."
  (let* ((down (downcase (string-trim (or s ""))))
         (ascii (replace-regexp-in-string "[^[:alnum:]]+" "-" down))
         (trimmed (replace-regexp-in-string "^-\|-+$" "" ascii)))
    (if (string-empty-p trimmed) "untitled" trimmed)))

(defun zk-note-new-id ()
  "Return a new stable org ID."
  (funcall zk-id-function))

(defun zk-note-default-draft-filename (slug)
  "Return the default draft filename for SLUG."
  (format-time-string
   (concat "%Y%m%dT%H%M%S-" (zk-note-slugify slug) "." zk-file-extension)))

(defun zk-note-valid-address-p (address)
  "Return non-nil when ADDRESS has valid Luhmann address syntax."
  (and (stringp address)
       (string-match-p "\\`[0-9]+[a-z0-9]*\\'" address)))

(defun zk-note-normalize-address (address)
  "Normalize ADDRESS for use as a canonical filename basename.
Uppercase input is normalized to lowercase. Valid addresses start
with one or more digits and then contain only lowercase letters or
digits."
  (let* ((trimmed (string-trim (or address "")))
         (base (if (string-suffix-p (concat "." zk-file-extension) trimmed t)
                   (file-name-sans-extension trimmed)
                 trimmed))
         (normalized (downcase base)))
    (unless (zk-note-valid-address-p normalized)
      (user-error "Invalid zk address: %s" address))
    normalized))

(defun zk-note-address-filename (address)
  "Return canonical filename for ADDRESS."
  (concat (zk-note-normalize-address address) "." zk-file-extension))

(defun zk-note--ensure-directory (dir)
  "Ensure DIR exists."
  (make-directory dir t))

(defun zk-note--note-extension-regexp ()
  "Return regexp matching zk note files."
  (concat "\\." (regexp-quote zk-file-extension) "\\'"))

(defun zk-note--real-note-file-p (path)
  "Return non-nil when PATH names a real zk note file."
  (let ((name (file-name-nondirectory path)))
    (and (string-match-p (zk-note--note-extension-regexp) path)
         (not (string-prefix-p ".#" name))
         (not (string-prefix-p "#" name))
         (not (string-suffix-p "~" name)))))

(defun zk-note-files (&optional include-inbox)
  "Return zk note files.
When INCLUDE-INBOX is non-nil, include draft notes too."
  (let ((dirs (delq nil (list zk-directory (and include-inbox zk-inbox-directory))))
        files)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (setq files
              (append files
                      (seq-filter #'zk-note--real-note-file-p
                                  (directory-files-recursively dir (zk-note--note-extension-regexp)))))))
    (delete-dups files)))

(defun zk-note--read-file (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun zk-note--keyword (name text)
  "Return org keyword NAME from TEXT, or nil."
  (when (string-match (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote name)) text)
    (string-trim (match-string 1 text))))

(defun zk-note--property (name text)
  "Return property NAME from first org property drawer in TEXT, or nil."
  (when (string-match (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" (regexp-quote name)) text)
    (string-trim (match-string 1 text))))

(defun zk-note--parse-tags (raw)
  "Parse RAW org filetags string."
  (when raw
    (split-string (string-trim raw ":" ":") ":" t)))

(defun zk-note--inferred-address (path)
  "Return inferred address for PATH, or nil if PATH is not address-named."
  (unless (file-in-directory-p path zk-inbox-directory)
    (let ((base (downcase (file-name-base path))))
      (when (zk-note-valid-address-p base)
        base))))

(defun zk-note-read (path)
  "Read PATH as a `zk-note'."
  (let* ((text (zk-note--read-file path))
         (title (or (zk-note--keyword "title" text)
                    (file-name-base path)))
         (tags (zk-note--parse-tags (zk-note--keyword "filetags" text))))
    (make-zk-note
     :path path
     :title title
     :id (zk-note--property "ID" text)
     :address (or (zk-note--keyword "address" text)
                  (zk-note--inferred-address path))
     :tags tags
     :aliases (zk-note--keyword "aliases" text)
     :source (zk-note--keyword "source" text)
     :created (zk-note--property "ZK_CREATED" text)
     :modified (zk-note--property "ZK_MODIFIED" text))))

(defun zk-note-all (&optional include-inbox)
  "Return all zk notes.
When INCLUDE-INBOX is non-nil, include draft notes too."
  (mapcar #'zk-note-read (zk-note-files include-inbox)))

(defun zk-note-format-metadata (title id &optional address tags created modified)
  "Return org metadata block for TITLE, ID, ADDRESS, TAGS, CREATED, and MODIFIED."
  (let ((date (format-time-string "%Y-%m-%d")))
    (concat
     "#+title: " title "\n"
     (when address
       (concat "#+address: " (zk-note-normalize-address address) "\n"))
     (when tags
       (concat "#+filetags: :" (string-join tags ":") ":\n"))
     ":PROPERTIES:\n"
     ":ID: " id "\n"
     ":ZK_CREATED: " (or created date) "\n"
     ":ZK_MODIFIED: " (or modified date) "\n"
     ":END:\n\n")))

(defun zk-note--unique-path (dir filename)
  "Return a non-existing path under DIR based on FILENAME."
  (let* ((base (file-name-sans-extension filename))
         (ext (file-name-extension filename t))
         (candidate (expand-file-name filename dir))
         (n 2))
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name (format "%s-%d%s" base n ext) dir))
      (setq n (1+ n)))
    candidate))

(defun zk-note--same-file-name-p (a b)
  "Return non-nil if A and B name the same expanded file path."
  (string= (expand-file-name a) (expand-file-name b)))

(defun zk-note-address-conflict (address &optional except-path)
  "Return path of note already using ADDRESS, excluding EXCEPT-PATH."
  (let ((normalized (zk-note-normalize-address address))
        conflict)
    (dolist (path (zk-note-files t))
      (unless (and except-path (zk-note--same-file-name-p path except-path))
        (let ((note (zk-note-read path)))
          (when (equal (zk-note-address note) normalized)
            (setq conflict path)))))
    conflict))

(defun zk-note--assert-address-free (address &optional except-path)
  "Signal an error if ADDRESS is already used, excluding EXCEPT-PATH."
  (when-let ((conflict (zk-note-address-conflict address except-path)))
    (user-error "Address already exists: %s (%s)"
                (zk-note-normalize-address address)
                conflict)))

(defun zk-note--register-id (id path)
  "Register ID at PATH with org-id when possible."
  (when (and id (fboundp 'org-id-add-location))
    (org-id-add-location id (expand-file-name path))))

(defun zk-note-update-id-locations ()
  "Update org-id locations for zk notes."
  (interactive)
  (setq org-id-locations (make-hash-table :test 'equal))
  (dolist (note (zk-note-all t))
    (when (zk-note-id note)
      (zk-note--register-id (zk-note-id note) (zk-note-path note))))
  (org-id-locations-save)
  org-id-locations)

(defun zk-note-create-draft (title &optional body id)
  "Create a draft note titled TITLE with optional BODY and ID.
Return the new file path."
  (let* ((note-id (or id (zk-note-new-id)))
         (filename (funcall zk-draft-filename-function title))
         (path (zk-note--unique-path zk-inbox-directory filename)))
    (zk-note--ensure-directory zk-inbox-directory)
    (with-temp-file path
      (insert (zk-note-format-metadata title note-id))
      (when body
        (insert body)
        (unless (string-suffix-p "\n" body)
          (insert "\n"))))
    (zk-note--register-id note-id path)
    path))

(defun zk-note-create-addressed (title address &optional body id)
  "Create an addressed note titled TITLE at ADDRESS with optional BODY and ID.
Return the new file path."
  (let* ((normalized (zk-note-normalize-address address))
         (path (expand-file-name (zk-note-address-filename normalized) zk-directory))
         (note-id (or id (zk-note-new-id))))
    (zk-note--ensure-directory zk-directory)
    (zk-note--assert-address-free normalized)
    (when (file-exists-p path)
      (user-error "Address already exists: %s" normalized))
    (with-temp-file path
      (insert (zk-note-format-metadata title note-id normalized))
      (when body
        (insert body)
        (unless (string-suffix-p "\n" body)
          (insert "\n"))))
    (zk-note--register-id note-id path)
    path))

(defun zk-note-current-path ()
  "Return current buffer file path, or signal an error."
  (or buffer-file-name
      (user-error "Current buffer is not visiting a file")))

(defun zk-note--replace-keyword (name value)
  "Replace org keyword NAME with VALUE in current buffer, inserting if absent."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+%s:.*$" (regexp-quote name)) nil t)
        (replace-match (format "#+%s: %s" name value) t t)
      (goto-char (point-min))
      (insert (format "#+%s: %s\n" name value)))))

(defun zk-note--replace-property (name value)
  "Replace org property NAME with VALUE in current buffer, inserting in first drawer."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward (format "^[ \t]*:%s:.*$" (regexp-quote name)) nil t)
      (replace-match (format ":%s: %s" name value) t t))
     ((re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
      (forward-line 1)
      (insert (format ":%s: %s\n" name value)))
     (t
      (goto-char (point-min))
      (insert ":PROPERTIES:\n" (format ":%s: %s\n" name value) ":END:\n")))))

(defun zk-note--zk-link-regexp (address)
  "Return regexp matching zk links to ADDRESS."
  (format "\\(zk:\\)%s\\([^[:alnum:]]\\|\\'\\)"
          (regexp-quote (zk-note-normalize-address address))))

(defun zk-note-count-zk-links-to (address)
  "Return number of zk links to ADDRESS across all zk notes."
  (let ((regexp (zk-note--zk-link-regexp address))
        (count 0))
    (dolist (path (zk-note-files t))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (setq count (1+ count)))))
    count))

(defun zk-note-rewrite-zk-links (old-address new-address)
  "Rewrite zk links from OLD-ADDRESS to NEW-ADDRESS.
Return the number of replacements made."
  (let ((regexp (zk-note--zk-link-regexp old-address))
        (normalized-new (zk-note-normalize-address new-address))
        (count 0))
    (dolist (path (zk-note-files t))
      (with-current-buffer (find-file-noselect path)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (replace-match (concat (match-string 1)
                                   normalized-new
                                   (match-string 2))
                           t t)
            (setq count (1+ count))))
        (when (buffer-modified-p)
          (save-buffer))))
    count))

(defun zk-note-set-address (path address)
  "Set ADDRESS on note at PATH, rename file canonically, and return new path.
This is the low-level operation used for initial placement; it does
not rewrite incoming links. Use `zk-note-readdress' for deliberate
address changes."
  (let* ((normalized (zk-note-normalize-address address))
         (new-path (expand-file-name (zk-note-address-filename normalized) zk-directory)))
    (zk-note--ensure-directory zk-directory)
    (zk-note--assert-address-free normalized path)
    (when (and (file-exists-p new-path)
               (not (zk-note--same-file-name-p path new-path)))
      (user-error "Address already exists: %s" normalized))
    (with-current-buffer (find-file-noselect path)
      (zk-note--replace-keyword "address" normalized)
      (save-buffer))
    (unless (string= (expand-file-name path) (expand-file-name new-path))
      (rename-file path new-path))
    (zk-note--register-id (zk-note-id (zk-note-read new-path)) new-path)
    new-path))

(defun zk-note-readdress (path new-address &optional no-confirm)
  "Deliberately change addressed note PATH to NEW-ADDRESS.
Incoming `zk:' links are rewritten. Interactively, callers should
leave NO-CONFIRM nil to preserve address-change friction. Return a
plist with :old-address, :new-address, :old-path, :new-path, and
:links-rewritten."
  (let* ((note (zk-note-read path))
         (old-address (zk-note-address note))
         (normalized-new (zk-note-normalize-address new-address)))
    (unless old-address
      (user-error "Current note has no address; use zk-refile for drafts"))
    (when (equal old-address normalized-new)
      (user-error "New address is the same as old address: %s" old-address))
    (zk-note--assert-address-free normalized-new path)
    (let ((link-count (zk-note-count-zk-links-to old-address)))
      (when (and (not no-confirm)
                 (not (yes-or-no-p
                       (format "Readdress %s -> %s and rewrite %d incoming zk link(s)? "
                               old-address normalized-new link-count))))
        (user-error "Readdress cancelled"))
      (let ((new-path (zk-note-set-address path normalized-new))
            (rewritten 0))
        (setq rewritten (zk-note-rewrite-zk-links old-address normalized-new))
        (list :old-address old-address
              :new-address normalized-new
              :old-path path
              :new-path new-path
              :links-rewritten rewritten)))))

(defun zk-note-refile (path address)
  "Promote draft note PATH to permanent ADDRESS.
Return the new path."
  (zk-note-set-address path address))

(defun zk-note--replace-first-heading (title)
  "Replace the first Org heading with TITLE when one exists."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*+ .*$" nil t)
      (replace-match (concat "* " title) t t))))

(defun zk-note-rename-title (path title)
  "Update note title at PATH and return PATH."
  (with-current-buffer (find-file-noselect path)
    (zk-note--replace-keyword "title" title)
    (zk-note--replace-first-heading title)
    (save-buffer))
  path)

(provide 'zk-note)

;;; zk-note.el ends here

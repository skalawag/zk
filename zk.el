;;; zk.el --- Zk-style note workflow for org-mode -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Version: 0.1.0
;; Keywords: outlines, hypermedia, notes

;;; Commentary:

;; zk provides a file-first note workflow for org-mode. Drafts are captured
;; quickly into an inbox; permanent notes use Luhmann-style addresses as their
;; canonical filenames.

;;; Code:

(require 'zk-note)
(require 'zk-template)
(require 'zk-search)
(require 'zk-link)
(require 'zk-review)

(defcustom zk-index-file nil
  "Path to the zk index file.
When nil, use `index.org' in `zk-directory'."
  :type '(choice (const :tag "Default index.org in zk-directory" nil)
                 file)
  :group 'zk)

(defconst zk--modules
  '(zk-note zk-template zk-search zk-link zk-review zk)
  "Zk modules in load order.")

;;;###autoload
(defun zk-reload ()
  "Reload zk modules for development without restarting Emacs."
  (interactive)
  (let ((dirs (delete-dups
               (delq nil
                     (mapcar (lambda (feature)
                               (when-let ((file (locate-library (symbol-name feature))))
                                 (file-name-directory file)))
                             zk--modules)))))
    (dolist (feature (reverse zk--modules))
      (when (featurep feature)
        (ignore-errors (unload-feature feature t))))
    (dolist (dir dirs)
      (add-to-list 'load-path dir))
    (require 'zk)
    (message "Reloaded zk")))

(defun zk-index-path ()
  "Return the effective zk index file path."
  (or zk-index-file
      (expand-file-name "index.org" zk-directory)))

;;;###autoload
(defun zk-index-open ()
  "Open the zk index file, creating it if needed."
  (interactive)
  (let ((path (zk-index-path)))
    (make-directory (file-name-directory path) t)
    (unless (file-exists-p path)
      (with-temp-file path
        (insert "#+title: Index\n\n")))
    (find-file path)))

;;;###autoload
(defun zk-new (title address &optional template-key)
  "Create a permanent addressed note with TITLE at ADDRESS.
With prefix, prompt for TEMPLATE-KEY."
  (interactive
   (list (read-string "Title: ")
         (read-string "Address: ")
         (when current-prefix-arg
           (zk-template-read-key))))
  (let* ((id (zk-note-new-id))
         (template (or template-key zk-default-template))
         (body (zk-template-body template title id))
         (path (zk-note-create-addressed title address body id)))
    (find-file path)))

;;;###autoload
(defun zk-capture (slug)
  "Capture a draft note for SLUG and open it for editing."
  (interactive (list (read-string "Draft slug: ")))
  (let ((path (zk-note-create-draft slug)))
    (find-file path)))

;;;###autoload
(defun zk-refile (address)
  "Promote current draft note to permanent ADDRESS."
  (interactive (list (read-string "Address: ")))
  (let* ((old-path (zk-note-current-path))
         (new-path (zk-note-refile old-path address)))
    (when (get-file-buffer old-path)
      (kill-buffer (get-file-buffer old-path)))
    (find-file new-path)))

;;;###autoload
(defun zk-readdress (address)
  "Deliberately change the current note to ADDRESS and rewrite incoming links."
  (interactive (list (read-string "New address: ")))
  (let* ((old-path (zk-note-current-path))
         (result (zk-note-readdress old-path address))
         (new-path (plist-get result :new-path))
         (rewritten (plist-get result :links-rewritten)))
    (when (get-file-buffer old-path)
      (kill-buffer (get-file-buffer old-path)))
    (find-file new-path)
    (message "Readdressed %s -> %s; rewrote %d zk link(s)"
             (plist-get result :old-address)
             (plist-get result :new-address)
             rewritten)))

;;;###autoload
(defun zk-set-address (address)
  "Compatibility alias for `zk-readdress'."
  (interactive (list (read-string "New address: ")))
  (zk-readdress address))

;;;###autoload
(defun zk-rename (title)
  "Rename the current note title to TITLE.
This does not change the note address or filename."
  (interactive (list (read-string "Title: " (when buffer-file-name
                                               (zk-note-title (zk-note-read buffer-file-name))))))
  (zk-note-rename-title (zk-note-current-path) title)
  (revert-buffer :ignore-auto :noconfirm))

;;;###autoload
(defun zk-update-id-locations ()
  "Update org-id locations for zk notes."
  (interactive)
  (zk-note-update-id-locations))

;;;###autoload
(defun zk-dispatch ()
  "Dispatch zk commands."
  (interactive)
  (if (fboundp 'transient-define-prefix)
      (call-interactively #'zk-transient)
    (let* ((commands '(("new" . zk-new)
                       ("capture" . zk-capture)
                       ("find/open" . zk-open)
                       ("index" . zk-index-open)
                       ("search" . zk-search)
                       ("link" . zk-link)
                       ("reload" . zk-reload)
                       ("refile" . zk-refile)
                       ("readdress" . zk-readdress)
                       ("rename-title" . zk-rename)
                       ("update-id-locations" . zk-update-id-locations)
                       ("review" . zk-review)))
           (choice (completing-read "zk: " commands nil t)))
      (call-interactively (cdr (assoc choice commands))))))

(when (require 'transient nil t)
  (transient-define-prefix zk-transient ()
    "Transient command hub for zk."
    [["Create"
      ("n" "new addressed note" zk-new)
      ("c" "capture draft" zk-capture)]
     ["Find"
      ("o" "open" zk-open)
      ("i" "index" zk-index-open)
      ("s" "search" zk-search)
      ("r" "review" zk-review)]
     ["Connect"
      ("l" "insert link" zk-link)
      ("L" "link dwim" zk-link-dwim)]
     ["Develop"
      ("x" "reload" zk-reload)]
     ["Maintain"
      ("f" "refile draft" zk-refile)
      ("a" "readdress note" zk-readdress)
      ("t" "rename title" zk-rename)
      ("u" "update ID locations" zk-update-id-locations)]]))

(provide 'zk)

;;; zk.el ends here

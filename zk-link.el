;;; zk-link.el --- Link insertion for zk -*- lexical-binding: t; -*-

;;; Commentary:

;; Explicit org link insertion for zk notes. The default link target is the
;; note's Luhmann address via a custom zk: link type.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'thingatpt)
(require 'zk-note)
(require 'zk-search)

(defun zk-link--file-target (note)
  "Return a file link target for NOTE."
  (let ((path (zk-note-path note)))
    (if (and (zk-note-address note)
             (file-in-directory-p path zk-directory))
        (zk-note-address-filename (zk-note-address note))
      path)))

(defun zk-link--address-target (note)
  "Return a zk address link target for NOTE."
  (unless (zk-note-address note)
    (user-error "Cannot create zk link to note without address"))
  (concat "zk:" (zk-note-normalize-address (zk-note-address note))))

(defun zk-link--note-link (note)
  "Return an org link target for NOTE."
  (pcase zk-link-format
    ('zk (zk-link--address-target note))
    ('id (if (zk-note-id note)
             (concat "id:" (zk-note-id note))
           (zk-link--address-target note)))
    (_ (concat "file:" (zk-link--file-target note)))))

(defun zk-link-path (address)
  "Return file path for zk ADDRESS."
  (expand-file-name (zk-note-address-filename address) zk-directory))

(defun zk-link-follow (address _arg)
  "Follow zk ADDRESS link."
  (let ((path (zk-link-path address)))
    (unless (file-exists-p path)
      (user-error "No zk note at address: %s" address))
    (find-file path)))

(defun zk-link-complete ()
  "Complete a zk link target."
  (let* ((candidates (zk-search-addressed-candidates))
         (choice (completing-read "Zk note: " candidates nil t))
         (note (zk-note-read (cdr (assoc choice candidates)))))
    (zk-link--address-target note)))

(org-link-set-parameters "zk"
                         :follow #'zk-link-follow
                         :complete #'zk-link-complete)

(defun zk-link--read-description (default)
  "Read link description, using DEFAULT when input is empty."
  (let ((description (read-string (format "Description (default %s): " default)
                                  nil nil default)))
    (if (string-empty-p description)
        default
      description)))

(defun zk-link--same-file-p (a b)
  "Return non-nil when A and B name the same file."
  (and a b (string= (file-truename a) (file-truename b))))

(defun zk-link ()
  "Insert a link to a selected zk note.
Prompt for link description, defaulting to the note title."
  (interactive)
  (let* ((candidates (zk-search-addressed-candidates))
         (choice (completing-read "Link note: " candidates nil t))
         (path (cdr (assoc choice candidates))))
    (when (zk-link--same-file-p path buffer-file-name)
      (user-error "Cannot link a note to itself"))
    (let* ((note (zk-note-read path))
           (description (zk-link--read-description (zk-note-title note))))
      (org-insert-link nil (zk-link--note-link note) description))))

(defun zk-link-dwim ()
  "Insert a zk link, using region or thing at point as search hint."
  (interactive)
  (let ((hint (cond
               ((use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end)))
               ((thing-at-point 'symbol t))
               (t nil))))
    (when hint
      (message "Search hint: %s" hint))
    (zk-link)))

(provide 'zk-link)

;;; zk-link.el ends here

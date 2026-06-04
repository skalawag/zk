;;; test-init.el --- Local init for testing zk.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'org)
(require 'zk)

(setq zk-directory (expand-file-name "notes" default-directory))
(setq zk-inbox-directory (expand-file-name "inbox" zk-directory))
(setq org-id-track-globally t)
(setq org-id-locations-file (expand-file-name ".org-id-locations" zk-directory))
(setq org-id-locations (make-hash-table :test 'equal))

(defun zk-test-refresh-id-locations ()
  "Refresh Org ID locations for the local zk test notes."
  (interactive)
  (when (file-directory-p zk-directory)
    (zk-update-id-locations)))

(zk-test-refresh-id-locations)

(provide 'test-init)

;;; test-init.el ends here

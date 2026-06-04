;;; zk-search.el --- Search and completion for zk -*- lexical-binding: t; -*-

;;; Commentary:

;; File-based note discovery and search for zk.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'zk-note)

(defun zk-search--candidate-label (note)
  "Return completion label for NOTE."
  (let ((title (zk-note-title note))
        (address (zk-note-address note))
        (path (zk-note-path note)))
    (if address
        (format "%s --- %s" (zk-note-address-filename address) title)
      (format "%s --- %s" (file-name-nondirectory path) title))))

(defun zk-search--candidate (note)
  "Return completion candidate for NOTE."
  (cons (zk-search--candidate-label note) (zk-note-path note)))

(defun zk-search-candidates (&optional include-inbox)
  "Return completion candidates for notes.
When INCLUDE-INBOX is non-nil, include draft notes too."
  (mapcar #'zk-search--candidate (zk-note-all include-inbox)))

(defun zk-search-addressed-candidates ()
  "Return completion candidates for addressed notes only."
  (mapcar #'zk-search--candidate
          (seq-filter #'zk-note-address (zk-note-all nil))))

(defun zk-open (&optional include-inbox)
  "Open a zk note selected by completion.
With prefix INCLUDE-INBOX, include draft notes."
  (interactive "P")
  (let* ((candidates (zk-search-candidates include-inbox))
         (choice (completing-read "Note: " candidates nil t)))
    (find-file (cdr (assoc choice candidates)))))

(defalias 'zk-find #'zk-open)

(defun zk-search (&optional initial)
  "Search note contents.
INITIAL is used as initial query when supported."
  (interactive)
  (let ((dir zk-directory))
    (cond
     ((fboundp 'consult-ripgrep)
      (consult-ripgrep dir initial))
     ((executable-find "rg")
      (let ((query (read-string "Search notes: " initial)))
        (grep (format "rg --line-number --no-heading %s %s"
                      (shell-quote-argument query)
                      (shell-quote-argument dir)))))
     (t
      (rgrep (read-string "Search notes: " initial)
             (concat "*." zk-file-extension)
             dir)))))

(provide 'zk-search)

;;; zk-search.el ends here

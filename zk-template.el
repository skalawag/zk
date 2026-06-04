;;; zk-template.el --- Templates for zk -*- lexical-binding: t; -*-

;;; Commentary:

;; Template expansion for zk notes.

;;; Code:

(require 'subr-x)
(require 'zk-note)

(defcustom zk-templates
  '((default . "")
    (draft . ""))
  "Alist of zk template names to body strings.
Supported placeholders: %title, %id, %date."
  :type '(alist :key-type symbol :value-type string)
  :group 'zk)

(defcustom zk-default-template 'default
  "Default template key used by `zk-new'."
  :type 'symbol
  :group 'zk)

(defun zk-template-expand (template title id)
  "Expand TEMPLATE for TITLE and ID."
  (let ((result (or template "")))
    (setq result (string-replace "%title" title result))
    (setq result (string-replace "%id" id result))
    (setq result (string-replace "%date" (format-time-string "%Y-%m-%d") result))
    result))

(defun zk-template-body (template-key title id)
  "Return expanded body for TEMPLATE-KEY, TITLE, and ID."
  (zk-template-expand (alist-get template-key zk-templates) title id))

(defun zk-template-read-key (&optional prompt)
  "Read a template key with PROMPT."
  (intern (completing-read (or prompt "Template: ")
                           (mapcar (lambda (entry) (symbol-name (car entry))) zk-templates)
                           nil t nil nil (symbol-name zk-default-template))))

(provide 'zk-template)

;;; zk-template.el ends here

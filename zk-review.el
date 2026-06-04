;;; zk-review.el --- Review queue for zk -*- lexical-binding: t; -*-

;;; Commentary:

;; Simple review queue for inbox notes.

;;; Code:

(require 'tabulated-list)
(require 'zk-note)

(define-derived-mode zk-review-mode tabulated-list-mode "zk-review"
  "Major mode for reviewing zk notes."
  (setq tabulated-list-format [
                               ("Title" 40 t)
                               ("Created" 12 t)
                               ("Path" 60 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun zk-review--entries ()
  "Return tabulated review entries."
  (mapcar (lambda (note)
            (let ((path (zk-note-path note)))
              (list path
                    (vector (or (zk-note-title note) "")
                            (or (zk-note-created note) "")
                            path))))
          (zk-note-all t)))

(defun zk-review ()
  "Show zk review queue."
  (interactive)
  (let ((buffer (get-buffer-create "*zk review*")))
    (with-current-buffer buffer
      (zk-review-mode)
      (setq tabulated-list-entries (zk-review--entries))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun zk-review-open ()
  "Open the note at point in a zk review buffer."
  (interactive)
  (let ((path (tabulated-list-get-id)))
    (unless path
      (user-error "No note at point"))
    (find-file path)))

(define-key zk-review-mode-map (kbd "RET") #'zk-review-open)

(provide 'zk-review)

;;; zk-review.el ends here

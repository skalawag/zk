;;; zk-test.el --- Tests for zk.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'zk)

(defmacro zk-test--with-temp-zk (&rest body)
  "Run BODY with isolated zk directories."
  (declare (indent 0))
  `(let* ((root (make-temp-file "zk-test" t))
          (zk-directory (expand-file-name "notes" root))
          (zk-inbox-directory (expand-file-name "inbox" root))
          (zk-link-format 'zk)
          (org-id-locations-file (expand-file-name ".org-id-locations" root))
          (org-id-locations (make-hash-table :test 'equal)))
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

(ert-deftest zk-reload/is-command ()
  (should (commandp #'zk-reload)))

(ert-deftest zk-dispatch-simple/selects-command ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _args)
               (caar collection)))
            ((symbol-function 'call-interactively)
             (lambda (command &optional _record-flag _keys)
               command)))
    (should (eq (zk-dispatch-simple) #'zk-new))))

(ert-deftest zk-dispatch/uses-simple-dispatch-by-default ()
  (let ((zk-use-transient nil))
    (should-not (zk--transient-available-p))))

(ert-deftest zk-dispatch/uses-transient-when-enabled ()
  (let ((zk-use-transient t))
    (cl-letf (((symbol-function 'zk-transient)
               (lambda () 'transient)))
      (should (zk--transient-available-p)))))

(ert-deftest zk-transient/is-command ()
  (should (commandp #'zk-transient)))

(ert-deftest zk-index-open/creates-default-index ()
  (zk-test--with-temp-zk
    (let (opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (path)
                   (setq opened path))))
        (zk-index-open))
      (should (equal (expand-file-name opened)
                     (expand-file-name "index.org" zk-directory)))
      (should (file-exists-p opened))
      (should (string-match-p "#\\+title: Index" (zk-note--read-file opened))))))

(ert-deftest zk-index-open/uses-custom-index-file ()
  (zk-test--with-temp-zk
    (let ((zk-index-file (expand-file-name "custom-index.org" zk-directory))
          opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (path)
                   (setq opened path))))
        (zk-index-open))
      (should (equal (expand-file-name opened)
                     (expand-file-name zk-index-file))))))

(ert-deftest zk-note-create-draft/uses-generated-filename ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-draft "Draft note" "Body")))
      (should (file-in-directory-p path zk-inbox-directory))
      (should (string-match-p "[0-9]\\{8\\}T[0-9]\\{6\\}-draft-note\\.org\\'" path))
      (let ((note (zk-note-read path)))
        (should (equal (zk-note-title note) "Draft note"))
        (should-not (zk-note-address note))))))

(ert-deftest zk-capture/prompts-only-for-slug-and-opens-draft ()
  (zk-test--with-temp-zk
    (let ((prompts nil)
          opened)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _args)
                   (push prompt prompts)
                   "draft-slug"))
                ((symbol-function 'find-file)
                 (lambda (path)
                   (setq opened path))))
        (call-interactively #'zk-capture))
      (should (equal (nreverse prompts) '("Draft slug: ")))
      (should opened)
      (should (file-exists-p opened))
      (should (file-in-directory-p opened zk-inbox-directory)))))

(ert-deftest zk-note-files/ignores-emacs-lock-and-backup-files ()
  (zk-test--with-temp-zk
    (make-directory zk-directory t)
    (let ((real (zk-note-create-addressed "Real" "1"))
          (lock (expand-file-name ".#1.org" zk-directory))
          (autosave (expand-file-name "#1.org#" zk-directory))
          (backup (expand-file-name "1.org~" zk-directory)))
      (with-temp-file lock (insert "lock"))
      (with-temp-file autosave (insert "autosave"))
      (with-temp-file backup (insert "backup"))
      (should (equal (zk-note-files) (list real))))))

(ert-deftest zk-note-create-addressed/uses-address-filename ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-addressed "Addressed note" "1a2" "Body")))
      (should (equal (file-name-nondirectory path) "1a2.org"))
      (let ((note (zk-note-read path)))
        (should (equal (zk-note-title note) "Addressed note"))
        (should (equal (zk-note-address note) "1a2"))
        (should (zk-note-id note))
        (should-not (equal (zk-note-id note) "1a2"))))))

(ert-deftest zk-note-refile/promotes-draft-to-address ()
  (zk-test--with-temp-zk
    (let* ((draft (zk-note-create-draft "Draft" "Body"))
           (draft-id (zk-note-id (zk-note-read draft)))
           (final (zk-note-refile draft "1a3")))
      (should-not (file-exists-p draft))
      (should (file-exists-p final))
      (should (equal (file-name-nondirectory final) "1a3.org"))
      (let ((note (zk-note-read final)))
        (should (equal (zk-note-address note) "1a3"))
        (should (equal (zk-note-id note) draft-id))
        (should-not (equal (zk-note-id note) "1a3"))))))

(ert-deftest zk-note-address/normalizes-uppercase ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-addressed "Upper" "1A2B")))
      (should (equal (file-name-nondirectory path) "1a2b.org"))
      (should (equal (zk-note-address (zk-note-read path)) "1a2b")))))

(ert-deftest zk-note-address/rejects-invalid-format ()
  (zk-test--with-temp-zk
    (dolist (address '("" "abc" "a1" "1 a" "1-a" "1_a" "1/2" "1.2"))
      (should-error (zk-note-create-addressed "Bad" address)))))

(ert-deftest zk-note-refile/refuses-address-collision ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Existing" "1a2")
    (let ((draft (zk-note-create-draft "Draft")))
      (should-error (zk-note-refile draft "1a2")))))

(ert-deftest zk-note-refile/refuses-duplicate-address-metadata ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Existing" "1a2")
    (let ((odd (expand-file-name "9z.org" zk-directory))
          (draft (zk-note-create-draft "Draft")))
      (with-temp-file odd
        (insert (zk-note-format-metadata "Odd duplicate" (zk-note-new-id) "1a3")))
      (should-error (zk-note-refile draft "1a3")))))

(ert-deftest zk-note-readdress/renames-and-rewrites-incoming-links ()
  (zk-test--with-temp-zk
    (let* ((source (zk-note-create-addressed "Source" "1" "[[zk:1a][Target]]\n"))
           (target (zk-note-create-addressed "Target" "1a" "Body"))
           (result (zk-note-readdress target "1b" t))
           (new-path (plist-get result :new-path)))
      (should-not (file-exists-p target))
      (should (file-exists-p new-path))
      (should (equal (file-name-nondirectory new-path) "1b.org"))
      (should (equal (zk-note-address (zk-note-read new-path)) "1b"))
      (should (= (plist-get result :links-rewritten) 1))
      (should (string-match-p "\\[\\[zk:1b\\]\\[Target\\]\\]"
                              (zk-note--read-file source))))))

(ert-deftest zk-note-readdress/does-not-rewrite-address-prefixes ()
  (zk-test--with-temp-zk
    (let* ((source (zk-note-create-addressed "Source" "1" "[[zk:1a2][Other]]\n[[zk:1a][Target]]\n"))
           (target (zk-note-create-addressed "Target" "1a" "Body")))
      (zk-note-readdress target "1b" t)
      (let ((text (zk-note--read-file source)))
        (should (string-match-p "zk:1a2" text))
        (should (string-match-p "zk:1b" text))
        (should-not (string-match-p "zk:1b2" text))))))

(ert-deftest zk-note-readdress/refuses-address-collision ()
  (zk-test--with-temp-zk
    (let ((target (zk-note-create-addressed "Target" "1a" "Body")))
      (zk-note-create-addressed "Existing" "1b" "Body")
      (should-error (zk-note-readdress target "1b" t)))))

(ert-deftest zk-note-update-id-locations/registers-ids ()
  (zk-test--with-temp-zk
    (let* ((path (zk-note-create-addressed "ID target" "1a2"))
           (id (zk-note-id (zk-note-read path))))
      (setq org-id-locations (make-hash-table :test 'equal))
      (zk-note-update-id-locations)
      (should (equal (expand-file-name path)
                     (expand-file-name (gethash id org-id-locations)))))))

(ert-deftest zk-search-candidates/include-address-filename-prefix ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a2")
    (let ((label (caar (zk-search-candidates))))
      (should (string-prefix-p "1a2.org" label))
      (should (string-match-p "Linked note" label)))))

(ert-deftest zk-search-candidates/tolerates-index-file ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a")
    (with-temp-file (expand-file-name "index.org" zk-directory)
      (insert "#+title: Index\n"))
    (let ((labels (mapcar #'car (zk-search-candidates))))
      (should (member "1a.org --- Linked note" labels))
      (should (member "index.org --- Index" labels)))))

(ert-deftest zk-search-addressed-candidates/excludes-index-file ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a")
    (with-temp-file (expand-file-name "index.org" zk-directory)
      (insert "#+title: Index\n"))
    (let ((labels (mapcar #'car (zk-search-addressed-candidates))))
      (should (member "1a.org --- Linked note" labels))
      (should-not (member "index.org --- Index" labels)))))

(ert-deftest zk-link/inserts-zk-link-with-default-description ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a2")
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (caar collection)))
                ((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial _history default &rest _args)
                   default)))
        (zk-link))
      (should (equal (buffer-string) "[[zk:1a2][Linked note]]")))))

(ert-deftest zk-link/inserts-zk-link-with-custom-description ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a2")
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (caar collection)))
                ((symbol-function 'read-string)
                 (lambda (&rest _args)
                   "custom description")))
        (zk-link))
      (should (equal (buffer-string) "[[zk:1a2][custom description]]")))))

(ert-deftest zk-link/accepts-bare-address-as-target ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a")
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) "1a"))
                ((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial _history default &rest _args)
                   default)))
        (zk-link))
      (should (equal (buffer-string) "[[zk:1a][Linked note]]")))))

(ert-deftest zk-link/accepts-address-filename-as-target ()
  (zk-test--with-temp-zk
    (zk-note-create-addressed "Linked note" "1a")
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) "1a.org"))
                ((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial _history default &rest _args)
                   default)))
        (zk-link))
      (should (equal (buffer-string) "[[zk:1a][Linked note]]")))))

(ert-deftest zk-search-addressed-candidate-path/falls-back-to-address-file ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-addressed "Linked note" "1a")))
      (should (equal (expand-file-name path)
                     (expand-file-name
                      (zk-search-addressed-candidate-path "1a" nil)))))))

(ert-deftest zk-link/follows-address-link ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-addressed "Linked note" "1a2"))
          opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (target)
                   (setq opened target))))
        (zk-link-follow "1A2" nil))
      (should (equal (expand-file-name opened) (expand-file-name path))))))

(ert-deftest zk-link/can-insert-file-link-when-configured ()
  (zk-test--with-temp-zk
    (let ((zk-link-format 'file))
      (zk-note-create-addressed "Linked note" "1a2")
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (caar collection)))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional _initial _history default &rest _args)
                     default)))
          (zk-link))
        (should (equal (buffer-string) "[[file:1a2.org][Linked note]]"))))))

(ert-deftest zk-link/rejects-self-link ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-addressed "Self" "1a2")))
      (with-current-buffer (find-file-noselect path)
        (unwind-protect
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (caar collection)))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args)
                         (ert-fail "Description prompt should not be reached"))))
              (should-error (zk-link)))
          (kill-buffer))))))

(ert-deftest zk-note-rename-title/does-not-rename-address-file ()
  (zk-test--with-temp-zk
    (let ((path (zk-note-create-addressed "Old title" "1a2")))
      (zk-note-rename-title path "New title")
      (should (file-exists-p path))
      (let ((note (zk-note-read path)))
        (should (equal (zk-note-title note) "New title"))
        (should (equal (zk-note-address note) "1a2"))))))

(provide 'zk-test)

;;; zk-test.el ends here

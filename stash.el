;;; -*- lexical-binding: t; -*-


(defun $compile-interactive ()
  (interactive)
  (compile (concat "./" (file-name-nondirectory (buffer-file-name))) t)
  (evil-window-right 1)
  (evil-insert-state))

($leader-local-set-key
  :keymaps 'python-mode-map
  "x" '$compile-interactive)

(use-package lsp-pyright
  :ensure t)  ; or lsp-deferred


(defun $read-bytecode (x)
  (let  ((arglist (aref x 0))
         (code (aref x 1))
         (const (aref x 2)))
    (vector arglist (loop for c across code collect c) const)))

(use-package company-box)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))


(use-package swift-mode
  :gfhook 'copilot-mode 'lsp
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(use-package lsp-sourcekit
  :demand t
  :after 'swift-mode)

(setq recentf-list (loop for file in recentf-list
                         unless (s-contains-p "archive" file)
                         collect file))

(defvar $image-sort-first-file nil)

(defun $image-sort-first-file ()
  (interactive)
  (let* ((directory (file-name-directory
                     (or (buffer-file-name)
                         list-buffers-directory
                         default-directory)))
         (files
          (seq-filter
           'file-regular-p
           (directory-files directory
                            'full
                            (rx bos ;; ignore auto-save-files
                                (optional ".")
                                (not (any ".#")))))))
    (setq $image-sort-first-file (cdr files))
    (find-file (car files))))

(defun $image-sort-next-file ()
  (interactive)
  (if-let ((file (car $image-sort-first-file)))
      (progn (setq $image-sort-first-file (cdr $image-sort-first-file))
             (kill-buffer (current-buffer))
             (find-file file))
    (user-error "end of files")))

(defun $image-set-filter-file (num)
  (let* ((name (buffer-file-name))
         (dir (file-name-directory name))
         (basename (file-name-nondirectory name)))
    (rename-file name (format "%s#%d_%s" dir num basename))
    ($image-sort-next-file)))

($image-set-filter-file 5)

(defhydra image-sort (:hint nil)
  "
  Sort
    _f_irst _n_next
  "
  ("f" $image-sort-first-file)
  ("n" $image-sort-next-file)
  ("1" ($image-set-filter-file 1))
  ("2" ($image-set-filter-file 2))
  ("3" ($image-set-filter-file 3))
  ("4" ($image-set-filter-file 4))
  ("5" ($image-set-filter-file 5))
  ("q" nil :exit t))

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

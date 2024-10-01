(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error nil)
 '(helm-minibuffer-history-key "M-p")
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   '((org-src-preserve-indentation)
     (eval require 'ol-man nil t)
     (eval require 'magit-base nil t)
     (elisp-lint-indent-specs
      (git-gutter:awhen . 1))
     (magit-todos-exclude-globs)
     (lisp-backquote-indentation . t)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (rust-format-on-save . t)
     (eval require 'org-roam-dev)
     (org-src-preserve-indentation . t)
     (eval visit-tags-table
           (expand-file-name "TAGS"
                             (vc-git-root default-directory))
           t)
     (checkdoc-package-keywords-flag)
     (checkdoc-minor-mode . t)
     (cperl-close-paren-offset . -4)
     (cperl-indent-parens-as-block . t)
     (eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")))
 '(savehist-additional-variables '(helm-ff-history))
 '(scroll-bar-mode nil)
 '(shx-kept-commands
   '(("show all tool config" . "ToolConfig.pl show_tool_info | ep")
     ("tool_info" . "ToolConfig.pl show_tool_info ipconfig | ep")
     ("Enable wordwrap at 90 columns" . ":eval (shx-wordwrap 90)")))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

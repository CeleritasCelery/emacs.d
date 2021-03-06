(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(debug-on-error nil)
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   '((eval require 'org-roam-dev)
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
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")))
 '(savehist-additional-variables (quote (helm-ff-history)))
 '(scroll-bar-mode nil)
 '(shx-kept-commands
   (quote
    (("show all tool config" . "ToolConfig.pl show_tool_info | ep")
     ("tool_info" . "ToolConfig.pl show_tool_info ipconfig | ep")
     ("Enable wordwrap at 90 columns" . ":eval (shx-wordwrap 90)"))))
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

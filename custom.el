(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(debug-on-error nil)
 '(safe-local-variable-values
   (quote
    ((checkdoc-minor-mode . t)
     (cperl-close-paren-offset . -4)
     (cperl-indent-parens-as-block . t)
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)"))))
 '(shx-kept-commands
   (quote
    (("mdf sim command" . "simregress -net -dut mdf_10nm -model mdf_s_v_0 -l $MODEL_ROOT/verif/reglist/mdf_10nm/dft_mdf_s_v_0_level0.list -C 'SLES11SP4&&20G' -save && simregress -net -dut mdf_10nm -model mdf_c_v_0 -l $MODEL_ROOT/verif/reglist/mdf_10nm/dft_mdf_c_v_0_level0.list -C 'SLES11SP4&&20G' -save && simregress -net -dut mdf_10nm -model mdf_s_h_0 -l $MODEL_ROOT/verif/reglist/mdf_10nm/dft_mdf_s_h_0_level0.list -C 'SLES11SP4&&20G' -save")
     ("show all tool config" . "ToolConfig.pl show_tool_info | ep")
     ("tool_info" . "ToolConfig.pl show_tool_info ipconfig | ep")
     ("git filter branch" . "git filter-branch --tag-name-filter cat --prune-empty --subdirectory-filter layers/shell-config/local/shell-config")
     ("Enable wordwrap at 90 columns" . ":eval (shx-wordwrap 90)")))))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

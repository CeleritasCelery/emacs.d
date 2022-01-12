;; early init -*- lexical-binding: t -*-

(when (eq system-type 'darwin)
  ;; required for native compile to work
  ;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323#issuecomment-805490388
  (setenv "LIBRARY_PATH" "/opt/homebrew/Cellar/gcc/11.2.0_3/lib/gcc/11:/opt/homebrew/Cellar/libgccjit/11.2.0_1/lib/gcc/11:/opt/homebrew/Cellar/gcc/11.2.0_3/lib/gcc/11/gcc/aarch64-apple-darwin21/11/")
  (setenv "MANPATH" "/opt/local/share/man:/opt/homebrew/share/man:")
  (setenv "PATH" "/opt/local/bin:/opt/local/sbin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/troyhinckley/.cargo/bin")
  (setq exec-path '("/opt/local/bin/" "/opt/local/sbin/" "/opt/homebrew/bin/" "/opt/homebrew/sbin/" "/usr/local/bin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" "/Users/troyhinckley/.cargo/bin/" "/opt/homebrew/Cellar/emacs-plus@28/28.0.50/libexec/emacs/28.0.91/aarch64-apple-darwin21.2.0/")))

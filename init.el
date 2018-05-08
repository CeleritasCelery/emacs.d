 ;; init.el --- My init file, calls out to the literate code -*- lexical-binding: t -*-


;; Copyright (C) 2018  Troy Hinckley

;; based on https://github.com/gilbertw1/emacs-literate-starter/blob/master/init.el

;;; Code:

;; Set this to a high value so we don't have a lot of costly garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil)
      (el-file (expand-file-name "emacs.el" user-emacs-directory))
      (org-file (expand-file-name "emacs.org" user-emacs-directory)))
  ;; If config is updated, load it
  (if (and (file-exists-p el-file)
           (file-newer-than-file-p el-file org-file))
      (load-file el-file)
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file org-file)))

(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

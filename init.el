 ;; init.el --- My init file, calls out to the literate code -*- lexical-binding: t -*-


;; Copyright (C) 2018  Troy Hinckley

;; based on https://github.com/gilbertw1/emacs-literate-starter/blob/master/init.el

;;; Code:

;; Set this to a high value so we don't have a lot of costly garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(defun $load-literate-file (name &optional dir)
  "if file is tangled load the .el file, else tangle it"
  (setq dir (or dir user-emacs-directory))
  (let ((file-name-handler-alist nil)
        (load-prefer-newer t)
        (el-file (expand-file-name (concat name ".el") dir))
        (org-file (expand-file-name (concat name ".org") dir)))
    ;; If config is updated, load it
    (if (and (file-exists-p el-file)
             (file-newer-than-file-p el-file org-file))
        (load-file el-file)
      ;; Otherwise use org-babel to tangle and load the configuration
      (require 'org)
      (org-babel-load-file org-file))))

($load-literate-file "emacs")

(setq gc-cons-threshold 20000000
      gc-cons-percentage 0.1)

;;; init.el ends here

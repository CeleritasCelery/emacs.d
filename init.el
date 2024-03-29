;; init.el --- My init file, calls out to the literate code -*- lexical-binding: t -*-


;; Copyright (C) 2018  Troy Hinckley

;; based on https://github.com/gilbertw1/emacs-literate-starter/blob/master/init.el

;;; Code:

(defun $load-literate-file (name &optional dir)
  "if file is tangled load the .el file, else tangle it"
  (setq dir (or dir user-emacs-directory))
  (let ((gc-cons-threshold most-positive-fixnum)
        ;; Set this to a high value so we don't have a lot of costly
        ;; garbage collection
        (el-file (expand-file-name (concat name ".el") dir))
        (org-file (expand-file-name (concat name ".org") dir)))
    ;; If config is updated, load it
    (if (and (file-exists-p el-file)
             (file-newer-than-file-p el-file org-file))
        (load-file el-file)
      ;; Otherwise use org-babel to tangle and load the configuration
      (require 'org)
      (org-babel-load-file org-file))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org)

($load-literate-file "emacs")

;;; init.el ends here

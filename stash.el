;;; -*- lexical-binding: t; -*-


(setq recentf-list (loop for file in recentf-list
                         unless (s-contains-p "archive" file)
                         collect file))

;;;;;;;;;;;;;;;;
;; Image sort ;;
;;;;;;;;;;;;;;;;

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

(let ((base-dir "directory"))
  (cl-loop for dir in (directory-files base-dir nil "[[:alpha:]]")
           collect (cl-loop for file in (directory-files (concat base-dir dir) nil "[[:alpha:]]")
                            collect (let ((parts (split-string file "_"))
                                          (new))
                                      (setcdr parts (cons dir (cdr parts)))
                                      (setq new (string-join parts "_"))
                                      (let ((default-directory (concat base-dir dir)))
                                        (rename-file file (concat base-dir new)))))))

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

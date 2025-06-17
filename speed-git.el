;;; -*- lexical-binding: t; -*-

(defvar speed-git-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'speed-git-stage-file-at-point)
    (define-key map (kbd "u") 'speed-git-unstage-file-at-point)
    (define-key map (kbd "d") 'speed-git-diff-file-at-point)
    (define-key map (kbd "q") 'kill-this-buffer)
    map)
  "Keymap for speed-git-mode.")

(general-def '(normal visual motion) speed-git-mode-map
  "s" #'speed-git-stage-file-at-point
  "u" #'speed-git-unstage-file-at-point
  "gr" #'speed-git-refresh
  "d" #'speed-git-diff-file-at-point)


(define-derived-mode speed-git-mode fundamental-mode "SpeedGit"
  "A major mode for quickly staging and unstaging git files."
  (setq major-mode 'speed-git-mode)
  (setq mode-name "SpeedGit")
  (use-local-map speed-git-mode-map)
  (setq-local buffer-read-only t))

(defun speed-git-get-filename-at-point ()
  "Extracts the status and filename from the current line in a git status buffer.
Returns a list (STATUS FILENAME), or raises an error if no file is found.
STATUS is a string like \"deleted:\", \"modified:\", etc., or \"untracked\" for untracked files."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Matches lines like "deleted: path/to/file", "modified: path/to/file"
     ((re-search-forward "^\t\\(modified:\\|new file:\\|deleted:\\|renamed:\\|copied:\\) +\\(.*\\)$" (line-end-position) t)
      (list (match-string 1) (match-string 2)))
     ;; Matches lines for porcelain format
     ((re-search-forward "^\t\\(UU\\|AA\\|DD\\|AU\\|UA\\|DU\\|UD\\|\\?\\?\\| M\\| A\\| D\\) \\(.*\\)$" (line-end-position) t) ; for --porcelain=v1
      (list (match-string 1) (match-string 2)))
     ;; Matches lines that are just filenames (typically untracked files)
     ((re-search-forward "^\t\\([^ ]+\\)" (line-end-position) t)
      (list "untracked" (match-string 1)))
     (t
      (user-error "no file at point found")))))

(defun speed-git-run-git-command (command args)
  "Runs a git command and refreshes the speed-git buffer."
  (let ((git-command (mapconcat 'identity (cons "git" (cons command args)) " ")))
    (unless default-directory
      (error "Not in a git repository"))
    (shell-command git-command)))

(defun speed-git-get-file-infos-in-selection ()
  "Returns a list of (STATUS . FILENAME) for files in the active region,
or for the file at point if no region is active.
Ignores lines where no file is found and returns an empty list in such cases for regions,
or prints a message and returns an empty list for the point case."
  (let ((file-infos '()))
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (when (< end beg)
            (setq end beg)
            (setq beg (region-end)))
          (save-excursion
           (goto-char beg)
           (while (< (point) end)
             (condition-case nil
                 (let ((file-info (speed-git-get-filename-at-point)))
                   (when file-info (push file-info file-infos)))
               (user-error nil))        ; Ignore lines in region where no file is found
             (forward-line 1))))
      ;; No region, try to get file at point
      (condition-case nil
          (let ((file-info (speed-git-get-filename-at-point)))
            (when file-info (push file-info file-infos)))
        (user-error (message "No file at point found")))) ; Message if no file at point
    (nreverse file-infos)))

(defun speed-git-stage-file-at-point ()
  "Stages the file(s) at the current point or in the active region.
Uses 'git rm' for deleted files."
  (interactive)
  (let ((file-infos (speed-git-get-file-infos-in-selection)))
    (if file-infos
        (dolist (file-info file-infos)
          (let ((status (car file-info))
                (filename (cadr file-info)))
            (message "Processing %s (status: %s)..." filename status)
            (if (or (string= status "deleted:")
                    ;; Handle porcelain status for deleted files:
                    ;; " D" -> deleted in worktree
                    ;; "DD" -> unmerged, both deleted
                    (string= status " D")
                    (string= status "DD"))
                (speed-git-run-git-command "rm" (list filename))
              (speed-git-run-git-command "add" (list filename)))
            (message "Processed %s." filename)))
      (message "No files found at point or in region."))))

(defun speed-git-refresh ()
  "Refresh the speed git buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (let* ((output-buffer (get-buffer-create "*speed-git-status*"))
           (status-output (shell-command-to-string "git status"))
           (line (line-number-at-pos)))
      (erase-buffer)
      (insert status-output)
      (goto-line line))))

(defun speed-git-unstage-file-at-point ()
  "Unstages the file(s) at the current point or in the active region (git restore --staged)."
  (interactive)
  (let ((file-infos (speed-git-get-file-infos-in-selection)))
    (if file-infos
        (dolist (file-info file-infos)
          (let* ((filename (cadr file-info))) ;; Status not strictly needed for unstage
            (message "Unstaging %s..." filename)
            (speed-git-run-git-command "restore" (list "--staged" filename))
            (message "Unstaged %s." filename)))
      (message "No files found at point or in region."))))

(defun speed-git-diff-file-at-point ()
  "Runs git diff on the file at point."
  (interactive)
  (let ((file-infos (speed-git-get-file-infos-in-selection)))
    (if file-infos
        (dolist (file-info file-infos)
          (let ((filename (cadr file-info)))
            (message "Displaying git diff for %s..." filename)
            (let* ((diff-output (shell-command-to-string (format "git diff %s" filename)))
                   (output-buffer (get-buffer-create "*speed-git-diff*")))
              (with-current-buffer output-buffer
                (erase-buffer)
                (insert diff-output)
                (goto-char (point-min))
                (diff-mode))
              (display-buffer output-buffer))))
      (message "No files found at point or in region."))))

(defun speed-git-status ()
  "Runs git status and displays the output in a new buffer with speed-git-mode."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git"))
        (inhibit-read-only t))
    (unless default-directory
      (error "Not in a git repository"))
    (let* ((output-buffer (get-buffer-create "*speed-git-status*"))
           (status-output (shell-command-to-string "git status")))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert status-output)
        (speed-git-mode))
      (switch-to-buffer output-buffer))))

;; Example usage:
;; M-x speed-git-status

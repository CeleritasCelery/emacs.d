(defconst csh-comment-regexp "^\\s *#")
(defconst csh-label-re "^\\s *[^!#$\n ]+:")
(defconst csh-case-item-re "^\\s *\\(case .*\\|default\\):")
(defconst csh-keywords-re "^\\s *\\(else\\b\\|foreach\\b\\|if\\b.+\\(\\\\\\|\\bthen\\b\\)\\|switch\\b\\|while\\b\\)")
(defconst csh-case-default-re "^\\s *\\(case\\|default\\)\\b")
(defconst csh-if-re "^\\s *if\\b.+\\(\\\\\\|\\bthen\\b\\)")
(defconst csh-else-re "^\\s *\\belse\\(\\b\\|$\\)")
(defconst csh-else-if-re "^\\s *\\belse if\\(\\b\\|$\\)")
(defconst csh-endif-re "^\\s *endif\\b")
(defconst csh-end-re "^\\s *end\\b")
(defconst csh-endsw-re "^\\s *endsw\\b")
(defconst csh-switch-re "^\\s *switch\\b")
(defconst csh-iteration-keywords-re "^[^#\n]*\\s\"*\\b\\(while\\|foreach\\)\\b")
(defconst csh-multiline-re "^.*\\\\$")


(defvar csh-indent 4)
(defvar csh-case-item-offset csh-indent
  "Additional indentation for case items within a case statement.")
(defvar csh-match-and-tell t
  "If non-nil echo in the minibuffer the matching compound command
for the \"breaksw\", \"end\", or \"endif\".")


(defun csh-indent-line ()
  "Indent current line as far as it should go according
to the syntax/context"
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (unless (bobp)
        ;; Align this line to current nesting level
        (let* ((level-list (csh-get-nest-level)) ; Where to nest against
               (this-line-level (current-indentation))
               (nester-column (csh-get-nester-column (cdr level-list)))
               (struct-match (csh-match-structure-and-reindent)))
          (when struct-match
            (setq nester-column struct-match))
          (unless (eq nester-column this-line-level)
            (beginning-of-line)
            (let ((beg (point)))
              (back-to-indentation)
              (delete-region beg (point)))
            (indent-to nester-column)))))
    ;; Position point on this line
    (let* ((this-line-level (current-indentation))
           (this-bol (save-excursion
                       (beginning-of-line)
                       (point)))
           (this-point (- (point) this-bol)))
      (when (> this-line-level this-point);; point in initial white space
        (back-to-indentation)))))

(defun csh-indent-region (start end)
  "From start to end, indent each line."
  ;; The algorithm is just moving through the region line by line with
  ;; the match noise turned off.  Only modifies nonempty lines.
  (save-excursion
    (let (csh-match-and-tell
          (endmark (copy-marker end)))
      (goto-char start)
      (beginning-of-line)
      (setq start (point))
      (while (> (marker-position endmark) start)
        (unless (and (bolp) (eolp))
          (csh-indent-line))
        (forward-line 1)
        (setq start (point)))
      (set-marker endmark nil))))

(defun csh-match-indent-level (begin-re end-re)
  "Match the compound command and indent. Return nil on no match,
indentation to use for this line otherwise."
  (interactive)
  (let* ((case-fold-search)
         (nest-list
          (save-excursion
            (csh-get-compound-level begin-re end-re (point)))))
    (if nest-list
        (let* ((nest-level (car nest-list))
               (match-line (cdr nest-list)))
          (when csh-match-and-tell
            (save-excursion
              (goto-line match-line)
              (message "Matched ... %s" (csh-line-to-string))))
          nest-level)
      (when csh-match-and-tell
        (message "No matching compound command"))
      nil))) ;; Propagate a miss.

(defun csh-get-nest-level ()
  "Return a 2 element list (nest-level nest-line) describing where the
current line should nest."
  (let (case-fold-search level)
    (save-excursion
      (forward-line -1)
      (while (not (or (bobp) level))
        (if (not (or (looking-at "^\\s *$")
                     (save-excursion
                       (forward-line -1)
                       (beginning-of-line)
                       (looking-at csh-multiline-re))
                     (looking-at csh-comment-regexp)))
            (setq level (cons (current-indentation)
                              (csh-current-line)))
          (forward-line -1)))
      (if level level (cons (current-indentation) (csh-current-line))))))

(defun csh-current-line ()
  "Return the vertical position of point in the buffer.
Top line is 1."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun csh-get-nester-column (nest-line)
  "Return the column to indent to with respect to nest-line taking
into consideration keywords and other nesting constructs."
  (save-excursion
    (let ((fence-post)
          (case-fold-search)
          (start-line (csh-current-line)))
      ;; Handle case item indentation constructs for this line
      (cond ((looking-at csh-case-item-re)
             ;; This line is a case item...
             (save-excursion
               (goto-line nest-line)
               (let ((fence-post (save-excursion (end-of-line) (point))))
                 (cond ((re-search-forward csh-switch-re fence-post t)
                        ;; If this is the first case under the switch, indent.
                        (goto-char (match-beginning 0))
                        (+ (current-indentation) csh-case-item-offset))
                       ((re-search-forward csh-case-item-re fence-post t)
                        ;; If this is another case right under a previous case
                        ;; without intervening code, stay at the same
                        ;; indentation.
                        (goto-char (match-beginning 0))
                        (current-indentation))
                       (t
                        ;; Else, this is a new case.  Outdent.
                        (- (current-indentation) csh-case-item-offset))))))
            (t;; Not a case-item.  What to do relative to the nest-line?
             (save-excursion
               (goto-line nest-line)
               (setq fence-post (save-excursion (end-of-line) (point)))
               (save-excursion
                 (cond
                  ;; Check if we are in a continued statement
                  ((and (looking-at csh-multiline-re)
                        (save-excursion
                          (goto-line (1- start-line))
                          (looking-at csh-multiline-re)))
                   (if (looking-at ".*[\'\"]\\\\")
                       ;; If this is a continued string, indent under
                       ;; opening quote.
                       (progn
                         (re-search-forward "[\'\"]")
                         (forward-char -1))
                     (if (looking-at ".*([^\)\n]*\\\\")
                         ;; Else if this is a continued parenthesized
                         ;; list, indent after paren.
                         (re-search-forward "(" fence-post t)
                       ;; Else, indent after whitespace after first word.
                       (re-search-forward "[^ \t]+[ \t]+" fence-post t)))
                   (current-column))

                  ;; In order to locate the column of the keyword,
                  ;; which might be embedded within a case-item,
                  ;; it is necessary to use re-search-forward.
                  ;; Search by literal case, since shell is
                  ;; case-sensitive.
                  ((re-search-forward csh-keywords-re fence-post t)
                   (goto-char (match-beginning 1))
                   (if (looking-at csh-switch-re)
                       (+ (current-indentation) csh-case-item-offset)
                     (+ (current-indentation)
                        (if (null csh-indent)
                            2 csh-indent))))
                  ((re-search-forward csh-case-default-re fence-post t)
                   (if csh-indent
                       (progn
                         (goto-char (match-beginning 1))
                         (+ (current-indentation) csh-indent))
                     (goto-char (match-end 1))
                     (+ (current-indentation) 1)))
                  ;; Now detect first statement under a case item
                  ((looking-at csh-case-item-re)
                   (if csh-case-indent
                       (+ (current-indentation) csh-case-indent)
                     (re-search-forward csh-case-item-re fence-post t)
                     (goto-char (match-end 1))
                     (+ (current-column) 1)))
                  ;; If this is the first statement under a control-flow
                  ;; label, indent one level.
                  ((csh-looking-at-label)
                   (+ (current-indentation) csh-indent))
                  ;; This is hosed when using current-column
                  ;; and there is a multi-command expression as the
                  ;; nester.
                  (t (current-indentation))))))))))

(defun csh-looking-at-label ()
  "Return true if current line is a label (not the default: case label)."
  (and (looking-at csh-label-re)
       (not (looking-at "^\\s *default:"))))

(defun csh-match-structure-and-reindent ()
  "If the current line matches one of the indenting keywords
or one of the control structure ending keywords then reindent. Also
if csh-match-and-tell is non-nil the matching structure will echo in
the minibuffer"
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at csh-else-re)
             (csh-match-indent-level csh-if-re csh-endif-re))
            ((looking-at csh-else-if-re)
             (csh-match-indent-level csh-if-re csh-endif-re))
            ((looking-at csh-endif-re)
             (csh-match-indent-level csh-if-re csh-endif-re))
            ((looking-at csh-end-re)
             (csh-match-indent-level csh-iteration-keywords-re csh-end-re))
            ((looking-at csh-endsw-re)
             (csh-match-indent-level csh-switch-re csh-endsw-re))
            ((csh-looking-at-label)
             ;; Flush control-flow labels left since they don't nest.
             0)))))



(defun csh-get-compound-level
    (begin-re end-re anchor-point &optional balance-list)
  "Determine how much to indent this structure. Return a list (level line)
of the matching compound command or nil if no match found."
  (let* ((match-point (if (re-search-backward begin-re (point-min) t)
                          (match-beginning 0) 0))
         ;; Locate the next compound begin keyword bounded by point-min
         (nest-column (if (zerop match-point)
                          1
                        (goto-char match-point)
                        (current-indentation)))
         (nest-list (cons 0 0)))    ;; sentinel cons since cdr is >= 1
    (unless (zerop match-point)
      (when (nlistp balance-list)
        (setq balance-list (list)))
      ;; Now search forward from matching start keyword for end keyword
      (while (and (consp nest-list)
                  (zerop (cdr nest-list))
                  (re-search-forward end-re anchor-point t))
        (unless (memq (point) balance-list)
          (setq balance-list (cons (point) balance-list))
          (goto-char match-point)  ;; beginning of compound cmd
          (setq nest-list
                (csh-get-compound-level begin-re end-re
                                        anchor-point balance-list))))
      (when (consp nest-list)
        (if (zerop (cdr nest-list))
            (progn
              (goto-char match-point)
              (cons nest-column (csh-current-line)))
          nest-list)))))

(defun csh-line-to-string ()
  "From point, construct a string from all characters on
current line"
  (skip-chars-forward " \t") ;; skip tabs as well as spaces
  (buffer-substring (point) (progn (end-of-line 1) (point))))

(provide 'csh-indent-function)

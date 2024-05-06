;;; -*- lexical-binding: t; -*-

(defun how-to (query)
  (interactive "sQuestion: ")
  (let* ((max-questions 2)
         (question_ids
          (with-current-buffer
              (url-retrieve-synchronously
               (concat "https://google.com/search?ie=utf-8&oe=utf-8&hl=en&as_qdr=all&q="
                       (url-hexify-string (concat query " site:stackoverflow.com"))))
            (let (ids)
              (while (re-search-forward "https://stackoverflow.com/questions/\\([0-9]+\\)" nil t)
                (push (match-string-no-properties 1) ids))
              (setq ids (reverse ids))
              (if (> (length ids) max-questions)
                  (subseq ids 0 max-questions)
                ids))))

         (url_template (format "https://api.stackexchange.com/2.2/questions/%s%%s?site=stackoverflow.com"
                               (string-join question_ids ";")))

         (questions (with-current-buffer
                        (url-retrieve-synchronously
                         (format url_template ""))
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (append (assoc-default 'items (json-read)) nil)))

         (answers (with-current-buffer
                      (url-retrieve-synchronously
                       (concat (format url_template "/answers")
                               "&order=desc&sort=activity&filter=withbody"))
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (sort (append (assoc-default 'items (json-read)) nil)
                          (lambda (x y)
                            (> (assoc-default 'score x)
                               (assoc-default 'score y))))))
         (language (cond ((string-match-p "python" query) "python")
                         ((string-match-p "perl" query) "perl")
                         ((string-match-p "tcl" query) "tcl")
                         ((string-match-p "bash" query) "bash")
                         ((string-match-p "rust" query) "rust")
                         ((string-match-p "lisp" query) "lisp")
                         (t ""))))

    (switch-to-buffer "*stackexchange*")
    (erase-buffer)

    (dolist (question_id (mapcar 'string-to-number question_ids))
      (let ((question (some (lambda (question)
                              (if (equal (assoc-default 'question_id question)
                                         question_id)
                                  question))
                            questions)))
        (insert "* Question: "
                (format "%s<br>[[%s][link]]<br>"
                        (assoc-default 'title question)
                        (assoc-default 'link question))
                "\n"
                (mapconcat
                 'identity
                 (let ((rendered
                        (remove-if
                         'null
                         (mapcar (lambda (answer)
                                   (if (and (equal question_id
                                                   (assoc-default 'question_id answer))
                                            (>= (assoc-default 'score answer) 0))
                                       (concat "<br>** Answer - score: "
                                               (number-to-string (assoc-default 'score answer))
                                               (thread-last (assoc-default 'body answer)
                                                 (replace-regexp-in-string "<pre><code>" (format "<br>#+begin_src %s<br><pre><code>" language))
                                                 (replace-regexp-in-string "</code></pre>" "</code></pre>#+end_src<br>")
                                                 (replace-regexp-in-string ($rx "<code>" (group (+ (not (in "<>\n")))) "</code>") "~\\1~")))))
                                 answers))))
                   (if (> (length rendered) 5)
                       (append (subseq rendered 0 5)
                               (list (format "<br><br><a href='%s'>%s</a>"
                                             (assoc-default 'link question)
                                             "More answers...")))
                     rendered))
                 "\n"))))
    (shr-render-region (point-min) (point-max))
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "^M" nil t)
        (replace-match "")))
    (org-mode)))

($leader-set-key "so" 'how-to)

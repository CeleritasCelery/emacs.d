;; init.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2024  Troy Hinckley

;;; Bootstrap
;; https://github.com/progfolio/elpaca#installer
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Configuration

;; I have some basic design principles that I am trying to keep
;; consistent through all of my config.

;; *** namespace
;; I am using =$= as my personal namespace. I like it because it reminds
;; me of perl, it is really simple, and this is elisp, so I can make my
;; namespace whatever I want.

;; *** lambdas
;; the easiest way add a simple wrapper function to a hook or advice is
;; to use lambdas. However that has the problem of making them very hard
;; to inspect and remove. It is better to use named functions. However it
;; can be confusing if a function is only used as a named lambda or if it
;; is being used elsewhere. Therefore I will put named functions (using
;; =defun=) inside of =add-hook= or =advice-add=. This allows me to
;; clearly associate the function with the purpose but also avoids the
;; confusion surrounding anonymous functions. However unless it is
;; necessary, I am not going to add a doc string to these named lambda
;; functions.

;; *** overrides
;; Never override a function when an advice will do. when I actually do
;; need to override a function, I will try to use the package =el-patch=
;; to make it maintainable.

;;; Emacs Initialization

(setq user-full-name "Troy Hinckley")
(setenv "LSP_USE_PLISTS" "true")

;;;; Customization

(defmacro csetq (&rest pairs)
  "For each SYMBOL VALUE pair, calls either `custom-set' or `set-default'."
  (let (forms)
    (while pairs
      (let ((variable (pop pairs))
            (value (pop pairs)))
        (push `(funcall (or (get ',variable 'custom-set) 'set-default)
                        ',variable ,value)
              forms)))
    `(progn ,@(nreverse forms))))

(defun $dev-config-p ()
  (equal user-login-name "thinckley"))

(setq epg-pinentry-mode 'loopback)

;;;; Settings

(defvar $leader-key "SPC"
  "leader key used to quicky access commands.")

(defvar $mm-leader-key ","
  "leader key for major mode specific commands")

(setq inhibit-startup-screen t)

(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

(electric-pair-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

(csetq load-prefer-newer t)

(setq native-comp-async-report-warnings-errors nil)

(setq bidi-inhibit-bpa t
      bidi-paragraph-direction 'left-to-right)

(setq auto-window-vscroll nil)

(setq display-raw-bytes-as-hex t)

(setq ring-bell-function 'ignore)

(require 'cl)

(setq initial-major-mode 'fundamental-mode)

;;;; Environment
(push "~/bin" exec-path)
(push "~/.local/bin" exec-path)
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path "/home/thinckley/.local/bin"))

(setenv "PAGER" "cat")

(add-hook 'elpaca-after-init-hook 'server-start)

;;;; Site

(let ((site-file (expand-file-name "site/site.el" user-emacs-directory)))
  (when (file-exists-p site-file)
    (load-file site-file)))

(setq mac-command-modifier 'control)

;;; Package manager

;; Elpaca setup
(elpaca elpaca-use-package
        ;; Enable Elpaca support for use-package's :ensure keyword.
        (elpaca-use-package-mode))

;; Make sure use-package uses elpaca
(setq use-package-always-ensure t
      use-package-always-defer t)

;;; Startup

(use-package esup)

(use-package bug-hunter)

(use-package compdef
  :ensure (:wait t)
  :demand t)

;;;; GC hack

(defvar $gc-timer nil)
(defvar $use-gc-timer t)
(defun $maybe-gc ()
  (setq gc-cons-threshold 800000)
  (setq $gc-timer (run-with-timer 5 nil #'$schedule-maybe-gc))
  (setq gc-cons-threshold 100000000))

(defun $schedule-maybe-gc ()
  (if $use-gc-timer
      (setq $gc-timer (run-with-idle-timer 3 nil #'$maybe-gc))
    (setq gc-cons-threshold 800000
          $gc-timer nil)))

;; disable for now
;; (add-hook 'elpaca-after-init-hook '$schedule-maybe-gc)
(setq gc-cons-threshold 8000000)

(add-function :after
                  after-focus-change-function
                  (lambda () (unless (frame-focus-state) (garbage-collect))))

;;;; Windows

(defun $default-display-function (buffer alist)
  "Display the window and select it"
  (cl-loop for fn in (car display-buffer-fallback-action)
           if (funcall fn buffer alist)
           return (select-window (get-buffer-window buffer))))

(defun $select-popup-window-p (buffer _action)
  (provided-mode-derived-p
   (buffer-local-value 'major-mode (get-buffer buffer))
   'compilation-mode 'flycheck-error-list-mode))

(add-to-list 'display-buffer-alist '($select-popup-window-p $default-display-function))

;;; Keybindings

(use-package general
  :ensure (:wait t)
  :demand t)

(general-create-definer $leader-set-key
  :prefix $leader-key
  :states 'motion
  :keymaps 'override)

(general-create-definer $leader-local-set-key
  :prefix $mm-leader-key
  :states 'motion)

(defun general-leader-define-key (_state keymap key def _orig-def _kargs)
  "define a new key based on leader"
  (if (eq keymap 'global)
      (eval `($leader-set-key ,key ',def))
    (eval `($leader-local-set-key :keymaps ',keymap ,key ',def))))

(defalias 'use-package-handler/:keys 'use-package-handler/:general)
(defalias 'use-package-normalize/:keys 'use-package-normalize/:general)
(add-to-list 'use-package-keywords :keys)

(use-package no-littering
  :demand t
  :config
  ;; These paths need to be relative to the home directory, but no-littering makes them absolute
  ;; paths.
  (setq detached-db-directory      "~/.emacs.d/var/detached/db/")
  (setq detached-session-directory "~/.emacs.d/var/detached/sessions/"))


(use-package savehist
  :ensure nil
  :defer 1
  :config
  (add-to-list 'savehist-additional-variables 'read-expression-history)
  (savehist-mode))

(use-package el-patch)

(use-package which-key
  :demand t
  :init
  (setq which-key-allow-evil-operators t)
  :config
  (which-key-mode)
  (push '((nil . "\\$") . (nil . "")) which-key-replacement-alist))

;;; Early Packages
;;;; Evil

(use-package evil
  :demand t
  :general
  ('evil-ex-completion-map
   "C-f" nil
   "C-b" nil
   "C-o" 'evil-ex-command-window)
  ('evil-ex-search-keymap
   "C-f" nil
   "C-o" 'evil-ex-search-command-window)
  :custom
  (evil-want-C-i-jump nil)
  (evil-symbol-word-search t "Using * and #, search foward for symbols, not words")
  (evil-ex-substitute-global t)
  (evil-want-abbrev-expand-on-insert-exit nil "Don't try abbrev expand on exit. Causes real issues in verilog mode")
  (evil-ex-search-vim-style-regexp t "use vim style regexp because it is shorter")
  (evil-magic 'very-magic "enable the full power of vim regexp")
  (evil-v$-excludes-newline t "make v$y behave like y$")
  :init
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  :config
  (general-swap-key nil 'motion "0" "^")
  ;; not sure why I can't use :custom for these, but they don't work
  (csetq evil-want-Y-yank-to-eol t
         evil-search-module 'evil-search)
  (evil-mode 1))

(general-def '(normal visual)
  "C-f" 'evil-scroll-down
  "C-b" 'evil-scroll-up)

(general-def 'motion
  [remap evil-next-line] 'evil-next-visual-line
  [remap evil-previous-line] 'evil-previous-visual-line)

(general-def 'operator
  [remap evil-next-line] 'evil-next-line
  [remap evil-previous-line] 'evil-previous-line)

(with-eval-after-load 'evil
  (defun forward-evil-word (&optional count)
    (evil-forward-nearest
     count
     #'(lambda (&optional cnt)
         (let ((word-separating-categories evil-cjk-word-separating-categories)
               (word-combining-categories evil-cjk-word-combining-categories)
               (pnt (point)))
           (forward-word cnt)
           (if (= pnt (point)) cnt 0)))
     #'(lambda (&optional cnt)
         (evil-forward-chars "^[:word:]\n\r\t\f ._/-" cnt))
     #'forward-evil-empty-line)))

(advice-add 'evil-mouse-drag-region :after
            (defun $fix-miss-drag (&rest _x)
              (when (region-active-p)
                (cl-destructuring-bind (beg . end) (car (region-bounds))
                  (when (> 4 (- end beg))
                    (evil-normal-state))))))

(advice-add 'mouse-set-region :after 'deactivate-mark)

(general-add-hook '(evil-visual-state-entry-hook evil-insert-state-entry-hook)
                  (defun $disable-hl-line ()
                    (global-hl-line-mode -1)))

(general-add-hook '(evil-visual-state-exit-hook evil-insert-state-exit-hook)
                  (defun $enable-hl-line ()
                    (global-hl-line-mode)))

;;;; Ivy

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer))
  :general
  (ivy-minibuffer-map
   "C-h" "DEL"
   "C-w" 'ivy-backward-kill-word
   "C-S-H" help-map
   "C-l" 'ivy-alt-done
   "<C-return>" 'ivy-immediate-done
   [mouse-1] 'ignore
   [mouse-2] 'ignore
   [mouse-3] 'ignore)
  (ivy-reverse-i-search-map
   "C-k" 'ivy-previous-line)
  (ivy-switch-buffer-map
   "C-k" 'ivy-previous-line
   "C-d" 'ivy-switch-buffer-kill)
  ("C-x r b" 'counsel-bookmark
   "C-x C-r" 'ivy-resume)
  (ivy-occur-grep-mode-map
   "SPC" nil)
  (minibuffer-local-map
   "C-c C-l" 'counsel-minibuffer-history)
  ("C-x C-b" 'ivy-switch-buffer)
  :init
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'abbreviate
        ivy-extra-directories nil
        ivy-use-selectable-prompt t
        ivy-count-format "%d/%d "
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  :config
  (ivy-mode)
 ;; don't resort my functions
  (setq ivy-sort-matches-functions-alist '((t))))

(setq ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)
                                       (org-mode . org-level-8)))

(use-package hydra
  :ensure (:wait t))
(use-package ivy-hydra
  :after (ivy hydra))

(with-eval-after-load 'counsel
  (ivy-add-actions
   t
   '(("y" $ivy-yank "yank" $ivy-yank-all)))
  (ivy-add-actions
   'counsel-find-file
   '(("g" $magit-status-in-dir "git status")
     ("d" $async-delete-file "delete")
     ("y" $yank-file-name "yank" $yank-file-name-list)
     ("s" (lambda (x) (counsel-rg nil x)) "search")
     ("f" $ivy-file-jump "find")
     ("o" find-file-other-window "other window")
     ("x" (lambda (x) ($counsel-shell-pop ivy-current-prefix-arg nil x)) "shell")
     ("j" (lambda (x) (let ((default-directory x)) (counsel-git))) "jump"))))

(defun $ivy-yank (x)
  (kill-new
   (if (consp x)
       (car x)
     x)))

(defun $ivy-file-jump (x)
  (let ((args (split-string x)))
    (counsel-fd-jump (cdr args) (car args))))

(defun $ivy-yank-all (x)
  ($ivy-yank (mapconcat 'identity x "\n")))

(defun $yank-file-name (x)
  (let ((file ($correct-file-path x)))
    (kill-new (string-remove-prefix (or (file-remote-p x) "") x))))

(defun $yank-file-name-list (x)
  (kill-new
   (mapconcat
    (lambda (f)
      ($correct-file-path (expand-file-name f ivy--directory)))
    x "\n")))

(general-def ivy-minibuffer-map ";" 'ivy-dispatching-done)

(use-package swiper
  :general
  ("C-s" 'swiper)
  :config
  (ivy-configure 'swiper-isearch
    :display-fn 'ivy-display-function-window)
  (ivy-configure 'swiper
    :display-fn 'ivy-display-function-window))

(defun ivy-display-function-window (text)
  (let ((buffer (get-buffer-create "*ivy-candidate-window*"))
        (str (with-current-buffer (window-buffer (active-minibuffer-window))
               (let ((point (point))
                     (string (concat (buffer-string) "  " text)))
                 (add-face-text-property
                  (- point 1) point 'ivy-cursor t string)
                 string))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)))
    (with-ivy-window
      (display-buffer
       buffer
       `((display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . ,(1+ (ivy--height (ivy-state-caller ivy-last)))))))))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-find-file)
         ("C-x C-j" . counsel-git)
         ("C-x j" . counsel-git)
         ("C-c s" . counsel-ag)
         ("M-x" . counsel-M-x))
  :general
  (:definer 'leader
   "T" 'counsel-load-theme)
  :init
  (setq counsel-find-file-ignore-regexp (rx (or (: bos (any "#.")) (: (any "#~") eos)))
        counsel-bookmark-avoid-dired t)
  :config
  ;; adding --search-zip can cause the PCRE engine to hit it's line limit. add `-- -z` to search zip files
  (setq counsel-rg-base-command (append counsel-rg-base-command '("--max-columns-preview")))
  (ivy-configure 'counsel-company
    :display-fn 'ivy-display-function-overlay)
  (setq ivy-initial-inputs-alist nil))

($leader-local-set-key
  :keymaps 'org-mode-map
  "j" 'counsel-org-goto)

(defun $counsel-rg-here ()
  (interactive)
  (counsel-rg nil default-directory))

(defun $counsel-rg-root ()
  (interactive)
  (counsel-rg nil ($model-root)))

(defun counsel-fd-jump (&optional initial-input initial-directory)
  (interactive)
  (let ((default-directory (or initial-directory default-directory)))
    (ivy-read "Jump file: "
              (counsel--call (list (or (executable-find "fd" t)
                                       (executable-find "fdfind" t))
                                   "--no-ignore" "--hidden" "--exclude" ".git" "--type" "f")
                             (lambda () (split-string (buffer-string) "\n")))
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action #'find-file
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :caller 'counsel-file-jump)))

(use-package smex)

(defvar $counsel-git-cands-cache nil)
(defun $memoize-counsel-git-cands (orig dir)
  ($memoize-remote (vc-git-root dir) '$counsel-git-cands-cache orig dir))

(defun $clear-counsel-git-cands-cache ()
  (interactive)
  (setq $counsel-git-cands-cache nil))

(advice-add 'counsel-git-cands :around #'$memoize-counsel-git-cands)

;;; Display

(setq shr-use-colors nil
      shr-bullet "• ")

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode))

(defvar $font-height 140)
(set-face-attribute 'default nil
                    :family (if (eq system-type 'windows-nt)
                                "Consolas"
                              "Source Code Pro")
                    :height $font-height)
(set-fontset-font t nil "Symbola" nil 'append)

(defun $toggle-large-font ()
  "Toggle between normal and large font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (if (< 200 (face-attribute 'default :height))
       $font-height
     220)))

(defhydra text-scale (:hint nil)
  "
Text Scale
  _i_n _o_ut _s_cale _r_eset _q_uit
"
  ("i" text-scale-increase)
  ("o" text-scale-decrease)
  ("s" (text-scale-set 3) :exit t)
  ("r" (text-scale-set 0) :exit t)
  ("q" nil :exit t))
($leader-set-key
  "z" '(:ignore t :wk "util")
  "zs" 'text-scale/body)

;;;; Line numbers

(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(defun $toggle-absolute-line-numbers ()
  (interactive)
  (setq-local display-line-numbers-type
              (if (eq t display-line-numbers-type)
                  'relative t))
  ;; toggle the minor mode
  (display-line-numbers-mode -1)
  (display-line-numbers-mode))

($leader-set-key
  "tz" '$toggle-large-font
  "tN" '$toggle-absolute-line-numbers)

;;;; Ligatures

(setq $fira-ligature-alist
      '(("&&"   . #xEF3B)
        ("||"   . #xEF3C)
        ("::"   . #xEF07)
        ("=="   . #xEF4C)
        ("->"   . #xEF15)
        ("=>"   . #xEF4F)
        ("/*"   . #xEF32)
        ("*/"   . #xEF03)
        (">>"   . #xEF58)
        ("<<"   . #xEF72)
        (".."   . #xEF28)
        ("__"   . #xEF39)
        ("~~"   . #xEF7F)
        ("++"   . #xEF47)
        ("!="   . #xEF0F)
        (".="   . #xEF27)
        ("=~"   . #xEF83)
        ("!~"   . #xEF84)
        (";;"   . #xEF31)
        ("##"   . #xEF1E)
        ("#!"   . #xEF1D)
        ("//"   . #xEF36)
        (":="   . #xEF0A)
        ("?="   . #xEF2E)
        ("?:"   . #xEF2D)
        ("<="   . #xEF91)
        (">="   . #xEF90)
        ("</"   . #xEF79)
        ("/>"   . #xEF35)
        ("</>"  . #xEF7A)
        ("///"  . #xEF37)
        ("==="  . #xEF4D)
        ("!=="  . #xEF10)
        ("<=>"  . #xEF6F)
        ("..."  . #xEF2B)
        ("->>"  . #xEF16)
        ("-->"  . #xEF14)
        ("<--"  . #xEF67)
        ("|->"  . #xEF8C)
        ("|=>"  . #xEF8D)
        ("<<<"  . #xEF75)
        (">>>"  . #xEF5B)
        ("###"  . #xEF1F)
        ("####" . #xEF20)
        ("<!--" . #xEF65)
        ("\\\\" . #xEF85)))

(set-fontset-font t '(#xEF00 . #xEFFF) "Fira Code Extended")

(defun $make-ligature-glyph (str glyph)
  (if (or (listp glyph)
          (eq 1 (string-width str)))
      glyph
    `(,@(mapcan (lambda (x) (list ?\s '(Br . Bl)))
                (number-sequence 2 (string-width str)))
      ?\s (Br . Br) ,(decode-char 'ucs glyph))))

(defun $set-ligature (symbol)
  (cl-destructuring-bind (str . glyph) symbol
    (setf (alist-get str prettify-symbols-alist nil nil 'equal)
          ($make-ligature-glyph str glyph))))

(defun $prettify-base-symbols ()
  "enable fira code ligatures"
  (interactive)
  (mapc '$set-ligature $fira-ligature-alist)
  (prettify-symbols-mode))

(add-hook 'prog-mode-hook '$prettify-base-symbols)

;; compose symbols (ligatures) no matter where they are. also unformat at
;; point so we can easily see the representation
(csetq prettify-symbols-unprettify-at-point t
       prettify-symbols-compose-predicate '$prettify-symbols-all-p)

(defun $prettify-symbols-all-p (start end match)
  (not (or (eq (char-before start) (char-after start))
           (eq (char-before end) (char-after end))
           (and (member match '("//" "/*"))
                (not (nth 4 (syntax-ppss)))) ;; inside comment
           (and (equal match "*/")
                (not (nth 4 (syntax-ppss (1- (point)))))) ;; inside comment
           (and (equal match "..")
                (or (eq (char-before start) ?/)
                    (eq (char-after end) ?/)))
           (and (equal match "=~")
                (eq (char-after end) ?/))
           (and (equal match ">=")
                (eq (char-after end) ?<)))))

;; fix issue where html tags are not highlighted properly when using ligatures.
(add-hook 'html-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("/>" . 'rainbow-delimiters-depth-1-face)))))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;;;; Themes

(setq custom--inhibit-theme-enable nil)

(use-package darktooth-theme
  :ensure (:host github :repo "CeleritasCelery/emacs-theme-darktooth"))
(use-package challenger-deep-theme)
(use-package dracula-theme)
(use-package gruvbox-theme)
(use-package spacemacs-theme)
(use-package moe-theme)
(use-package doom-themes)
(use-package solarized-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package noctilux-theme)
(use-package flatland-theme)
(use-package monokai-theme)
(use-package twilight-anti-bright-theme)
(use-package twilight-bright-theme)
(use-package ef-themes :defer t)
(use-package afternoon-theme)

(add-hook 'elpaca-after-init-hook (defun $load-theme () (load-theme 'darktooth t)))

(use-package rainbow-mode
  :init
  (setq rainbow-x-colors nil))

(use-package fontify-face)

(define-minor-mode $color-mode
  "turn on rainbow and fontify-face modes"
  :group '$color-mode
  (if $color-mode
      (progn (rainbow-mode)
             (fontify-face-mode))
    (rainbow-mode -1)
    (fontify-face-mode -1)))

;;;; Modeline

(use-package doom-modeline
  :hook elpaca-after-init
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-env-version nil)
  (doom-modeline-github nil)
  (doom-modeline-major-mode-color-icon t)
  :config
  (setq eldoc-eval-preferred-function 'eval-expression)
  (remove-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
  (remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
  (doom-modeline-def-segment buffer-encoding-simple
    (propertize
     (concat (pcase (coding-system-eol-type buffer-file-coding-system)
               (1 " CRLF")
               (2 " CR"))
             (let ((sys (coding-system-plist buffer-file-coding-system)))
               (unless (or (memq (plist-get sys :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (eq (plist-get sys :name) 'no-conversion))
                 (upcase (symbol-name (plist-get sys :name))))))
     'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
     'help-echo 'mode-line-mule-info-help-echo
     'mouse-face '(:box 0)
     'local-map mode-line-coding-system-map))
  (doom-modeline-def-modeline 'custom
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info debug lsp minor-modes buffer-encoding-simple major-mode process vcs))
  (add-hook 'doom-modeline-mode-hook
            (defun $custom-doom-modeline ()
              (doom-modeline-set-modeline 'custom 'default)))
  (column-number-mode))

(use-package all-the-icons)

;;; General Packages

;;;; Windows and Buffers

(use-package ace-window
  :general
  ("M-u" 'ace-window)
  :init
  (setq aw-dispatch-always t
        aw-background nil)
  :config
  (add-to-list 'aw-dispatch-alist '(?w $toggle-maximize-window))
  (add-to-list 'aw-dispatch-alist '(?d aw-delete-window "delete window"))
  (add-to-list 'aw-dispatch-alist '(?s aw-split-window-horz "Split Horz window")))

;; from https://gist.github.com/3402786
(defun $toggle-maximize-window ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun $alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun $quit-emacs ()
  "save buffers and quit"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun $show-and-copy-buffer-filename (arg)
  "Show and copy the full path to the current file in the minibuffer."
  (interactive "P")
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let ((file-name (or (buffer-file-name)
                          list-buffers-directory
                          default-directory)))
      (progn (kill-new (string-remove-prefix
                        (or (file-remote-p file-name) "")
                        ($correct-file-path file-name arg)))
             (message (current-kill 0)))
    (error "Buffer not visiting a file")))

(defun $correct-file-path (file &optional abs-path)
  "If file is in a work disk, get the absolute path.
If INVERT, do the opposite of the normal behavior."
  (let* ((remote (file-remote-p file))
         (home (expand-file-name
                (concat remote "~")))
         (prefix (concat home "/workspace")))
    (if (string-prefix-p prefix file)
        (setq file (string-replace
                    prefix
                    (file-truename prefix)
                    file)))
    (if abs-path
        (file-truename file)
      file)))

($leader-set-key
  "TAB" '$alternate-buffer
  "SPC" '$alternate-buffer
  "fy" '$show-and-copy-buffer-filename
  "q" '(:ignore t :wk "quit")
  "qq" '$quit-emacs)

(defhydra buffer-nav (:exit nil)
  "move quickly through recent buffers"
  ("p" previous-buffer "prev")
  ("N" previous-buffer "prev")
  ("n" next-buffer "next"))

($leader-set-key
  "b" '(:ignore t :wk "buffers")
  "bp" 'buffer-nav/previous-buffer
  "bn" 'buffer-nav/next-buffer)

(general-def "C-x k" 'kill-this-buffer)

(setq initial-scratch-message ";; scratch buffer -*- lexical-binding: t -*-\n")

(defun $open-scratch-buffer ()
  "open the scratch buffer"
  (interactive)
  (set-window-buffer
   (selected-window)
   (get-buffer-create "*scratch*")))

(general-def "C-c b" '$open-scratch-buffer)

($leader-set-key
  "bs" '$open-scratch-buffer)

(defun $switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

($leader-set-key
  "bm" '$switch-to-minibuffer-window)

(use-package rotate
  :general
  (:definer 'leader
   "bw" '$rotate-layout/rotate-layout))

(defhydra $rotate-layout (:hint nil)
  ("w" rotate-layout))

(use-package winum
  :defer 1
  :init
  (dolist (num (number-sequence 0 9))
    (let ((str (number-to-string num)))
      (eval `($leader-set-key
               ,str (intern (concat "winum-select-window-" ,str))))))
  :config
  (add-to-list 'winum-assign-functions
               (defun $winum-use-ace-window-numbering ()
                 (require 'ace-window)
                 (when-let ((windows (cl-sort (winum--window-list) 'aw-window<))
                            (pos (cl-position (selected-window) windows)))
                   (1+ pos))))
  (winum-mode))

(use-package zoom
  :general
  (:definer 'leader "zw" 'zoom))

(use-package helpful
  :ensure (:repo "CeleritasCelery/helpful")
  :general ("C-h k" 'helpful-key)
  :init
  (setq helpful-hide-docstring-in-source t))

(setq help-window-select t)

(csetq counsel-describe-variable-function 'helpful-variable
       counsel-describe-function-function 'helpful-callable)

(setq find-function-C-source-directory "~/Library/Caches/Homebrew/emacs-plus@29--git/src")

(general-def
  "C-h f" 'counsel-describe-function
  "C-h v" 'counsel-describe-variable
  "C-h x" 'describe-char)

($leader-set-key
  "t" '(:ignore t :wk "toggle")
  "tn" 'display-line-numbers-mode
  "tl" 'toggle-truncate-lines
  "te" 'toggle-debug-on-error
  "tq" 'toggle-debug-on-quit
  "tg" 'git-gutter-mode)

(defun $toggle-debug-on-signal ()
  "Used when debugging something wrapped in a condition-case"
  (interactive)
  (if debug-on-signal
      (progn (message "debug on signal disabled")
             (setq debug-on-signal nil))
    (message "debug on signal enabled")
    (setq debug-on-signal t)))

($leader-set-key
  "ts" '$toggle-debug-on-signal)

(when (version< emacs-version "29.1")
  (use-package restart-emacs))
($leader-set-key
  "qr" 'restart-emacs)

(setq confirm-kill-emacs 'y-or-n-p
      confirm-kill-processes nil)

(general-def
  "<XF86AudioLowerVolume>" 'ignore
  "<XF86AudioRaiseVolume>" 'ignore)

(general-def "C-x C-z" 'ignore)

(use-package highlight-escape-sequences
  :ghook ('(cperl-mode-hook perl-mode-hook python-mode-hook tcl-mode-hook) 'hes-mode)
  :config
  (dolist (mode '(perl-mode cperl-mode python-mode tcl-mode))
    (add-to-list 'hes-mode-alist (cons mode hes-common-escape-sequence-re))))

(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t))

;;;; ivy extras

(use-package ivy-posframe
  :demand t
  :after ivy
  :custom
  (ivy-posframe-size-function 'ivy-posframe-get-size)
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode))

(use-package ivy-rich
  :demand t
  :after counsel
  :custom
  (ivy-rich-parse-remote-buffer nil)
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-read-file-transformer)
                ($ivy-rich-counsel-find-file-truename (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-describe-variable
             '(:columns
               ((counsel-describe-variable-transformer (:width 40))
                (ivy-rich-counsel-variable-value (:width 10))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-candidate (:width 100))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 15 :face warning))
                (ivy-rich-switch-buffer-project (:width 25 :face success)))
               :predicate
               (lambda (cand) (get-buffer cand))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-describe-function
             '(:columns
               ((counsel-describe-function-transformer (:width 40))
                (ivy-rich-counsel-function-docstring (:width 70 :face font-lock-doc-face)))))
  (ivy-rich-mode)
  (ivy-rich-project-root-cache-mode))

(defun $ivy-rich-counsel-find-file-truename (candidate)
  (let ((type (and (not (file-remote-p ivy--directory))
                   (car (file-attributes
                         (directory-file-name
                          (expand-file-name candidate ivy--directory)))))))
    (if (stringp type)
        (concat "→ " (expand-file-name type ivy--directory))
      "")))

(defun ivy-rich-counsel-variable-value (candidate)
  (let* ((var (intern candidate))
         (val (prin1-to-string
               (if (boundp var)
                   (symbol-value var)
                 '<unbound>))))
    (if (< 31 (length val))
        (substring val 0 30)
      val)))

(use-package ivy-prescient
  :demand t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil "We don't want the initialism filtering")
  :init
  (setq ivy-prescient-sort-commands '(:not swiper swiper-isearch ivy-switch-buffer counsel-M-x))
  :config
  (prescient-persist-mode)
  (ivy-prescient-mode)
  (setf (alist-get 'counsel-describe-function ivy-sort-functions-alist)
        #'ivy-prescient-sort-function)
  (setf (alist-get 'counsel-describe-variable ivy-sort-functions-alist)
        #'ivy-prescient-sort-function))

;;;; Evil text objects

(evil-ex-define-cmd "q" 'kill-current-buffer)
(evil-ex-define-cmd "q!" 'kill-current-buffer)
(evil-ex-define-cmd "wq" (defun $save-and-kill-buffer ()
                           (interactive)
                           (save-buffer)
                           (kill-this-buffer)))

(evil-define-text-object evil-entire-buffer (count &optional _beg _end _type)
  (list (point-min) (point-max)))

(evil-define-text-object evil-pasted (count &optional _beg _end _type)
  (list (save-excursion (evil-goto-mark ?\[) (point))
        (save-excursion (evil-goto-mark ?\]) (1+ (point)))))

(evil-define-text-object evil-filename (count &optional _beg _end _type)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (list (car bounds) (cdr bounds))))

(evil-define-text-object evil-a-dir (count &optional _beg _end _type)
  (list (save-excursion (search-backward "/") (point))
        (save-excursion (search-forward "/") (1- (point)))))

(defmacro $define-evil-inner-object (name char)
  `(evil-define-text-object ,(cadr name) (count &optional _beg _end _type)
     (list (save-excursion (search-backward ,char) (1+ (point)))
           (save-excursion (search-forward ,char) (1- (point))))))

($define-evil-inner-object 'evil-i-dir "/")
($define-evil-inner-object 'evil-i-tilde "~")
($define-evil-inner-object 'evil-i-equal "=")
($define-evil-inner-object 'evil-i-dot ".")

(general-def 'outer
  "/" 'evil-a-dir)

(general-def 'inner
  "P" 'evil-pasted
  "n" 'evil-filename
  "/" 'evil-i-dir
  "~" 'evil-i-tilde
  "=" 'evil-i-equal
  "." 'evil-i-dot
  "g" 'evil-entire-buffer)

(use-package evil-indent-plus
  :demand t
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-textobj-syntax
  :ensure
  (:host github :repo "laishulu/evil-textobj-syntax")
  :general
  (inner "h" 'evil-i-syntax)
  (outer "h" 'evil-a-syntax))

(use-package evil-textobj-column
  :general
  ('inner
   "k" 'evil-textobj-column-word
   "K" 'evil-textobj-column-WORD))

(use-package evil-textobj-line
  :init
  (setq evil-textobj-line-i-key "v"
        evil-textobj-line-a-key "v")
  :general
  (inner "v" 'evil-inner-line)
  (outer "v" 'evil-a-line))

(use-package evil-args
  :general
  (inner "a" 'evil-inner-arg)
  (outer "a" 'evil-outer-arg))

(evil-ex-define-cmd "j" 'counsel-fd-jump)
(evil-ex-define-cmd "lisp" '$lisp-mode)

(defun $lisp-mode ()
  (interactive)
  (emacs-lisp-mode)
  (setq lexical-binding t))

($leader-set-key
  "ss" '$counsel-rg-here
  "sr" '$counsel-rg-root
  "j" 'counsel-semantic-or-imenu)

(general-def 'insert
  "C-v" 'yank
  "C-y" 'yank)
(general-def 'emacs
  "<escape>" 'evil-normal-state)

(general-def 'motion
  "K" 'evil-backward-WORD-end)

($leader-set-key
  "hs"  'profiler-start
  "hS"  'profiler-stop
  "hr"  'profiler-report
  "hR"  'profiler-reset
  "br" 'rename-buffer
  "bR" 'revert-buffer
  "s" '(:ignore t :wk "search")
  "sc" 'evil-ex-nohighlight
  "u" 'universal-argument)

(use-package undo-tree
  :demand t
  :init
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))
($leader-set-key
  "U" 'undo-tree-visualize)
(general-def '(normal visual)
  "u" 'undo-tree-undo
  "C-r" 'undo-tree-redo)

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(undo discard-info)))
(setq ad-redefinition-action 'accept)

(use-package evil-unimpaired
  :defer 2
  :ensure
  (:host github :repo "zmaas/evil-unimpaired")
  :init
  (setq evil-unimpaired-leader-keys '("gk" . "gj"))
  :config
  (evil-unimpaired-mode))

(use-package evil-collection
  :demand t
  :config
  (add-hook 'evil-collection-setup-hook
            (defun $unmap-leader (_mode keymaps)
              (when (and keymaps
                         (not (memq 'eshell-mode-map keymaps))
                         (not (memq 'ediff-mode-map keymaps)))
                (general-define-key
                 :states 'normal
                 :keymaps keymaps
                 $leader-key nil
                 $mm-leader-key nil))))
  (evil-collection-init)
  ;; evil-integration overloads `0'. Since we swap ^ and 0, we need to bind it
  ;; back
  (evil-define-minor-mode-key 'motion 'visual-line-mode
    "0" 'evil-first-non-blank-of-visual-line))

(use-package evil-surround
  :defer 4
  :general
  ('visual evil-surround-mode-map
           "s" 'evil-surround-region
           "S" 'evil-Surround-region)
  :config
  (setf (alist-get ?\( evil-surround-pairs-alist) '("(" . ")"))
  (setf (alist-get ?\[ evil-surround-pairs-alist) '("[" . "]"))
  (setf (alist-get ?\{ evil-surround-pairs-alist) '("{" . "}"))
  (setf (alist-get ?t  evil-surround-pairs-alist) '$evil-surround-tcl)
  (global-evil-surround-mode))

(defun $evil-surround-tcl ()
  "Read a functionname from the minibuffer and wrap selection in tcl set command"
  (let ((var (evil-surround-read-from-minibuffer "" "")))
    (cons (format "set %s [" (or var "")) "]")))

(use-package evil-nerd-commenter
  :commands (evilnc-copy-and-comment-operator
             evilnc-comment-operator)
  :general
  ('normal
   "gc" 'evilnc-comment-operator
   "gC" 'evilnc-copy-and-comment-operator)
  (inner "c" 'evilnc-inner-commenter)
  (outer "c" 'evilnc-outer-commenter))

(use-package evil-replace-with-register
  :general
  ('normal "go" 'evil-replace-with-register))

(use-package evil-exchange
  :general
  (:states '(visual normal)
   "gx" 'evil-exchange
   "gX" 'evil-exchange-cancel))

(use-package evil-numbers
  :ensure (:host github :repo "janpath/evil-numbers"))

(use-package evil-matchit
  :hook prog-mode)

(use-package lispyville
  :hook emacs-lisp-mode
  :init
  (setq lispyville-key-theme
        '(operators
          c-w
          prettify
          text-objects
          (atom-movement t)
          additional-movement
          slurp/barf-cp
          wrap
          additional
          additional-insert
          additional-wrap))
  :config
  (defhydra $lispyville-move-sexp (:pre (require 'lispyville)
                                   :exit nil)
    ("[" lispyville-previous-opening "( <-")
    ("]" lispyville-next-closing "-> )")
    ("{" lispyville-next-opening "-> (")
    ("}" lispyville-previous-closing ") <-"))
  (general-def 'normal emacs-lisp-mode-map
    "[[" '$lispyville-move-sexp/lispyville-previous-opening
    "]]" '$lispyville-move-sexp/lispyville-next-closing))

(general-def 'motion
 "H-[" 'evil-backward-paragraph
 "C-]" 'evil-forward-paragraph)

(defun $forward-sexp (arg)
  (interactive "^p")
  (when (and (null evil-move-beyond-eol)
             (memq (char-after) '(?\) ?\} ?\])))
    (forward-char))
  (forward-sexp arg))

(defun $backward-sexp (arg)
  (interactive "^p")
  (when (and (null evil-move-beyond-eol)
             (memq (char-after) '(?\) ?\} ?\])))
    (forward-char))
  (backward-sexp arg))

(general-def 'normal "C-M-f" '$forward-sexp)
(general-def 'normal "C-M-b" '$backward-sexp)

(defun $lispy-wrap-adjust-paren (_arg)
  "Always wrap the sexp that the cursor is on"
  (when (eq ?\) (char-syntax (char-after)))
    (evil-jump-item)))
(advice-add 'lispy-wrap-round :before '$lispy-wrap-adjust-paren)

(use-package paredit
  :ensure
  (:files ("paredit.el")
   :repo "http://mumble.net/~campbell/git/paredit.git"))

(use-package evil-cleverparens
  :commands (evil-cp-insert
             evil-cp-append)
  :general
  ('normal
   emacs-lisp-mode-map
   "i" 'evil-cp-insert
   "a" 'evil-cp-append))

(setq evil-kill-on-visual-paste nil
      select-enable-primary t)

(fset 'evil-visual-update-x-selection 'ignore)

(set-language-environment "UTF-8")

(csetq indent-tabs-mode nil)

(csetq large-file-warning-threshold 500000000)

(csetq sentence-end-double-space nil)

(setq-default fill-column 100)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun delete-whole-line (&optional arg)
  "delete current line.
With prefix ARG, delete that many lines starting from the current line.
If ARG is negative, delete backward.  Also delete the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.)
If ARG is zero, delete current line but exclude the trailing newline."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (setq last-command 'delete-region)
  (cond ((zerop arg)
         (save-excursion
           (delete-region (point) (progn (forward-visible-line 0) (point))))
         (delete-region (point) (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (save-excursion
           (delete-region (point) (progn (end-of-visible-line) (point))))
         (delete-region (point)
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp) (backward-char))
                               (point))))
        (t
         (save-excursion
           (delete-region (point) (progn (forward-visible-line 0) (point))))
         (delete-region (point)
                        (progn (forward-visible-line arg) (point))))))

(define-key minibuffer-local-map [C-backspace] 'backward-delete-word)
(define-key minibuffer-local-map [C-S-backspace] 'delete-whole-line)

(defun ivy-backward-delete-word ()
  "Forward to `backward-delete-word'."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (ivy--parent-dir (expand-file-name ivy--directory)))
        (ivy--exhibit))
    (ignore-errors
      (let ((pt (point))
            (last-command (if (eq last-command 'ivy-backward-delete-word)
                              'delete-region
                            last-command)))
        (forward-word -1)
        (delete-region pt (point))))))

(defun ivy-delete-whole-line ()
  "Forward to `delete-whole-line'."
  (interactive)
  (delete-region (minibuffer-prompt-end) (line-end-position)))

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map [C-S-backspace] 'ivy-delete-whole-line)
  (define-key ivy-minibuffer-map [C-backspace] 'ivy-backward-delete-word))

;; Emacs has an interesting way of handling the tab key. Both <tab> and C-i share
;; the same terminal keycode. This means that in terminal applications, using C-i
;; is the equivalent of pressing tab. Emacs distinguishes between the two by
;; assigning C-i to =TAB=. under normal circumstances, <tab> will be automatically
;; translated to =TAB= (C-i) before being dispatched. However this means that we
;; can't use the C-i binding in the GUI for anything other then tab. Evil provides
;; the ablitity to use C-i to jump to the next mark. So to get both C-i to jump to
;; the next mark and still retain our tab indent behavior, we setup the following
;; code. We remap the C-i key to the H-i (hyper) keycode in the =input-decode-map=,
;; then we bind =evil-jump-forward=. So now pressing C-i will trigger the keycode for
;; H-i, which is bound to evil-jump-forward.
(general-def input-decode-map
  "C-i" "H-i"
  "C-[" "H-["
  "C-m" "H-m")
(general-def 'normal
  "H-i" 'evil-jump-forward)

(setq tab-always-indent 'complete)

;;;; Copilot

(defun $copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(use-package copilot
  :ensure (:host github :repo "zerolfx/copilot.el"
           :files ("dist" "copilot.el" "copilot-balancer.el"))
  :general
  (:states '(insert) :keymaps 'copilot-mode-map
   "C-c c" #'copilot-complete
   "TAB" #'$copilot-tab
   "<tab>" #'$copilot-tab)
  (:keymaps 'copilot-completion-map
   "M-n" #'copilot-accept-completion-by-line
   "M-N" #'copilot-next-completion
   "M-P" #'copilot-previous-completion)
  :init
  (setq copilot-max-char -1)
  (setq copilot-clear-overlay-ignore-commands '(mwheel-scroll)))

(setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))

(use-package chatgpt-shell
  :init
  (setq chatgpt-shell-model-version "gpt-4"))

(evil-ex-define-cmd "chat" #'chatgpt-shell)
($leader-set-key
  "e" #'chatgpt-shell)

(use-package aider
  :ensure (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--sonnet"))
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package gptel
  :config
  (gptel-make-anthropic "Claude" :stream t))


(use-package ws-butler
  :hook (org-mode prog-mode)
  :config
  (setq ws-butler-convert-leading-tabs-or-spaces t))

(with-eval-after-load 'whitespace
  (delq 'lines whitespace-style))

($leader-set-key
  "tw" 'whitespace-mode
  "xd" 'delete-trailing-whitespace)

(setq-default indicate-empty-lines t)

(defun $normalize-text (beg end)
  "normalize characters used in Microsoft formatting"
  (let* ((orig-text (buffer-substring beg end))
         (normalized-text
          (thread-last orig-text
            (replace-regexp-in-string "–" "--")
            (replace-regexp-in-string (rx (char "‘’")) "'")
            (replace-regexp-in-string (rx (char "“”")) "\""))))
    (unless (equal orig-text normalized-text)
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert normalized-text)))))

(defun $normalize-region (beg end)
  "normalzie the last paste, or if region is selected, normalize
that region."
  (interactive "r")
  (if (region-active-p)
      (progn ($normalize-text beg end)
             (deactivate-mark))
    (apply #'$normalize-text (cl-sort (list (point) (mark t)) '<))))

($leader-set-key
  "xn" '$normalize-region)

($leader-set-key
  "xa" 'align
  "xr" 'align-regexp
  "xt" 'untabify
  "zq" 'quick-calc)

(setq enable-recursive-minibuffers t)

(defun insert-current-file-name-at-point (&optional full-path)
  "Insert the current filename at point.
   With prefix argument, use full path."
  (interactive "P")
  (let* ((buffer (if (minibufferp)
                     (window-buffer (minibuffer-selected-window))
                   (current-buffer)))
         (filename (buffer-file-name buffer)))
    (if filename
        (insert (if full-path
                    (file-truename filename)
                  (file-name-nondirectory filename)))
      (error (format "Buffer %s is not visiting a file" (buffer-name buffer))))))

(general-def minibuffer-local-map
  "H-i" #'insert-current-file-name-at-point)

(general-def '(evil-ex-completion-map minibuffer-local-map)
  "C-n" 'next-complete-history-element
  "C-p" 'previous-complete-history-element)

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-l")
  (read-process-output-max (* 1024 1024) "needed for better performace")
  :general
  ('normal "C-." 'lsp-execute-code-action)
  (lsp-mode-map "C-." 'lsp-execute-code-action
                "C-l f" 'lsp-ivy-workspace-symbol))

(use-package lsp-ui
  :custom
  (lsp-idle-delay 1.0)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-doc-show-with-mouse t))

(advice-add #'lsp-execute-code-action :after
            (defun $evil-normal-state-advice (&rest _)
              (evil-normal-state)))

(use-package lsp-ivy)

;;;; LSP Booster
;; https://github.com/blahgeek/emacs-lsp-booster

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))


(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(with-eval-after-load 'lsp-mode
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

($leader-set-key
  "n" '(:ignore t :wk "narrow")
  "nw" 'widen
  "nr" 'narrow-to-region
  "np" '$narrow-to-paragraph
  "nf" 'narrow-to-defun)

(defun $narrow-to-paragraph ()
  "Narrow to the current evil paragraph"
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'evil-paragraph)
    (narrow-to-region beg end)))

(use-package saveplace
  :defer 5
  :ensure nil
  :config
  (save-place-mode))

(use-package mwim
  :general
  (:states 'insert
   "C-e" 'mwim-end
   "C-a" 'mwim-beginning))

;;;; Indentation level
(defun $get-indent-level ()
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((flag t)
          (size 0))
      (while flag
        (let ((char (following-char)))
          (cond ((eq char ?\s) (cl-incf size))
                ((eq char ?\t) (cl-incf size tab-width))
                (t (setq flag nil)))
          (forward-char)))
      size)))

(defun $check-indent-level (indent)
  (or (looking-at-p (rx bol (0+ space) eol))
      (<= indent ($get-indent-level))))

(defun $up-indent-level ()
  "go up an indentation level"
  (interactive)
  (let ((indent ($get-indent-level)))
    (while ($check-indent-level indent)
      (previous-line))))

(defun $down-indent-level ()
  "go down an indentation level"
  (interactive)
  (let ((indent ($get-indent-level)))
    (while ($check-indent-level indent)
      (next-line))))

;;;; Navigation packages
(general-def 'motion "gh" '$up-indent-level)
(general-def 'motion "gH" '$down-indent-level)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 'append))

(use-package avy
  :init
  (setq avy-timeout-seconds 0.2)
  :general
  (:states '(motion normal)
   "s" 'avy-goto-char-timer))

(use-package symbol-overlay
  :init
  ($leader-set-key
    "ii" 'symbol-overlay-put
    "ic" 'symbol-overlay-remove-all))

(use-package quickrun
  :general
  (:definer 'leader
   :keymaps '(python-mode-map sh-mode-map tcl-mode-map)
   "r" 'quickrun
   "R" 'quickrun-with-arg)
  :custom
  (quickrun-timeout-seconds 30))

(use-package yasnippet
  :defer 3
  :general
  ('yas-minor-mode-map
   "TAB" nil
   ;; [(tab)] nil
   "<tab>" nil
   "C-c C-s" 'yas-expand)
  :config
  (let ((inhibit-message t))
    (yas-global-mode)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package evil-multiedit
  :custom
  (evil-multiedit-use-symbols t)
  :config
  (evil-multiedit-default-keybinds)
  :general
  (:states '(normal visual)
   "M-d" 'evil-multiedit-match-symbol-and-next
   "M-D" 'evil-multiedit-match-symbol-and-prev
   "R" 'evil-multiedit-match-all))

(use-package 0xc
  :general
  (:definer 'leader
   "xc" '0xc-convert
   "xp" '0xc-convert-point))

;;;; Regexp
(defvar $rx-defaults
  '((spc (any " \t"))
    (spc+ (1+ spc))
    (spc* (0+ spc))
    (-> (1+ any))
    (^ bol)
    (file (1+ (any alnum "-_/.#~:")))
    (symbol (1+ (any alnum "_-")))
    (nums (1+ num))
    (fp (1+ (any num "."))))
  "modified rx forms that are really usefull")

(if (version< "27.0.0" emacs-version)
    (defmacro define-arx (name defs)
      `(defmacro ,name (&rest forms)
         `(rx-let ,,defs
            (rx ,@forms))))
  (use-package ample-regexps))

(define-arx $rx $rx-defaults)

(use-package pcre2el
  :commands reb-change-syntax)

;;;; TRAMP

;; Fix for Emacs 29 tramp issue
;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2024-05/msg01920.html
(use-package tramp
  :init
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        tramp-copy-size-limit 5000000
        tramp-verbose 2
        remote-file-name-inhibit-auto-save-visited t))

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :machine "server")
 'remote-direct-async-process)

(with-eval-after-load 'tramp-sh
  (setq magit-tramp-pipe-stty-settings 'pty))

;; Don't use ffap on remote files for performance
(with-eval-after-load 'ffap
  (defun $ffap-guess-is-remote ()
    (file-remote-p default-directory))
  (advice-add 'ffap-guess-file-name-at-point :before-until #'$ffap-guess-is-remote))

;; Used to speed up some tramp operations
(defun $memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if they key is a remote path."
  (if (file-remote-p key)
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

;;;; Projects

(defvar project-current-cache nil
  "Cache of current projects")

(defun $memoize-project-current (orig &optional prompt directory)
  "If we have visited project before, return the cached value."
  ($memoize-remote (or directory
                       project-current-directory-override
                       default-directory)
                   'project-current-cache orig prompt directory))

(advice-add 'project-current :around #'$memoize-project-current)

(defun $clear-project-current-cache ()
  "Clear the project cache"
  (interactive)
  (setq project-current-cache nil))

(defun $find-file-from-root ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (call-interactively #'find-file)))

($leader-set-key "ff" '$find-file-from-root)

(defun $project-buffers (arg &optional dir)
  (interactive "P")
  (let ((root (project-root (project-current
                    nil (or dir default-directory))))
        ivy-use-virtual-buffers
        buffers)
    (if (null root)
        (user-error "no project root found")
      (setq root (file-truename root))
      (setq buffers (all-completions
                     "" #'internal-complete-buffer
                     (lambda (buf) ($buffer-in-project buf root arg))))
      (ivy-read (if arg "project buffers: "
                  "open project files: ")
                buffers
                :keymap ivy-switch-buffer-map
                :action #'ivy--switch-buffer-action
                :matcher #'ivy--switch-buffer-matcher))))

(defun $get-project-root (buffer)
  (thread-last
      (or (buffer-local-value 'buffer-file-truename buffer)
          (file-truename (buffer-local-value 'default-directory buffer)))
    (project-current nil)
    (cdr)))

(defun $buffer-in-project (buf project include-dirs)
  (let* ((buffer (cdr buf)))
    (and (not (string-match-p "\\*" (buffer-name buffer)))
         (not (file-remote-p (buffer-local-value 'default-directory buffer)))
         (or include-dirs
             (buffer-local-value 'buffer-file-name buffer))
         (equal ($get-project-root buffer)
                project))))

(with-eval-after-load 'ivy
  (ivy-add-actions 'counsel-find-file
                   '(("b" (lambda (x)
                            ($project-buffers ivy-current-prefix-arg x))
                      "buffers")))
  (ivy-add-actions '$project-buffers
                   '(("k" ivy--kill-buffer-action "kill"))))

($leader-set-key "bb" 'ivy-switch-buffer)

(defun $read-common-file (file-list &optional prompt)
  "read a file amoung common paths"
  (unless file-list (user-error "no files found"))
  (if (cdr file-list)
      (let ((parent (f-common-parent file-list)))
        (f-expand (completing-read
                   (or prompt "Select file: ")
                   (mapcar (lambda (x) (f-relative x parent)) file-list))
                  parent))
    (car file-list)))

(defun $jump-project-bookmark ()
  "Jump to a project bookmark."
  (interactive)
  (let* ((bookmark (completing-read "Jump to bookmark: " (mapcar 'car $project-bookmarks)))
         (paths (cdr (assoc bookmark $project-bookmarks)))
         (file-list (cl-loop for path in paths
                             for files = (file-expand-wildcards (expand-file-name path ($model-root)))
                             if files
                             return files))
         (file (condition-case-unless-debug nil
                   ($read-common-file file-list)
                 (error (user-error "No file found for '%s'" bookmark)))))
    (if (file-directory-p file)
        (find-file (read-file-name "Find file: " file))
      (find-file file))))

($leader-set-key
  "pj" '$jump-project-bookmark)

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package paren
  :ensure nil
  :demand t
  :after prog-mode
  :custom
  (evil-show-paren-range 3)
  (show-paren-delay 0)
  :config
  (show-paren-mode))

;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

;; A function taken from
;; https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen
;; that will use an overlay to echo the matching paren line. I am just afraid
;; that this will lead to issues when looking at really large JSON file and the
;; function tries to back track through the whole file to find a match.

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
                (not (or cursor-in-echo-area
                         executing-kbd-macro
                         noninteractive
                         (minibufferp)
                         this-command))
                (and (not (bobp))
                     (memq (char-syntax (if (eq evil-state 'insert)
                                            (char-before)
                                          (or (char-after) 41)))
                           '(?\) ?\$)))
                (= 1 (logand 1 (- (point)
                                  (save-excursion
                                    (forward-char -1)
                                    (skip-syntax-backward "/\\")
                                    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
                  (lambda (msg &rest args)
                    (let ((msg (apply #'format-message msg args)))
                      (setq ov (display-line-overlay+
                                (window-start) msg ))))))
         (save-excursion
           (goto-char (if (eq evil-state 'insert)
                          (point)
                        (1+ (point))))
           (blink-matching-open)))))))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(setq show-paren-style 'parenthesis
      show-paren-when-point-in-periphery t)

(csetq revert-without-query `(,(rx (1+ nonl))))

;;;; file operations

(use-package crux
  :general
  (:definer 'leader
   "fR" 'crux-rename-file-and-buffer
   "fD" 'crux-delete-file-and-buffer)
  ("C-c e" 'crux-eval-and-replace))

(defun $find-user-config-file ()
  "Edit the org file we use for config, in the current window."
  (interactive)
  (find-file-existing (expand-file-name "init.el" user-emacs-directory)))

(use-package async)

(defun $async-delete-file (target &optional targets no-prompt)
  "delete a file or directory by moving it to a tmp location and
then removing in the background"
  (interactive "D")
  (when (and (file-exists-p (or target (car targets)))
             (or no-prompt
                 (y-or-n-p "really delete file(s)?")))
    (cl-loop for file in (or targets (list target))
             do (let ((tmp-file (make-temp-name
                                 (concat
                                  (string-remove-suffix "/" file)
                                  ".del."))))
                  (async-start
                   (lambda ()
                     (rename-file file tmp-file)
                     (if (file-directory-p tmp-file)
                         (delete-directory tmp-file 'recursive)
                       (delete-file tmp-file)))
                   (lambda (_result)
                     (message (format "file %s deleted successfully" file))))))))

(defun $copy-file ()
  "Copy the current file and create any directories along the way"
  (interactive)
  (let* ((destination (read-file-name "Write File: "))
         (dir (file-name-directory destination)))
    (unless (file-exists-p dir)
      (make-directory dir 'parents))
    (when (file-directory-p destination)
      (setq destination (expand-file-name
                         (file-name-nondirectory (buffer-file-name))
                         destination)))
    (setq doom-modeline-project-root nil)
    (write-file destination 'confirm)))

($leader-set-key
  "f" '(:ignore t :wk "files")
  "fc" '$copy-file
  "fe" '$find-user-config-file)

(defun $normalize-file-name (file)
  "This functions does a few things.
2. automatically add remote prefix if required
3. Remove problematic formating from files"
  ;; add remote url if required
  (let* ((remote-url (if (file-name-absolute-p file)
                         (file-remote-p default-directory)
                       "")))
    (when (and (not (string-suffix-p "/" file))
               (file-directory-p file))
      (cl-callf concat file "/"))
    ;; remove problematic formatting from files
    (thread-last file
                 (replace-regexp-in-string (rx (1+ (any space "\""))) "")
                 (replace-regexp-in-string "\"" "")
                 (string-remove-prefix "./")
                 (replace-regexp-in-string "$ENV" "$")
                 ($substitute-env-in-filename)
                 (replace-regexp-in-string (rx (1+ "/")) "/")
                 (concat remote-url)
                 (substitute-in-file-name))))

(defun $substitute-env-in-filename (file)
  "if the variables in the file name are present in the file,
substitute them in the path"
  (let ((re-env ($rx "$" (group symbol)))
        (re-path ($rx (group (1+ (any alnum "-_/.~$:"))) (or spc eol)))
        (orig-point (point)))
    (cl-loop for (var env) in (s-match-strings-all re-env file)
             if (save-excursion
                  (goto-char (point-min))
                  (or (re-search-forward (concat env "=" re-path) orig-point t) ;; bash
                      (re-search-forward (concat "setenv +" env " +" re-path) orig-point t))) ;; tcsh
             collect (cons var
                           ($substitute-env-in-filename
                            (match-string-no-properties 1)))
             into matches
             finally return (if matches
                                (s-replace-all matches file)
                              file))))

(defun $get-chars-at-point (chars)
  (let ((beg (save-excursion
               (skip-chars-backward chars)
               (point)))
        (end (save-excursion
               (skip-chars-forward chars)
               (point))))
    (cons beg end)))

(defun $get-path-at-point ()
  "Get the filepath at point.
This includes remote paths and enviroment variables."
  (let* ((bounds ($get-chars-at-point "+-{}[:alnum:]$:/.#_~\""))
         (beg (car bounds))
         (end (cdr bounds))
         (substring (buffer-substring-no-properties beg end))
         ;; we need to get : so that we can handle tramp paths, but sometimes it is also at the of a
         ;; path. In which case need to remove it
         (path (replace-regexp-in-string (rx (1+ (any ":" digit)) eos) "" substring))
         ;; remove +incdir+ from the start of the path
         (path (replace-regexp-in-string (rx bos "+incdir+") "" path)))
    (if (save-excursion
          (goto-char beg)
          (or (looking-back ($rx "cfg::MODEL_ROOT()" spc* "." spc*) (line-beginning-position))
              (looking-back (regexp-quote "$::env(MODEL_ROOT)") (line-beginning-position))))
        (concat "$MODEL_ROOT" path)
      path)))

(defun $counsel-initial-input (file)
  (unless (file-directory-p file)
    (concat (file-name-nondirectory file) "\\_>")))

(defun $open-file-in-clipboard ()
  "Open the file name in the clipboard"
  (interactive)
  (if-let ((path ($--get-file-from-clipboard)))
      (progn (when (string-prefix-p "/proj" path)
               (setq path (concat "/scp:server:" path)))
             (find-file path))
    (message "no path found in clipboard")))

(defun $open-dir-in-clipboard ()
  "Open the directory name in the clipboard"
  (interactive)
  (if-let ((path ($--get-file-from-clipboard)))
      (find-file (file-name-directory path))
    (message "no path found in clipboard")))

(defun $--get-file-from-clipboard ()
  (let ((is-path-p (apply-partially 'string-match-p ($rx bos (or "$" "/") file eos))))
    (thread-last (current-kill 0)
                 (string-trim)
                 (split-string)
                 (cl-find-if is-path-p)
                 ($normalize-file-name))))

(defun $find-file-at-point ()
  "A better replacement for `find-file-at-point'"
  (interactive)
  (let* ((file ($normalize-file-name ($get-path-at-point)))
         (context (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position 2)))
         (line-rx ($rx (any alnum "\"'")
                       (or ":"
                           (: "," (* (any " \n")))
                           (: (opt ",") " line ")
                           (: "(")
                           (: " ")
                           (: ", Line: "))
                       (group nums)))
         (root (when-let ((current (project-current)))
                 (project-root current)))
         (search-dirs
          (delq nil (list "" ;; current directory
                          (and root (concat root "bazel-build/"))
                          (and root (concat root "build/bazel-build/")))))
         (file-path (cl-loop for dir in search-dirs
                             for path = (concat dir file)
                             if (file-exists-p path)
                             return path
                             finally (user-error (format "File %s does not exists" file)))))
    (find-file file-path)
    (when (string-match line-rx context)
      (goto-line (string-to-number (match-string 1 context))))))

(defun $find-file-in-dirs (file dirs)
  (cl-loop for dir in dirs
           with path = (concat dir file)
           if (file-exists-p path)
           return path
           finally (user-error (format "File %s does not exists" file))))

(defun $paste-relative-path ()
  "paste the contents of the clipboard. If it is a path, make it relative to `default-directory'"
  (interactive)
  (let* ((path (current-kill 0))
         (rel-path (file-relative-name (file-truename path) (file-truename default-directory))))
    (insert rel-path)))

(defun $file-at-point-exists ()
  "Check if file at point exists."
  (interactive)
  (let ((file ($normalize-file-name ($get-path-at-point))))
    (if (file-exists-p file)
       (message "File exists")
     (message "File does not exist!"))))

(defun $change-model ()
  "Open a model in workspace"
  (interactive)
  (let ((default-directory (if ($dev-config-p) "/scp:server:~/workspace/" "~/"))
        (major-mode 'fundamental-mode))
    (counsel-find-file)))

(defun $goto-repo ()
  (interactive)
  (let ((default-directory "~/.emacs.d/elpaca/repos/")
        (major-mode 'fundamental-mode))
    (counsel-find-file)))

($leader-set-key
  "fo" '$open-file-in-clipboard
  "fO" '$open-dir-in-clipboard
  "fw" '$change-model
  "fa" '$file-at-point-exists
  "fd" '$goto-repo)
(general-def '(normal visual motion)
  "gf" '$find-file-at-point)

(general-def
  "C-c p" '$paste-relative-path)

(defun $update-filename-with-root ()
  (interactive)
  (when-let* ((file (current-kill 0))
              (root (expand-file-name (vc-git-root file))))
    (kill-new (s-replace root "$MODEL_ROOT/" file) 'replace)))

(defun $update-filename-relative ()
  (interactive)
  (let* ((file (current-kill 0)))
    (kill-new (file-relative-name file default-directory) 'replace)))

($leader-set-key
  "fr" '$update-filename-relative
  "fg" '$update-filename-with-root)


;; search up the directory stack looking for files that only differ from the
;; current file by one directory. This is useful for mulitply instantiated files
;; that use the same path with a different parent. For example

;; foo/bar/baz.txt
;; foo/bat/baz.txt
;; foo/ban/baz.txt

;; would all be considered similar files.

(defun $switch-to-similar-file ()
  "find a file of the same name with only one directory different"
  (interactive)
  (require 'f)
  (if-let ((file (buffer-file-name))
           (parts (f-split file))
           (head (butlast parts 2))
           (dir (car (last parts 2)))
           (tail (last parts))
           (other-files (or ($find-similar-file head dir tail)
                            ($find-similar-file head dir
                                                (list (concat "*" ($file-name-extension (car tail)))))))
           (common-parent (f-common-parent (cons file other-files)))
           (unique-file (completing-read "select other file: "
                                         (mapcar (lambda (f)
                                                   (string-remove-prefix common-parent f))
                                                 other-files))))
      (find-file (f-join common-parent unique-file))
    (cond ((buffer-file-name) (user-error "No similar file found"))
          (t (user-error "buffer not visitng a file")))))

(defun $file-name-extension (file)
  (when (string-match (rx "." (1+ nonl) eos) file)
    (match-string 0 file)))

(defun $find-similar-file (head dir tail)
  "search up the directory path for paths that very by only one
directory pointing to the same file name"
  ;; anything above 5 depth is not worth searching becase we are out
  ;; of the disk
  (when (>= (length head) 5)
    (let* ((parent (apply 'f-join head))
           (child (apply 'f-join tail))
           (rootp (file-exists-p (f-join parent ".git")))
           (orig (car (file-expand-wildcards
                       (f-join parent dir child))))
           (paths (delete orig (file-expand-wildcards
                                (f-join parent "*" child))))
           (valid-paths (cl-remove-if (lambda (f) (file-equal-p f orig)) paths)))
      (unless rootp
        (or valid-paths
            ($find-similar-file (butlast head) (car (last head)) (cons dir tail)))))))

(defun $find-file-other-model ()
  "find the same file in a different model in the same directory"
  (interactive)
  (let* ((file (buffer-file-name))
         (root (vc-git-root file))
         (path (string-remove-prefix root file))
         (workspace (f-parent root))
         (models (file-expand-wildcards (concat workspace "/*/" path)))
         (model (completing-read "Select Model: "
                                 (mapcar (lambda (f)
                                           (thread-last f
                                             (string-remove-suffix (concat "/" path))
                                             (string-remove-prefix (concat workspace "/") )))
                                         models))))
    (find-file (format "%s/%s/%s" workspace model path))))

($leader-set-key
  "fs" '$switch-to-similar-file
  "fm" '$find-file-other-model)

;;;; dired

(setq dired-no-confirm t
      wdired-allow-to-change-permissions t
      dired-listing-switches "-alh"
      dired-recursive-copies 'always
      dired-dwim-target t
      dired-auto-revert-buffer (defun $dired-not-remote-and-changed (dir)
                                 (and (not (file-remote-p dir))
                                      (dired-directory-changed-p dir))))

(general-def dired-mode-map
  "C-c C-p" 'wdired-change-to-wdired-mode)

(defun dired-try-simple-copy (orig-fn file-creator operation fn-list name-constructor &optional marker-char)
  (let* ((to (funcall name-constructor (car fn-list)))
         (to-file (if (< 1 (length fn-list))
                      (file-name-directory to)
                    to))
         exit-code)
    (if (and (eq file-creator 'dired-copy-file)
             (file-remote-p to)
             (tramp-equal-remote (car fn-list) to))
        (prog1 (message "Copying to %s" to-file)
          (setq exit-code (shell-command
                           (format "cp -r %s %s"
                                   (mapconcat 'file-name-nondirectory fn-list " ")
                                   (string-remove-prefix (file-remote-p to-file) to-file))))
          (when (= exit-code 0)
            (message "Done Copying"))
          (with-current-buffer (dired-noselect to-file)
            (revert-buffer)))
      (apply orig-fn file-creator operation fn-list name-constructor marker-char))))

(advice-add 'dired-create-files :around #'dired-try-simple-copy)
;; (advice-remove 'dired-create-files #'dired-try-simple-copy)

(defun $dired-here ()
  (interactive)
  (dired default-directory))

($leader-set-key "d" '$dired-here)

(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes '("\\.gtar\\'" ".tar" nil)))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 500
        recentf-auto-cleanup "11:00pm"))

;;;; Path check

(defface path-check-exists
  '((t :inherit 'diff-refine-added :foreground "black"))
  "Face used to highlight paths that exist")
(defface path-check-nonexistent
  '((t :inherit 'diff-refine-removed :foreground "black"))
  "Face used to highlight paths that exist")

(defvar path-check-font-lock-keywords
  `((,($rx (group "/" alpha file))
     1 (if (file-exists-p (match-string 1))
           'path-check-exists
         'path-check-nonexistent)
     prepend)))

(define-minor-mode path-check-mode
  "check if paths in file exisit"
  :init-value nil
  (if path-check-mode
      (font-lock-add-keywords nil path-check-font-lock-keywords)
    (font-lock-remove-keywords nil path-check-font-lock-keywords))
  (font-lock-flush))

(midnight-mode)

(defun $never-kill-compilation-buffers (buffer)
  (with-current-buffer buffer
    (derived-mode-p 'compilation-mode)))

(add-to-list 'clean-buffer-list-kill-never-regexps '$never-kill-compilation-buffers)

(add-hook 'midnight-hook 'recentf-save-list)

;;;; auto insert

(use-package auto-insert
  :ensure nil
  :defer 5
  :init
  (setq auto-insert-query nil
        auto-insert-alist
        '(((emacs-lisp-mode . "Emacs Lisp header")
           nil ";;; -*- lexical-binding: t; -*-\n\n" _)
          (("\\.p[lm]\\'" . "Perl shebang")
           nil "#!/usr/intel/pkgs/perl/5.14.1/bin/perl\n\n"
           "use strict;\n" "use warnings;\n\n" _)
          ((sh-mode . "Sh shebang")
           nil "#!/bin/sh\n\n" _)
          ((python-mode . "Python shebang")
           nil "#!/usr/intel/bin/python3.6.3a\n\n" _)))
  (auto-insert-mode))

(winner-mode)
(defhydra $winner ()
  "Hydra for winner-mode"
  ("u" winner-undo "undo")
  ("r" winner-redo "redo"))
(with-eval-after-load 'ace-window
  (add-to-list 'aw-dispatch-alist '(?u $winner/winner-undo))
  (add-to-list 'aw-dispatch-alist '(?J aw-switch-buffer-other-window "Switch Buffer Other Window")))

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")

(setq-default ediff-ignore-similar-regions t)

(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package vdiff
  :general
  (:definer 'leader
   "fvf" 'vdiff-files
   "fvb" 'vdiff-buffers)
  (:keymaps 'vdiff-mode-map
   "C-c" '(:keymap vdiff-mode-prefix-map :package vdiff))
  :custom
  (vdiff-diff-algorithm 'git-diff-patience))

(use-package pdf-tools
  :init
  (pdf-loader-install))

;;;;;;;;
;;;; Git
;;;;;;;;

(csetq vc-follow-symlinks t)

(csetq vc-handled-backends '(Git))

(defun $git-command (&rest cmd)
  (string-trim-right
   (shell-command-to-string
    (concat (executable-find "git") " " (string-join cmd " ")))))

(autoload (function vc-git-root) "vc-git")

;; Use the git version of transient
(use-package transient)

(use-package magit
  :general
  ("C-x g" 'magit-status
   "C-x M-g" 'magit-dispatch
   "C-c M-g" 'magit-file-dispatch)
  (magit-diff-mode-map
   "SPC" nil)
  (magit-mode-map
   "SPC" nil)
  :init
  ($leader-set-key
    "g" '(:ignore t :wk "git")
    "gg" 'magit-dispatch
    "gf" 'magit-file-dispatch)
  (evil-ex-define-cmd "git" 'magit-status)
  ;; make transient not take the width of the whole frame
  (setq transient-display-buffer-action
        '(display-buffer-below-selected))
  (setq magit-commit-show-diff nil)
  (setq magit-branch-direct-configure nil)
  :config
  (when ($dev-config-p)
    (setq magit-refresh-status-buffer nil)
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source))

(use-package forge
  :demand t
  :after magit
  :init
  ;; will be set by evil-integration
  (setq forge-add-default-bindings nil)
  :config
  (remove-hook 'find-file-hook 'forge-bug-reference-setup) ;; for tramp
  (push '("aus-gitlab.local.tenstorrent.com"               ; GITHOST
          "aus-gitlab.local.tenstorrent.com/api/v4"        ; APIHOST
          "aus-gitlab.local.tenstorrent.com"               ; WEBHOST and INSTANCE-ID
          forge-gitlab-repository)    ; CLASS
        forge-alist))

;; improve performace by only reverting buffers in the local repo
;; https://magit.vc/manual/magit/Performance.html
(setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)

;; https://github.com/magit/magit/discussions/4817
(defun $magit-auto-revert-not-remote (orig-fun &rest args)
  (unless (and buffer-file-name (file-remote-p buffer-file-name))
    (apply orig-fun args)))

(advice-add 'magit-turn-on-auto-revert-mode-if-desired
            :around
            #'$magit-auto-revert-not-remote)

(defvar magit-toplevel-cache nil)

(defun $memoize-magit-toplevel (orig &optional directory)
  ($memoize-remote (or directory default-directory)
                   'magit-toplevel-cache orig directory))

(advice-add 'magit-toplevel :around #'$memoize-magit-toplevel)
;; (advice-remove 'magit-toplevel #'$memoize-magit-toplevel)

(defun $clear-magit-toplevel-cache ()
  (interactive)
  (setq magit-toplevel-cache nil))

(defvar vc-git-root-cache nil)

(defun $memoize-vc-git-root (orig file)
  ($memoize-remote (file-name-directory file) 'vc-git-root-cache orig file))

(advice-add 'vc-git-root :around #'$memoize-vc-git-root)
;; (advice-remove 'vc-git-root #'$memoize-vc-git-root)

(defun $clear-vc-git-root-cache ()
  (interactive)
  (setq vc-git-root-cache nil))

(csetq magit-diff-expansion-threshold 20)

(csetq magit-diff-paint-whitespace-lines 'both)

(with-eval-after-load 'with-editor
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(defun $magit-status-in-dir (dir)
  "limit magit status to directory"
  (interactive "D")
  (require 'magit)
  (let* ((root (vc-git-root dir))
         (dir (list (file-relative-name dir root)))
         (magit-status-mode-hook
          (cons (if (equal (car dir) "./")
                    '$magit-clear-diff-args-if-not-dir-local
                  (lambda () (setq-local magit-diff-section-file-args dir)))
                magit-status-mode-hook)))
    (magit-status-internal root)))

(defun $magit-status-current-dir ()
  "run magit in current dir"
  (interactive)
  ($magit-status-in-dir default-directory))

(defun $magit-clear-diff-args-if-not-dir-local ()
  (unless (assq 'magit-diff-section-file-args dir-local-variables-alist)
    (setq-local magit-diff-section-file-args nil)))

(advice-add 'magit-status :around
            (defun $magit-remove-diff-args (fn &rest args)
              (let ((magit-status-mode-hook (cons '$magit-clear-diff-args-if-not-dir-local
                                                  magit-status-mode-hook)))
                (apply fn args))))

($leader-set-key
  "gd" '$magit-status-current-dir
  "gs" 'magit-status)
(general-def
  "C-c g" '$magit-status-current-dir)

(defun $expand-org-mode-entry ()
  "When opening an org-mode file, show the current entry and all
headings that it is contained in."
  (when (derived-mode-p 'org-mode)
    (org-reveal '(4))))

(add-hook 'magit-diff-visit-file-hook '$expand-org-mode-entry)

(use-package browse-at-remote
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "gitlab\\..+\\.com" :type "gitlab")))

(use-package smerge-mode
  :ensure nil
  :config
  (defun $enable-smerge-keys ()
    (when smerge-mode
      (revert-buffer)
      ($smerge-keys/body)))
  (defhydra $smerge-keys
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _x_: kill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("x" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . $enable-smerge-keys ))

(use-package git-timemachine
  :general
  (:definer 'leader
   "gt" 'git-timemachine))

(use-package git-gutter
  :defer 3
  :init
  (setq git-gutter:diff-option "-w")
  (defhydra $git-hunk (:exit nil)
    ("n" git-gutter:next-hunk "next")
    ("N" git-gutter:previous-hunk "prev")
    ("p" git-gutter:previous-hunk "prev")
    ("r" git-gutter:revert-hunk "revert")
    ("s" git-gutter:stage-hunk "stage"))
  ($leader-set-key
    "gr" '$git-hunk/git-gutter:revert-hunk
    "gh" '$git-gutter-show-hunk
    "gs" '$git-hunk/git-gutter:stage-hunk
    "gn" '$git-hunk/git-gutter:next-hunk
    "gN" '$git-hunk/git-gutter:previous-hunk)
  :config
  (push `((nil . ,(rx (opt "$git-hunk/") "git-gutter:")) . (nil . "")) which-key-replacement-alist)
  (global-git-gutter-mode))

;; Don't load git gutter over tramp by default
(with-eval-after-load 'git-gutter
  (el-patch-defun git-gutter--turn-on ()
   (when (and (buffer-file-name)
              (not (memq major-mode git-gutter:disabled-modes))
              (el-patch-add (not (file-remote-p (buffer-file-name)))))
     (git-gutter-mode +1))))

(use-package git-gutter-fringe
  :demand t
  :after git-gutter)

(advice-add 'git-gutter:update-popuped-buffer :after
            (defun $git-gutter-window-quit (&rest _)
              (when-let ((buffer (get-buffer git-gutter:popup-buffer)))
                (with-current-buffer buffer
                  (general-def 'normal local
                    "q" 'quit-window)))))

(defun $git-gutter-show-hunk ()
  (interactive)
  (git-gutter:popup-hunk)
  (pop-to-buffer git-gutter:popup-buffer))

(with-eval-after-load 'git-gutter
  (advice-remove 'quit-window 'git-gutter:quit-window)
  (advice-remove 'switch-to-buffer 'git-gutter:switch-to-buffer)
  (general-advice-add '(switch-to-buffer quit-window)
                      :after (defun $git-gutter-reload (&rest _)
                               (when (and (not (file-remote-p default-directory))
                                          git-gutter-mode)
                                 (git-gutter)))))

(with-eval-after-load 'evil
  (evil-define-text-object evil-inner-hunk (count &optional _beg _end _type)
    (let ((hunk (git-gutter:search-here-diffinfo git-gutter:diffinfos)))
      (list (git-gutter:line-point (git-gutter-hunk-start-line hunk))
            (git-gutter:line-point (1+ (git-gutter-hunk-end-line hunk)))))))

(general-def 'inner
  "u" 'evil-inner-hunk)

;;;; shell
(setq shell-file-name "bash"
      explicit-shell-file-name "bash")

(use-package comint
  :ensure nil
  :general
  (:keymaps 'comint-mode-map
   :states '(normal insert)
   "C-p" 'comint-previous-matching-input-from-input
   "<return>" 'comint-send-input
   "C-n" 'comint-next-matching-input-from-input
   "C-<return>" '$copy-path)
  (:keymaps 'comint-mode-map
   [remap comint-dynamic-list-input-ring] 'counsel-shell-history)
  :custom
  (comint-input-ring-size 1000)
  (comint-scroll-to-bottom-on-input t)
  (comint-process-echoes t)
  (comint-prompt-read-only t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package xterm-color)

(defun $xterm-colorize-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (xterm-color-colorize-buffer)
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t)))
    (goto-char (point-min)))

(defun $goto-cmd-line (&rest _)
  (goto-char (point-max)))

(general-advice-add '(comint-next-matching-input-from-input
                      comint-previous-matching-input-from-input)
                    :before '$goto-cmd-line)

(defun $copy-path ()
  "copy path at point to prompt"
  (interactive)
  (let ((path (replace-regexp-in-string
               ($rx (or (: bol "{") (: "}" eol)))
               ""
               ($get-path-at-point))))
    (goto-char (point-max))
    (insert path)))

(use-package shell
  :ensure nil
  :gfhook #'company-mode)

(defvar $dir-history nil
  "previous shell directories")
(make-variable-buffer-local '$dir-history)

(defun $shell-directory-history ()
  "Select a previous directory using completing read"
  (interactive)
  (goto-char (point-max))
  (insert (concat "cd " (string-remove-prefix (or (file-remote-p default-directory) "")
                                              (completing-read "directory:" $dir-history)))))

(general-def shell-mode-map
  "C-c C-j" '$shell-directory-history)

(defun $track-shell-directory (str)
  "use the proc filesytem to get the current directory.
  Works on remote shells as well if `shx' and `shx-cmd-set-pid' are used. The
  remote shell will need to echo it's PID in the rc file in the form of `shx'
  markup."
  (if shell-dirtrack-mode
      (when (string-match (rx bol "Directory: " (group (+ (any alnum "-_/.")))) str)
        (let ((dir (match-string 1 str)))
          (cd dir)
          (unless (equal dir (car $dir-history))
            (push dir $dir-history))))
    (when (string-match-p comint-prompt-regexp str)
      (when-let ((remote (or (file-remote-p default-directory) ""))
                 (dir (-some->> (current-buffer)
                        (get-buffer-process)
                        (process-id)
                        (format "/proc/%s/cwd")
                        (concat remote)
                        file-symlink-p))
                 (full-dir (concat remote dir)))
        (when (file-directory-p full-dir)
          (cd full-dir)
          (unless (equal dir (car $dir-history))
            (push dir $dir-history))))))
  str)

(with-eval-after-load 'shell
  (modify-syntax-entry ?= "." shell-mode-syntax-table)
  (modify-syntax-entry ?> "." shell-mode-syntax-table)
  (modify-syntax-entry ?< "." shell-mode-syntax-table))

(defun $push-dir-to-history (dir)
  (unless (equal dir (car $dir-history))
    (push dir $dir-history)))

(defun $shell-mode-hook ()
  (setq-local evil-search-wrap nil)
  ;; Tramp will override this
  (setq-local comint-prompt-regexp (default-value 'comint-prompt-regexp))
  (shell-dirtrack-mode)
  (advice-add 'shell-cd :after #'$push-dir-to-history))

(add-hook 'shell-mode-hook '$shell-mode-hook)

(defun $counsel-shell-pop (arg buffer dir)
  "shell-pop to current buffers directory or dir"
  (if (and (boundp 'shell-pop-last-shell-buffer-name)
           (equal (buffer-name buffer)
                  shell-pop-last-shell-buffer-name))
      (shell-pop--cd-to-cwd dir)
    (let ((default-directory dir))
      (shell-pop arg))))

(el-patch-feature shell)
(with-eval-after-load 'shell
  (el-patch-defun shell-eval-command (command)
   "Eval COMMAND in the current shell process and return the result."
   (let* ((proc (get-buffer-process (current-buffer)))
          (old-filter (process-filter proc))
          (result "")
          prev)
     (unwind-protect
         (progn
           (set-process-filter
            proc
            (lambda (_proc string)
              (setq result (concat result string))))
           (process-send-string proc command)
           (while (el-patch-swap
                    (not (let* ((lines (string-lines result))
                                (last (car (last lines))))
                           (and (length> lines 0)
                                (not (equal last ""))
                                (or (not prev)
                                    (not (equal last prev)))
                                (setq prev last))))
                    (or (equal result "")
                        (string-suffix-p "\n" result)))
             (accept-process-output proc 0 100)))
       ;; Restore old filter.
       (set-process-filter proc old-filter))
     ;; Remove the prompt.
     (replace-regexp-in-string "\n.*\\'" "\n" result))))

(advice-add 'shell-eval-command
            :filter-return
  (defun $strip-ctrl-m (str)
    (replace-regexp-in-string "\r" "" str nil t)))

(defun $send-command (arg)
  "Send the current region or line to a process buffer."
  (interactive "P")
  (when-let* ((proc (thread-last (window-list)
                      (mapcar 'window-buffer)
                      (mapcar 'get-buffer-process)
                      (remove nil)
                      (car)))
              (beg (if (region-active-p)
                       (region-beginning)
                     (save-excursion (back-to-indentation) (point))))
              (end (if (region-active-p)
                       (region-end)
                     (line-end-position)))
              (cmd (buffer-substring beg end)))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert cmd)
      (comint-send-input))))

;; Shell mode does not understand progress bars since they are deleted and moved
;; to bottom of the screen every time they are updated. This code from
;; https://oremacs.com/2019/03/24/shell-apt/ will take progress bar
;; from a shell and display it in the minibuffer.
(advice-add
 'ansi-color-apply-on-region
 :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the minibuffer instead."
  (let ((end-marker (copy-marker end))
        mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (ora-apt-progress-message
           (substring-no-properties
            (delete-and-extract-region mb (point))
            2 -2)))))))

(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))

(use-package shell-pop
  :general
  ("C-c C-;" 'shell-pop)
  ("C-c C-:" '$shell-pop-root)
  :init
  (push (cons "\\*shell" display-buffer--same-window-action) display-buffer-alist)
  (setq shell-pop-restore-window-configuration nil)
  :config
  (add-hook 'shell-pop-in-after-hook 'evil-insert-state)
  (add-hook 'shell-pop-in-hook
            (defun $shell-pop-set-remote ()
              "properly handle remote paths in shell-pop"
              (let ((remote (file-remote-p default-directory)))
                (setq shell-pop-internal-mode-buffer
                      (format "*%sshell*" (or remote ""))))))
  (advice-add 'shell-pop--cd-to-cwd
              :before-until
              (defun $shell-in-cwd-p (cwd)
                "don't cd if we are already in that directory"
                (file-equal-p default-directory cwd)))
  (advice-add 'shell-pop--cd-to-cwd-shell
              :filter-args
              (defun $shell-pop-cd-fix-path (args)
                "fix remote paths during cd"
                (let* ((cwd (car args)))
                  (list (string-remove-prefix
                         (or (file-remote-p cwd) "")
                         cwd))))))

(defun $shell-pop-root (arg)
  "open a shell in the project root"
  (interactive "P")
  (let ((default-directory (vc-git-root default-directory)))
    (shell-pop arg)))

(use-package native-complete
  :compdef shell-mode
  :capf native-complete-at-point
  :init
  (with-eval-after-load 'shell
    (native-complete-setup-bash))
  :custom
  (native-complete-style-regex-alist
   `(("[A-Z]+> " . tab))))

(use-package company-native-complete
  :compdef shell-mode
  :company company-native-complete)

(use-package company-async-files
  :after company
  :ensure
  (:host github :repo "CeleritasCelery/company-async-files")
  :config
  (unless (memq 'company-async-files company-backends)
    (setq company-backends (append '(company-capf company-async-files) (cdr company-backends))))
  (add-to-list 'company-backends 'company-async-files))

;;;; compile

(use-package compile
  :ensure nil
  :general
  (:definer 'leader
   "o" '(:ignore t :wk "compile")
   "oc" '$compile
   "ob" 'bman-cmd/body
   "oi" '$run-ipgen
   "ot" '$run-turnin
   "os" '$run-simregress
   "oj" '$compilation-jump-to-buffer)
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :config
  (general-unbind compilation-mode-map "SPC"))

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    ;; If we don't use the controlmaster options, we need to input our password each compile
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-02/msg00731.html
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(defun $compile (cmd &optional comint)
  "Compile with model root set"
  (interactive (list (let ((file-name (buffer-file-name)))
                       (read-string "Compile Command: "
                                    (when file-name
                                      (let ((basename (file-name-nondirectory file-name)))
                                        (cond ((equal basename "Makefile") "make")
                                              ((file-executable-p file-name) (concat "./" basename))
                                              (t nil))))
                                    'compile-history))
                     (consp current-prefix-arg)))
  (let* ((model-root ($model-root))
         (shorten-fn (lambda (text) (match-string 1 text)))
         (cmd-name (thread-last cmd
                     (replace-regexp-in-string ($rx ^ "source " -> "&& ") "")
                     (replace-regexp-in-string ($rx "/" file "/"
                                                    (group (+ (in alnum "-_."))) symbol-end)
                                               shorten-fn)))
         (buffer-name (let ((root (f-filename model-root))
                            (dir (f-filename default-directory)))
                        (if (equal root dir)
                            (format "*%s - %s*" root cmd-name)
                          (format "*%s/.../%s - %s*" root dir cmd-name))))
         (env-var? (lambda (x) (string-match-p "=" x)))
         (parts (split-string-shell-command cmd))
         (final-cmd (mapconcat 'identity (-drop-while env-var? parts) " "))
         (compilation-environment (append (-take-while env-var? parts)
                                          (list (concat "MODEL_ROOT=" model-root))))
         (compilation-buffer-name-function (lambda (_mode) buffer-name)))
    (compile final-cmd comint)))

(defun $model-root (&optional dir)
  "current model root"
  (file-truename (expand-file-name (or (vc-git-root (or dir default-directory)) ""))))

;; compilation mode will throw warnings about clearing large buffers, but
;; we don't need undo in compilation buffers anyways so we can just turn
;; that off
(add-hook 'compilation-mode-hook 'buffer-disable-undo)

;;;;; keybindings
;; compillation can either be run in =compilation-mode= or in
;; =comint-mode= with =compilation-shell-minor-mode= enabled. The later
;; is needed when we want to interact with the running process. This has
;; a few issues that we are going to resolve now:

;; 1. There is a weird issue where the recompile buffer is not the same
;;    as the original buffer. Therefore we force the buffer name to
;;    remain the same by overriding the =compilation-arguments=.

;; 2. It is really convient to dismiss the compilation window with =q= so
;;    we are going to bind that. regular compilation-mode supports this
;;    binding by default.

;; 3. We want to start in normal state, so we easily dismiss the
;;    window with =q= right when it first pops up if we want.
(general-define-key
 :definer 'minor-mode
 :states 'normal
 :keymaps 'compilation-shell-minor-mode
 "q" 'quit-window
 "gr" 'recompile
 "gf" '$find-file-at-point)

(general-define-key
 :states 'normal
 :keymaps 'compilation-mode-map
 "gf" '$find-file-at-point)

(general-define-key
 :states 'normal
 :keymaps 'compilation-minor-mode-map
 "gr" 'revert-buffer)

(advice-add 'recompile :before (defun $set-recompile-buffer-name (_)
                                 (when compilation-arguments
                                   (setf (nth 2 compilation-arguments) (lambda (_) (buffer-name))))))

(add-hook 'compilation-shell-minor-mode-hook 'evil-normal-state)

(define-arx err-rx
  (append $rx-defaults
          '((fill (1+ (not (any space))))
            (filename (group-n 1 file))
            (line (group-n 2 nums))
            (col (group-n 3 nums))
            (info (opt "-I-:")))))

;;;;; errors
;; This adds a ton of regex that we don't need
(with-eval-after-load 'verilog-mode
    (remove-hook 'compilation-mode-hook 'verilog-error-regexp-add-emacs))

;;;;; dir
;; by setting the compliation root, we can ensure that we are only prompted to
;; save buffers that actaully exist in the project instead of it trying prompt
;; us to save all buffers.
(defvar-local $current-compilation-dir nil
  "root of current compliation")

(defun $set-compilation-dir (&rest _)
  "set the root of the current compilation"
  (setq $current-compilation-dir default-directory))

(defun $compilation-save-buffer-p ()
  (when-let ((name (buffer-file-name))
             (root (vc-git-root name))
             (comp-root (vc-git-root (or $current-compilation-dir
                                         default-directory))))
    (and (not (string-match-p (rx ".log" eos) (buffer-file-name)))
         (f-same? comp-root root))))

(setq compilation-save-buffers-predicate '$compilation-save-buffer-p)

(advice-add 'compilation-start :before #'$set-compilation-dir)

;;;;; timestamps
(defvar $compilation-start-time nil)
(make-variable-buffer-local '$compilation-start-time)

(defun $compilation-set-start-time (proc)
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local $compilation-start-time (current-time))))))

(add-hook 'compilation-start-hook '$compilation-set-start-time)

(add-hook 'compilation-finish-functions
          (defun $compilation-print-run-time (buffer _msg)
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-max))
                (insert (format " --- Total run time: %s ---\n"
                                ($time-difference-to-string
                                 (current-time) $compilation-start-time)))))))

(cl-defun $time-difference-to-string (time1 time2)
  "take the difference between two time stamps and print it"
  (let* ((names '("day" "hour" "minute" "second"))
         (decode (reverse (-take 4 (decode-time (time-subtract time1 time2) "UTC0"))))
         ;; since days starts at 1 (instead of 0) we subtract 1 from the first element
         (decode (cons (1- (car decode)) (cdr decode)))
         ;; units is the alist of (value . name) pairs. e.g. (1 . day) (4 . hour) etc.
         (units (-zip-pair decode names)))
    ;; The time difference was so small we didn't capture it
    (if (or (equal decode '(30 23 59 59))
            (equal decode '(0 0 0 0)))
        "less than a second"
      (while units
        ;; When we find the first non-zero unit we print it as well as
        ;; the following unit.
        (when-let ((formatted ($format-time-unit (pop units))))
          (cl-return-from $time-difference-to-string
            (s-join ", " (remove nil (list formatted ($format-time-unit (pop units)))))))))))

(defun $format-time-unit (unit)
  "return formatted time string if unit is not 0"
  (when (and unit
             (> (car unit) 0))
    (let ((value (car unit))
          (name (cdr unit)))
      (format "%d %s%s" value name (if (>= value 1) "s" "")))))

;;;;; stalled buffers
(defvar $compilation-stalled-buffers nil
  "buffers that have a stalled compilation")

(defun $alert-compilation-stall ()
  (when (looking-back (rx bol (+ alpha) "> ") (line-beginning-position))
    (add-to-list '$compilation-stalled-buffers (current-buffer))
    (alert "Compilation stalled"
           :severity 'moderate)))
(byte-compile #'$alert-compilation-stall)

(defun $compilation-remove-stalled-buffer (buffer _exit)
  (setq $compilation-stalled-buffers (delete buffer $compilation-stalled-buffers)))

(when ($dev-config-p)
  (add-hook 'compilation-filter-hook #'$alert-compilation-stall)
  (add-hook 'compilation-finish-functions #'$compilation-remove-stalled-buffer))

;;;;; status tracker

(defvar $compilation-finished-buffers nil
  "buffers that have finished compilation")

(add-hook 'compilation-finish-functions
          (defun $compilation-add-buffer (buffer msg)
            (push (cons buffer (replace-regexp-in-string "\n" "" msg))
                  $compilation-finished-buffers)))

;; Helper function for $compilation-buffer-candidates
(defun $compilation-proc-stalled (proc)
  (memq (process-buffer proc) $compilation-stalled-buffers))


(defun $compilation-format-candidate (buffer msg face)
  (cons (format "%-10s%s" (propertize msg 'face face) buffer)
        buffer))

(defun $compilation-buffers-candidates ()
  "show the status of all current compilations and allow easy
access"
  (setq $compilation-finished-buffers
        (seq-uniq $compilation-finished-buffers (lambda (x y) (eq (car x) (car y)))))
  ;; remove dead processes and buffers. If I kill buffers but they were still
  ;; running then they will never get updated. So we check for that at the start
  ;; of every status.
  (setq compilation-in-progress
        (cl-remove-if-not (lambda (proc)
                            (let ((buf (process-buffer proc)))
                              (and (buffer-live-p buf)
                                   (process-live-p proc))))
                          compilation-in-progress))

  (let* ((stalled-procs (cl-remove-if-not #'$compilation-proc-stalled compilation-in-progress))
         (running-procs (cl-remove-if #'$compilation-proc-stalled compilation-in-progress))
         (finished-buffers (cl-remove-if (-lambda ((buffer))
                                           (or (not (buffer-live-p buffer))
                                               (memq (get-buffer-process buffer) compilation-in-progress)))
                                         $compilation-finished-buffers))
         (formatted-stalled-buffers (--map ($compilation-format-candidate (process-buffer it) "stalled" 'compilation-warning)
                                           stalled-procs))
         (formatted-running-buffers (--map ($compilation-format-candidate (process-buffer it) "running" 'compilation-line-number)
                                           running-procs))
         (formatted-finished-buffers (--map (let* ((buffer (car it))
                                                   (msg (cdr it))
                                                   (face (if (string-prefix-p "exited abnormally" msg)
                                                             'compilation-error
                                                           'compilation-info)))
                                              ($compilation-format-candidate buffer (string-remove-prefix "exited abnormally with " msg) face))
                                            finished-buffers)))
    (append formatted-stalled-buffers formatted-finished-buffers formatted-running-buffers)))

(defun $compilation-jump-to-buffer ()
  "select from active and finished compilation buffers"
  (interactive)
  (let ((buffers ($compilation-buffers-candidates)))
    (switch-to-buffer (cdr (assoc (completing-read "jump to buffer: "  buffers) buffers)))))

;;;;; alerts
(add-hook 'compilation-finish-functions
          (defun $notify-compile-done (_buffer exit-string)
            "notfiy the user that compliation is finished"
            (alert "compliation finished"
                   :severity (if (string-prefix-p "exited abnormally" exit-string)
                                 'high
                               'normal))))

(defun $clear-alert ()
  "clear persistent alert"
  (interactive)
  (alert ""))

($leader-set-key
  "oa" '$clear-alert)

;;;;; font lock
(define-arx log-rx
  (append $rx-defaults
          '((I (: bol (opt "-I-")))
            (I: (: bol (opt "-I-:"))))))

(defvar $compilation-font-lock-keywords
  `((,(log-rx I: spc* (group (in "#*") (in " \t#*=") ->) eol)
     1 'compilation-line-number)
    (,(log-rx I: spc* (group (repeat 4 "=") ->) eol)
     1 'compilation-line-number)
    (,(log-rx I spc* (group (repeat 3 "-") ->) eol)
     1 'compilation-line-number)
    (,(log-rx I (opt ":")  (group (not (in "-")) (1+ (in alnum " \t_-"))) ": ")
     1 'font-lock-function-name-face)
    (,(log-rx I: (group (or "Note-" "NOTE" "OVM_INFO" "UVM_INFO")))
     1 'compilation-info prepend)
    (,(log-rx I: (group (or "%Error:" "Error:" "Error-" "Fatal-" "OVM_ERROR" "UVM_ERROR" "ERROR" "FATAL" "OVM_FATAL" "UVM_FATAL")))
     1 'compilation-error prepend)
    (,(log-rx I: (group (or "Warning-" "WARNING" "OVM_WARNING")))
     1 'compilation-warning prepend)))


(font-lock-add-keywords 'compilation-mode $compilation-font-lock-keywords)
(font-lock-add-keywords 'log-mode $compilation-font-lock-keywords)
(use-package alert
  :init
  (setq alert-default-style 'fringe))

(use-package detached)

;;; Org

(use-package org
  :gfhook #'$org-truncate-lines #'toggle-word-wrap #'visual-fill-column-mode
  :ensure nil
  :general
  (:definer 'leader
   :keymaps 'org-mode-map
   "," 'org-edit-special
   "g" '$org-navigate/body)
  (:definer 'leader
   "ol" 'org-store-link)
  :custom
  (org-lowest-priority ?D)
  (org-enforce-todo-dependencies t)
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "PENDING(p)" "|" "DONE(d)")
                       (sequence "READING(r)" "|" "READ(d)")
                       (sequence "|" "CANCELED(c@)")))
  (org-priority-faces '((?A . (:foreground "OrangeRed"))
                        (?B . (:foreground "yellow3"))
                        (?C . (:foreground "ForestGreen"))))
  :init
  (defun $org-truncate-lines ()
    (let ((inhibit-message t))
      (toggle-truncate-lines)))
  :config
  (setf (alist-get 'file org-link-frame-setup) 'find-file)
  (general-advice-add '(org-insert-subheading org-insert-todo-subheading)
                      :before
                      (defun $org-insert-subheading (_arg)
                        (let ((undo-inhibit-record-point t))
                          (end-of-line))))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (general-def org-mode-map
    "C-c C-<return>" 'org-insert-subheading
    "C-c C-SPC" 'org-insert-todo-subheading)
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp")))

(use-package visual-fill-column)

(defun $org-insert-header-priority (_arg &optional force)
  (when (org-at-heading-p)
    (let ((parent-priority (save-excursion (if force
                                               (org-up-heading-safe)
                                             (org-backward-heading-same-level 1))
                                           (org-show-priority))))
      (org-todo "TODO")
      (cl-loop do (org-priority (if (string< (org-show-priority) parent-priority) 'up 'down))
               until (equal parent-priority (org-show-priority))))))

(advice-add 'org-insert-todo-heading :after '$org-insert-header-priority)

(defhydra $org-navigate ()
  "navigate through org headers"
  ("J" org-next-visible-heading "next heading")
  ("K" org-previous-visible-heading "prev heading")
  ("j" org-forward-element "foward")
  ("k" org-backward-element "back")
  ("h" org-up-element "up")
  ("l" org-down-element "down")
  ("q" nil)
  ("<tab>" org-cycle "cycle"))

(defun $org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (outline-show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))
(advice-add 'counsel-org-goto :after '$org-show-current-heading-tidily)


(defun $org-archive-done-tasks ()
  "move tasks that are completed to the archive file."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELED" 'file))

($leader-local-set-key
  :keymaps 'org-mode-map
  "a" '$org-archive-done-tasks
  "h" '$org-show-current-heading-tidily)

(defun $org-procrastinate (arg)
  "shedule the selected item for tomrrow, effectivly removing
it from todays agenda."
  (interactive "P")
  (let ((fn (if (eq major-mode 'org-agenda-mode)
                'org-agenda-schedule
              'org-schedule)))
    (funcall fn arg "+1d")))
($leader-local-set-key
  :keymaps '(org-mode-map org-agenda-mode-map)
  "s" '$org-procrastinate)

;; Fix ~org-open-file~ on WSL. Based on
;; [[https://vxlabs.com/2020/03/07/patch-emacs-org-open-file-using-advice/][this]]
;; link
(defun wsl-fix-org-open-file (orig-org-open-file &rest args)
  ;; temporarily replace function,
  ;; see https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'start-process-shell-command) #'call-process-shell-command))
    (apply orig-org-open-file args)))

(advice-add #'org-open-file :around #'wsl-fix-org-open-file)

(csetq org-agenda-todo-ignore-scheduled 'future
       org-agenda-tags-todo-honor-ignore-options t
       org-agenda-dim-blocked-tasks 'invisible)

($leader-set-key
  "a" 'org-agenda)
(general-def org-agenda-mode-map "o" 'org-agenda-log-mode)

(defun $org-agenda-next-visual-line ()
  (interactive)
  (evil-next-visual-line)
  (org-agenda-do-context-action))
(defun $org-agenda-prev-visual-line ()
  (interactive)
  (evil-previous-visual-line)
  (org-agenda-do-context-action))

(add-hook 'org-agenda-mode-hook
          (defun $org-agenda-visual-lines ()
            (general-def 'motion org-agenda-mode-map
              "j" '$org-agenda-next-visual-line
              "k" '$org-agenda-prev-visual-line))
          'append)

(csetq org-stuck-projects
       '("/PROJ"
         ("TODO")
         nil ""))

(csetq org-refile-use-cache t)

(csetq org-refile-use-outline-path 'file
       org-outline-path-complete-in-steps nil)

(setq org-export-with-section-numbers nil)

(setq org-export-with-toc nil)

;; only make something a subscript if we had braces around
;; it. i.e. make 5_{4} a subscript but not 5_4. Otherwise we end up
;; with subscripts anytime we don't markup a variable name.
(setq org-export-with-sub-superscripts '{})

(setq org-html-postamble nil)

;; I program on a dark background because I am not heathen. But this
;; means that when we use the default inline CSS for syntax highlighting
;; we get really hard to read colors on a white background like email. So
;; instead org will just define css elements for the different syntax
;; elements and our =email.css= can define the colors for those. that way
;; we always have good colors for reading on white background regardless
;; of our theme.
(setq org-html-htmlize-output-type 'css)

(setq org-html-checkbox-type 'unicode)

(with-eval-after-load 'ox-html
  (setf (alist-get 'verbatim org-html-text-markup-alist)  "<kbd>%s</kbd>"))

(defun $org-export-buffer ()
  "Export the current org email and copy it to the clipboard"
  (interactive)
  (defvar org-export-show-temporary-export-buffer)
  (let (org-export-show-temporary-export-buffer)
    (org-html-export-as-html)
    (with-current-buffer "*Org HTML Export*"
      (kill-new (buffer-string)))
    (message "HTML copied to clipboard")))

(defun $export-code-region (beg end)
  "Export the current region as formatted HTML"
  (interactive "r")
  (let ((region (buffer-substring-no-properties beg end))
        (mode (string-remove-suffix "-mode" (symbol-name major-mode))))
    (with-temp-buffer
      (insert "#+BEGIN_SRC " mode "\n")
      (insert region "\n")
      (insert "#+END_SRC\n")
      ($org-export-buffer))))

($leader-set-key
  "xe" '$export-code-region)

(use-package htmlize)

(unless ($dev-config-p)
  (use-package ox-hugo))

(use-package org-capture
  :ensure nil
  :init
  ($leader-set-key
    "c" 'org-capture)
  ($leader-local-set-key
    :definer 'minor-mode
    :keymaps 'org-capture-mode
    "e" '$org-export-buffer)
  :config
  (setq
   $org-inbox-file (expand-file-name "inbox.org" org-directory)
   org-default-journal-file (expand-file-name "journal.org" org-directory)
   org-default-meeting-file (expand-file-name "meeting.org" org-directory)
   org-coding-problem-file (expand-file-name "personal/daily-coding-problems.org" org-directory)
   org-capture-templates
   (mapcar (lambda (x) (append x '(:empty-lines 1)))
           '(("t" "Todo" entry (file $org-inbox-file)
              "* TODO [#C] %?\n")
             ("l" "Todo Link" entry (file $org-inbox-file)
              "* TODO [#C] %?\n %i\n %a")
             ("s" "Scheduled TODO" entry (file $org-inbox-file)
              "* TODO [#C] %?\n  SCHEDULED: %^T\n")
             ("T" "Todo from Clipboard" entry (file $org-inbox-file)
              "* TODO [#C] %?\n%c")
             ("n" "Note" entry (file $org-inbox-file)
              "* %?")
             ("N" "Note with Clipboard" entry (file $org-inbox-file)
              "* %?\n   %c")
             ("i" "Interupt" entry (file $org-inbox-file)
              "* TODO [#C] %?\n" :clock-in t :clock-keep t)
             ("j" "Journal" entry (file org-default-journal-file)
              "* %<%a %b %e, %l:%M> -  %?")
             ("d" "Daily Coding Problem" entry (file org-coding-problem-file)
              "* TODO Problem #%($problem-number)\n%c%?")
             ("m" "Meeting" entry (file org-default-meeting-file)
              "* %?" :clock-in t :clock-resume t)))))

(defun $problem-number ()
  (with-current-buffer (find-file-noselect org-coding-problem-file)
    (save-excursion
      (goto-char (point-max))
      (org-previous-visible-heading 1)
      (if (re-search-forward (rx (+ digit)) (line-end-position) t)
          (number-to-string
           (1+ (string-to-number (match-string-no-properties 0))))
        "0"))))

;;;; roam
(unless ($dev-config-p)
  ;; Do a deep clone to work around version issue
  ;; https://github.com/progfolio/elpaca/issues/353
  (use-package emacsql :ensure (:main nil :depth nil))
  (use-package org-roam
    :init
    (setq org-roam-directory (expand-file-name "~/roam")
          org-roam-dailies-directory "journals/"
          org-roam-database-connector 'sqlite-builtin
          org-roam-db-location "~/.emacs.d/var/org/my-roam.db"
          org-roam-capture-templates
          '(("d" "default" plain
             "%?" :target
             (file+head "pages/${slug}.org" "#+title: ${title}\n")))
          org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %?" :target
             (file+head "%<%Y_%m_%d>.org" "#+title: %<%b %o, %Y>\n"))))
    :bind (:map org-mode-map
           (("C-c n i" . org-roam-node-insert)
            ("C-c n b" . org-roam-buffer-toggle)))
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n d" . org-roam-dailies-goto-today))
    :config
    (org-roam-setup)))

(defun format-time-ordinal (n)
  "Special day of month format."
  (format
   (concat
    "%d"
    (if (memq n '(11 12 13)) "th"
      (let ((last-digit (% n 10)))
        (cl-case last-digit
          (1 "st")
          (2 "nd")
          (3 "rd")
          (otherwise "th"))))) n))

(defun format-time-string-insert-ordinal-day (args)
  (let* ((format-string (nth 0 args))
         (time (nth 1 args))
         (day (nth 3 (decode-time (or time (current-time))))))
    (setcar args
            (replace-regexp-in-string "%o"
                                      (format-time-ordinal day)
                                      format-string)))
  args)

(advice-add #'format-time-string
            :filter-args
            'format-time-string-insert-ordinal-day)

(csetq org-tags-column 0
       org-fast-tag-selection-single-key t)

(defun $org-copy-url ()
  "in my current setup, the builtin browswer does not
work, so I copy links and paste them into chrome."
  (let ((context (org-element-context)))
    (when (member (org-element-property :type context)
                  '("http" "https"))
      (message "copied org link: %s"
               (kill-new (org-element-property :raw-link context))))))

(setq org-return-follows-link t)

;;;; editing

(use-package jinx)
(add-hook 'org-mode-hook 'jinx-mode)
(add-hook 'with-editor-mode-hook 'jinx-mode)

(add-hook 'org-mode-hook
          (defun $enable-company-spell ()
            (setq-local company-backends '(company-capf (company-ispell company-dabbrev)))
            (company-mode)))

(use-package org-variable-pitch
  :hook (org-mode . org-variable-pitch-minor-mode)
  :init
  (set-face-attribute 'variable-pitch nil
                      :font (if (eq system-type 'windows-nt)
                                "Calibri"
                              "Source Sans Pro")
                      :height (+ 10 $font-height))
  (setq org-variable-pitch-fixed-font
        (if (eq system-type 'windows-nt)
            "Consolas"
          "Source Code Pro"))
  :config
  (set-face-attribute 'org-variable-pitch-face nil :height 0.9))

(use-package org-src
  :ensure nil
  :gfhook #'$org-src-lexical-bindings
  :custom
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively t)
  (org-babel-default-header-args:emacs-lisp '((:lexical . "yes"))))

(defun $org-src-lexical-bindings ()
      (setq-local lexical-binding t))

(general-advice-add '(org-edit-src-exit org-edit-src-abort) :after 'evil-normal-state)

(with-eval-after-load 'org
  (csetq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages '((perl . t)
                               (shell . t)
                               (python . t)
                               (emacs-lisp . t)
                               (calc . t))))

(use-package evil-org
  :ensure (:host github :repo "hlissner/evil-org-mode")
  :hook org-mode
  :custom
  (evil-org-key-theme '(navigation insert return textobjects additional calendar))
  :init
  (add-hook 'org-insert-heading-hook 'evil-insert-state)
  :config
  (evil-org-set-key-theme)
  (evil-define-key 'normal evil-org-mode-map (kbd "RET") #'evil-org-return)
  (general-def 'normal 'org-mode-map
    "[[" '$org-headings/org-previous-visible-heading
    "]]" '$org-headings/org-next-visible-heading))


(defhydra $org-headings (:exit nil)
  ("[" org-previous-visible-heading  "prev")
  ("]" org-next-visible-heading "next"))

(use-package evil-org-agenda
  :ensure nil
  :demand t
  :after org-agenda
  :config (evil-org-agenda-set-keys)
  :hook (org-agenda-mode . evil-org-agenda-mode))

(defun $org-todo-or-evil-t ()
  "change the org todo state or evil's till operator"
  (interactive)
  (if (org-at-heading-p)
      (org-todo)
    (evil-find-char-to)))

(general-def 'normal org-mode-map
  "t" '$org-todo-or-evil-t)

(use-package org-noter
  :general
  ('motion
   doc-view-mode-map
   "i" 'org-noter-insert-note))

(use-package org-bullets
  :hook org-mode)

(use-package org-fancy-priorities
  :hook org-mode)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region
                                         (match-beginning 1)
                                         (match-end 1)
                                         "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region
                                         (match-beginning 1)
                                         (match-end 1)
                                         "◦"))))))

(defun $prettify-src-block ()
  "Remove delimeter text from org src blocks"
  (add-to-list 'prettify-symbols-alist '("#+BEGIN_SRC" . "†"))
  (add-to-list 'prettify-symbols-alist '("#+END_SRC" . "†"))
  (add-to-list 'prettify-symbols-alist '("#+begin_src" . "†"))
  (add-to-list 'prettify-symbols-alist '("#+end_src" . "†"))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook '$prettify-src-block)

;;;; Languages

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package flycheck
  :init
  ($leader-set-key
    "tf" 'flycheck-mode)
  :general
  (:keymaps 'flycheck-error-list-mode-map
   "q" 'quit-window))

(defhydra $flycheck (:exit nil)
  "navigate by flycheck errors"
  ("n" flycheck-next-error "next")
  ("p" flycheck-previous-error "prev")
  ("N" flycheck-previous-error "prev")
  ("e" flycheck-list-errors "list" :exit t))

($leader-set-key
  "l" '$flycheck/body)

(csetq flycheck-shellcheck-excluded-warnings '("SC2086" "SC2046"))

(use-package company
  :general
  (:keymaps 'company-active-map
   "RET" nil
   "TAB" nil
   [tab] nil
   [return] nil
   "C-w" nil
   "C-," 'counsel-company
   "C-l" 'company-complete-selection)
  (:definer 'leader
   "tc" 'company-mode)
  :hook '(prog-mode org-mode ielm-mode)
  :custom
  (company-idle-delay 0.2)
  (evil-collection-company-use-tng nil)
  (company-dabbrev-downcase nil)
  (company-require-match nil)
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-ignore-case t)
  (company-backends '((company-capf company-dabbrev-code company-keywords)
                      company-dabbrev))
  :config
  (advice-add 'company-select-previous :before-until #'$company-select-prev-or-comint-match-input))

(defun $company-select-prev-or-comint-match-input (&optional _)
  "Disable company mode when when we are selecting a previous
prompt in shell mode"
  (when (and (eq major-mode 'shell-mode)
             (eq company-selection 0))
    (company-abort)
    (call-interactively 'comint-previous-matching-input-from-input)))

(with-eval-after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'shell-mode))

(use-package company-posframe)

;; https://github.com/doomemacs/doomemacs/commit/2e476de44693c9f4953f3c467284e88b28b6084e
(add-hook 'evil-local-mode-hook
          (defun $fix-evil-company-keymap-conflict ()
            (when (memq 'company-emulation-alist emulation-mode-map-alists)
              (company-ensure-emulation-alist))))

(use-package company-statistics
  :demand t
  :after company
  :config
  (company-statistics-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css" "v0.20.0")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json" "v0.20.2")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
        (swift "https://github.com/alex-pinkus/tree-sitter-swift")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (verilog "https://github.com/gmlarumbe/tree-sitter-systemverilog")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;;;; elisp

(use-package elisp-mode
  :ensure nil
  :custom
  (eval-expression-print-length nil)
  :init
  ($leader-local-set-key
    :keymaps 'emacs-lisp-mode-map
    "ee" 'eval-last-sexp
    "eb" 'eval-buffer
    "er" 'eval-region
    "ef" 'eval-defun)
  :custom
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local tab-width 8))))

(defun $prettify-cons ()
  "make cons cells and lambda better formatted in elisp"
  (add-to-list 'prettify-symbols-alist '("lambda" . ?λ))
  (add-to-list 'prettify-symbols-alist '("." . ?•))
  (prettify-symbols-mode -1)
  (prettify-symbols-mode 1)
  (setq-local prettify-symbols-compose-predicate
              (defun $prettify-symbols-handle-dot (start end match)
                (if (equal "." match)
                    (prettify-symbols-default-compose-p start end match)
                  ($prettify-symbols-all-p start end match)))))

(add-hook 'emacs-lisp-mode-hook '$prettify-cons)

(use-package page-break-lines
  :hook (emacs-lisp-mode help-mode))

(use-package aggressive-indent
  :ensure
  (:fork "CeleritasCelery/aggressive-indent-mode"))

(defun $lisp-indent-function (indent-point state)
  "Override `lisp-indent-function' to properly handle plists. See the original function fo full description"
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(setq lisp-indent-function '$lisp-indent-function)

(use-package lisp-extra-font-lock
  :hook emacs-lisp-mode
  :config
  (dolist (fn '("when-let" "when-let*" "if-let" "if-let*" "-let" "-let*"))
    (add-to-list 'lisp-extra-font-lock-let-functions fn)))

(use-package macrostep
  :init
  (defhydra macrostep (:exit nil :foreign-keys run)
    "expand macros one step at a time"
    ("e" macrostep-expand "expand")
    ("c" macrostep-collapse "collapse")
    ("n" macrostep-next-macro "next")
    ("N" macrostep-prev-macro "prev")
    ("q" macrostep-collapse-all "quit" :exit t))
  ($leader-local-set-key
    :keymaps 'emacs-lisp-mode-map
    "m" 'macrostep/body))

(use-package epdh
  :ensure (:host github :repo "alphapapa/emacs-package-dev-handbook"))

(defmacro $profile (&rest body)
  "generate a CPU profile report for BODY"
  (require 'profiler)
  (when (profiler-running-p)
    (profiler-stop)
    (profiler-reset))
  `(progn (profiler-start 'cpu)
          ,@body
          (profiler-report)
          (profiler-stop)
          (profiler-reset)))

(defun $set-keyboard-quit ()
  (setq unread-command-events
        (mapcar (lambda (e) `(t . ,e))
                (listify-key-sequence (kbd "C-g")))))

;; cperl-mode is considered by the better perl mode, and it certainly has
;; more features. However I prefer to use perl-mode for the following
;; reasons:
;; 1. the font lock cperl HERE docs and perldocs are broken. If you
;;    insert a newline in the middle of one of those multiline
;;    constructs, it will break for the rest of the document
;; 2. cperl overhighlights, it tries to highlight array's and hashes in
;;    comments, and hash keys are highlighted in the same face as has
;;    values. Makes it harder to read
;; 3. cperl defines a custom face for arrays and hashs that is really
;;    jarring. It can be overriden, but you have to do that for every
;;    theme that you are using
;; 4. I don't really use any of the features in cperl mode, so I don't
;;    think it is worth using

;; I use regular perl-mode for files that are considered read-only.
(use-package perl-mode
  :ensure nil
  :mode (rx "." (or "ip_info" "espflist" "udf" "hdl" "map") eos)
  :general
  (:definer 'leader
   :keymaps 'perl-mode-map
   "r" 'quickrun)
  :init
  (setq perl-indent-parens-as-block t
        perl-continued-brace-offset 0
        perl-continued-statement-offset 0))

;; We also change : to be a punctuation character to match perl mode.
;; This fixes a ligature issue.
(use-package cperl-mode
  :ensure nil
  :init
  (setq
   ;; highlight all scalar variables not just the instantiation
   cperl-highlight-variables-indiscriminately t
   cperl-indent-level 4        ; 4 spaces is the standard indentation
   cperl-close-paren-offset -4 ; indent the closing paren back four spaces
   cperl-continued-statement-offset 4 ; if a statement continues indent it to four spaces
   cperl-indent-parens-as-block t)
  :config
  (modify-syntax-entry ?: "." cperl-mode-syntax-table))

(setq flycheck-perl-executable "/usr/intel/pkgs/perl/5.14.1/bin/perl"
        flycheck-perl-perlcritic-executable "/usr/intel/pkgs/perl/5.14.1-threads/bin/perlcritic"
        flycheck-perl-include-path '("/p/hdk/cad/spf/latest/lib/perl5"
                                     "../lib/perl5"
                                     "../../lib/perl5"
                                     ".."))
    (setenv "SPF_ROOT" "/p/hdk/cad/spf/latest")
    (setenv "SPF_PERL_LIB" "/p/hdk/cad/spf/latest/lib/perl5")
    (setenv "XWEAVE_REPO_ROOT" "/p/hdk/rtl/ip_releases/shdk74/xweave/v17ww43a")
    (setenv "IDS_HOME" "/p/hdk/rtl/cad/x86-64_linux26/dteg/ideas_shell/0.15.1")

(setq $string-interpolation-keywords
      `((,(rx (not (in "\\")) (group-n 1 "$" (opt "{")) (group-n 2 (1+ (any alnum "_"))) (group-n 3 (opt "}")))
         (1 (when (not (nth 4 (syntax-ppss)))
              'default)
            prepend)
         (2 (when (not (nth 4 (syntax-ppss)))
              font-lock-variable-name-face)
            prepend)
         (3 (when (not (nth 4 (syntax-ppss)))
              'default)
            prepend))))

(font-lock-add-keywords 'perl-mode $string-interpolation-keywords)

;; (use-package perltidy
;;   :general
;;   (:definer 'leader
;;    :keymaps '(perl-mode-map
;;               cperl-mode-map)
;;    "f" '(:ignore t :wk "format")
;;    "fr" 'perltidy-region
;;    "ff" 'perltidy-dwim-safe
;;    "fb" 'perltidy-buffer
;;    "fs" 'perltidy-subroutine))

;;;; Python
(setq python-prettify-symbols-alist '(("lambda" . ?λ)))

(use-package yapfify)
(use-package blacken
  :init
  (setq blacken-line-length 'fill))

(use-package live-py-mode
  :custom
  (live-py-version $python-executable))

(use-package python
  :ensure nil
  :compdef python-mode
  :company (company-capf company-dabbrev-code)
  :config
  (add-hook 'python-base-mode-hook #'$lsp-unless-remote)
  (unless ($dev-config-p)
    (add-hook 'python-base-mode-hook #'copilot-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(defun $lsp-unless-remote ()
  (if (file-remote-p buffer-file-name)
      (progn (eldoc-mode -1)
             (setq-local completion-at-point-functions nil))
    (lsp)))

(use-package lsp-jedi :demand t :after python)
(use-package lsp-pyright :demand t :after python)
(setq lsp-disabled-clients '(pyright-tramp))

(setq lsp-pylsp-plugins-flake8-enabled nil
      lsp-pylsp-plugins-pydocstyle-enabled nil
      lsp-pylsp-plugins-pyflakes-enabled t)

(defun $python-shell ()
  (interactive)
  (unless (python-shell-get-process)
    (run-python))
  (python-shell-switch-to-shell t))

(general-def 'normal python-mode-map "gz" #'$python-shell)

;;;; Rust
(setq-default flycheck-rust-crate-type nil)
(use-package rustic
  :gfhook #'copilot-mode
  :general
  (:definer 'leader :keymaps 'rustic-mode-map
   "m" 'lsp-rust-analyzer-expand-macro)
  (:states '(normal) :keymaps 'comint-mode-map
   "q" 'quit-window)
  (:states '(normal) :keymaps 'rustic-mode-map
   "J" 'lsp-rust-analyzer-join-lines)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-lens-run-enable nil)
  (lsp-rust-analyzer-lens-debug-enable nil)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-clippy-preference "on")
  (rustic-compile-backtrace "1")
  (lsp-rust-analyzer-diagnostics-disabled ["inactive-code" "incorrect-ident-case"])
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-default-test-arguments "--tests --all-features")
  :config
  (setenv "RUST_BACKTRACE" "full")
  (csetq rustic-ansi-faces (cl-map 'vector (lambda (x) (face-attribute x :foreground)) ansi-color-normal-colors-vector))
  (csetq rustic-clippy-arguments "--all-targets --all-features"))

(evil-initial-state 'rustic-popup-mode 'emacs)

(when (version<= "29.1" emacs-version)
  (add-to-list 'major-mode-remap-alist '(rust-ts-mode . rustic-mode)))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("rust" . rustic)))

;;;; C
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

;;; Verilog
(use-package verilog-mode
  :init
  (setq verilog-auto-indent-on-newline nil)
  (setq verilog-indent-lists nil)
  :config
  (add-to-list 'verilog-imenu-generic-expression
               `("*Instances*" ,($rx ^ spc+ (or (: (opt "#(") "." symbol "(" -> "))")
                                                (: symbol "_" symbol))
                                     spc+ (group symbol) (or eol (: spc+ "("))) 1)))

(use-package verilog-ext
  :hook ((verilog-ts-mode . verilog-ext-mode))
  :init
  (setq verilog-ext-feature-list
      '(font-lock
        xref
        capf
        hierarchy
        lsp
        beautify
        navigation
        template
        formatter
        compilation
        imenu
        which-func
        hideshow
        typedefs
        ports)))

(use-package verilog-ts-mode)
(add-to-list 'auto-mode-alist (cons (rx "." (or "v" "sv" "svh" "sv09" "sv_ts" "sv.dft") eos) 'verilog-ts-mode))

(define-derived-mode filelist-mode prog-mode "FileList"
  "Major mode for filelists"
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(filelist-font-lock-keywords))
  (modify-syntax-entry ?/ ". 12b" filelist-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" filelist-mode-syntax-table))

(setq filelist-font-lock-keywords
      `((,($rx bol spc* "-f" (1+ any)) 0 font-lock-function-name-face)
        (,($rx bol spc* "+"  (1+ any)) 0 font-lock-builtin-face)))

(add-to-list 'auto-mode-alist '("\\.f\\'" . filelist-mode))

;; Ligatures in verilog are more complicated then in other languages
;; because the symbol <= can be either a left arrow or a "less then or
;; equal" symbol. So we add an addition wrapper around the compose
;; function to handle this special case.
(defun $compose-conditional-symbol (alist)
  (or (and (memq major-mode '(verilog-mode verilog-ts-mode))
           (equal (match-string 0) "<=")
           (not (looking-at-p (rx (or (: (0+ " ") "(" )
                                      (: (1+ (not (in "(\n"))) ")")))))
           `((("<=" . (?\s (Br . Bl) ?\s (Br . Br)
                           ,(decode-char 'ucs #xEF87))))) )
      alist))

(with-eval-after-load 'prog-mode
  (advice-add 'prettify-symbols--compose-symbol :filter-args #'$compose-conditional-symbol))

(font-lock-add-keywords 'verilog-mode
                        `(("'" . 'error)
                          (,(rx (or bol "'h" "'b" "'d" (not (any "_" alnum)))
                                (group (1+ digit)))
                           1 font-lock-constant-face)))

(use-package dtrt-indent
  :hook verilog-mode
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list '(verilog-mode c/c++/java verilog-indent-level)))

;;;; TCL
(use-package tcl-mode
  :ensure nil
  :gfhook #'$tcl-fix-symbol-def #'flycheck-mode
  :mode (rx "." (or "upf" "pdl" "dofile" "do" "tcl" "iprocs") eos)
  :company ((company-syntcl company-dabbrev-code) (company-capf company-dabbrev))
  :general
  ('normal tcl-mode-map "gz" 'inferior-tcl)
  (tcl-mode-map "TAB" 'indent-for-tab-command)
  (tcl-mode-map "C-<return>" '$send-command)
  :custom
  (flycheck-tcl-nagelfar-syntaxdb-file "~/custom/tcl_json_files/TclComplete/syntaxdb_tessent.tcl")
  (tcl-application "tclsh")
  :init
  (setq tcl-proc-list '("proc" "method" "class" "namespace" "iProc" "iTopProc" "tepam::procedure")))

(with-eval-after-load 'tcl
  (add-to-list 'tcl-keyword-list "iProc")
  (add-to-list 'tcl-keyword-list "iTopProc")
  (add-to-list 'tcl-keyword-list "tepam::procedure")
  (add-to-list 'tcl-typeword-list "iProcsForModule")
  (setq tcl-builtin-list (append tcl-builtin-list
                                 '("iWrite" "iRead" "iApply" "iCall" "iTake" "iNote" "iRunLoop")))
  (tcl-set-font-lock-keywords)
  (font-lock-add-keywords 'tcl-mode $string-interpolation-keywords))

(defun $tcl-fix-symbol-def ()
  (modify-syntax-entry ?$ "." tcl-mode-syntax-table))

;; (when ($dev-config-p)
;;   (use-package company-syntcl
;;     :ensure
;;     (:repo "https://github.com/tjhinckl/company-syntcl.git"
;;      :files ("company-syntcl.el"))
;;     :custom
;;     (company-syntcl-dir "~/custom/TclComplete")
;;     :config
;;     (defun company-syntcl--annotation (_) nil)))

;;;; ICL

(define-derived-mode icl-mode java-mode "ICL"
  (setq-local c-basic-offset 3)
  (setq-local indent-line-function 'icl-indent-line)
  (setq-local font-lock-defaults '(icl-font-lock-keywords))
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local imenu-generic-expression
              `(("modules" ,($rx ^ "Module"
                                 spc+ (group symbol) spc+ "{") 1)
                ("instances" ,($rx ^ spc+ "Instance"
                                   spc+ (group symbol) spc+ "Of") 1)))
  (modify-syntax-entry ?\' "." icl-mode-syntax-table)
  (modify-syntax-entry ?$ "." icl-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.icl\\'" . icl-mode))

(defun icl-broken-line-p ()
  (save-excursion
    (previous-line)
    (end-of-line)
    (skip-syntax-backward " " (line-beginning-position))
    (save-match-data
      (looking-back (rx (or "Of" "SelectedBy" "=")) (line-beginning-position)))))

(defun icl-indent-line ()
  (interactive)
  (if (icl-broken-line-p)
      (let ((c-basic-offset 4))
        (c-indent-line))
    (c-indent-line)))

(setq icl-font-lock-keywords
      `((,(rx symbol-start "Attribute" symbol-end) 0 font-lock-variable-name-face)
        (,(rx symbol-start (or "Instance" "Module" "Enum") symbol-end)
         0 font-lock-function-name-face)
        (,($rx ^ spc* (group upper (1+ alnum)) spc+) 1 font-lock-keyword-face)
        (,(rx symbol-start (or "InputPort"
                               "Alias"
                               "ClockMux"
                               "DataMux"
                               "ScanMux")
              symbol-end)
         0 font-lock-keyword-face)
        (,(rx (char " [':") (group (opt (char "bh")) (1+ digit))) 1 font-lock-type-face)
        (,(rx ".") 0 'error)
        (,($rx spc+ (group (or "Of" "SelectedBy"))) 1 font-lock-builtin-face)))

(define-derived-mode tessent-spec-mode java-mode "Tessent Spec"
  (setq-local c-basic-offset 2))

(add-to-list 'auto-mode-alist
             `(,(rx (or "meta_spec" "tessent_meta") eos) . tessent-spec-mode))

;;;; JSON
(use-package json-mode
  :gfhook 'flycheck-mode 'hs-minor-mode
  :config
  (add-to-list 'hs-special-modes-alist (list 'json-mode (rx (any "{[")) (rx (any "]}")) (rx "/" (any "/*"))))
  (font-lock-add-keywords 'json-mode
                          `((,(rx (group "//" (0+ nonl)) eol) 1 font-lock-comment-face))))

;;;; Other
(add-hook 'css-mode-hook
          (defun $setup-css-mode ()
            (setq tab-width 4)))

(use-package csv-mode
  :general
  (:definer 'leader
   :keymaps 'csv-mode-map
   "a" 'csv-align-mode))

(add-to-list 'interpreter-mode-alist '("gmake" . makefile-gmake-mode))

(font-lock-add-keywords 'sh-mode
                        `((,($rx spc (group (>= 1 (in "-")) symbol))
                           1 font-lock-constant-face)))

(use-package bazel
  :config
  ;; this is really slow
  ;; https://github.com/bazelbuild/emacs-bazel-mode/issues/423
  (when-let ((ffap (rassoc 'bazel-mode-ffap ffap-alist)))
    (setq ffap-alist (remove ffap ffap-alist)))
  ;; make bazel projects higher priority
  (setq project-find-functions
        (cons 'bazel-find-project (remove 'bazel-find-project project-find-functions))))

(defun $clear-bazel-progress-bar (orig start end)
  "Bazel uses the following terminal sequence to clear the progress
messages. We want to handle these in our terminal so we don't get
redundant output."
  (save-excursion
    (goto-char start)
    (while (search-forward "\r\e[1A\e[K" nil t)
      (unless (eq (line-beginning-position) (point-min))
        (let ((seq-end (point)))
          ;; need to call line-move first, because it does not stay at the start of the lines
          (progn (forward-line -1)
                 (when (< (point) start)
                   (setq start (point))
                   (when compilation-filter-start
                     (setq compilation-filter-start start)))
                 (delete-region (point) seq-end))))))
  (funcall orig start (point)))

(advice-add 'comint-carriage-motion :around '$clear-bazel-progress-bar)

;; Tcsh is poorly supported in Emacs. The worst offender is the default
;; indentation, which is totally broken. This code ripped from
;; https://github.com/Tux/tcsh/blob/master/csh-mode.el fixes that.
(defun $tcsh-set-indent-functions ()
  (when (eq sh-shell 'tcsh)
    (load-file (expand-file-name "csh-indent-function.el" user-emacs-directory))
    (setq-local indent-line-function 'csh-indent-line)
    (setq-local indent-region-function 'csh-indent-region)))

(add-hook 'sh-set-shell-hook #'$tcsh-set-indent-functions)

;; (use-package markdown-mode)

(use-package major-modes
  :ensure (:host gitlab :repo "foconoco/major-modes" :main nil)
  :init
  ($leader-local-set-key
    :keymaps 'spfspec-mode-map
    "g" 'spfspec-goto-definition))

(use-package highlight-numbers
  :hook log-mode
  :config
  (setq highlight-numbers-generic-regexp
        ($rx (or (seq "0x" (1+ hex))
                 (seq "'" nums "'")
                 (seq (or bol spc ":")
                      (or (seq nums (any "./-") nums)
                          nums))))))

(add-to-list 'auto-mode-alist `(,(rx "itools" eos) . conf-mode))

(setenv "LIBRARY_PATH")

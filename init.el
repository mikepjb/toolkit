;;; init.el --- An elegant weapon -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keybindings are provided in under use-package or together in a dolist macro.
;; There is a 'code-mode-hook' to add minor modes like linum to your code
;; editing buffers.

;; The tools for working with Go require godef and gocode to be installed.
;; Resolve Bad Request: (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Additionally, project specific configuration can be placed in a .dir-locals.el.

;;; Use:
;;
;; Opening a file with super-user rights:
;; /su::/etc/hostname or /sudo::/etc/hostname
;;
;; Cleaning buffers:
;; C-x C-b, k to mark and x to execute (remove buffers)
;; Vertical insertion: Mark region -> C-x r t <string> <ret>
;;
;; goto-line -> M-g M-g
;; align on whitespace (generic) -> C-u M-x align-regexp RET SPC RET RET RET y
;; align columns (defined as 2 or more spaces) \(\s-\{2,\} \)
;; evaluate org-babel code block -> C-c C-v C-e

;;; Code:

;; To start, we adjust the garbage collection param

(setq gc-cons-threshold 32000000     ;; 32 MB
      garbage-collection-messages t) ;; indicator of thrashing

(setq-default
 vc-follow-symlinks t
 column-number-mode t
 ido-enable-flex-matching t
 custom-theme-load-path (list "~/.emacs.d/lib/")
 backup-directory-alist `(("." . "~/.emacs.d/saves"))
 custom-file (make-temp-file "")
 use-package-always-ensure t
 load-prefer-newer t
 fill-column 80
 compilation-ask-about-save nil
 use-package-verbose)

 ;; make is not the only way; allows us to define compile commands per project.
(make-variable-buffer-local 'compile-command)

(defalias 'yes-or-no-p 'y-or-n-p)

;; control minimum size of a new split.
(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

(prefer-coding-system 'utf-8)

(require 'cl-lib)

(menu-bar-mode -1)

(electric-pair-mode t)
(show-paren-mode t)
(savehist-mode t) ;; save minibuffer commands between sessions
(global-auto-revert-mode t) ;; always reread from disk

(ido-mode t)

(defvar welcome-messages
  '("I used to live by a code."
    "What is broken can be reforged."
    "Shoot for the moon. Even if you miss it you will land among the stars."))

(defun display-startup-echo-area-message ()
  "Select a random string to present at startup."
  (message (seq-random-elt welcome-messages)))

(defun gui-setup ()
  "Disable otiose GUI settings, 'fringe-mode' ruins performance."
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1)
  (ignore-errors
    (set-frame-font "xos4 Terminus-12")))

(if window-system (gui-setup))

(defun sync ()
  "Update my toolkit, notes repos and run citadel (to update system packages)."
  (interactive)
  (async-shell-command "mb"))

(defun package-setup ()
  "Setup Emacs to use the melpa repository and use-package."
  (require 'package)
  (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun remove-quotes (s)
  "Remove single and double quotes from the edges of a string, S."
  (string-trim (string-trim s "\"" "\"") "'" "'"))

(defun read-env ()
  "Read environment variables defined in the current sh buffer into Emacs."
  (interactive)
  (if (equal major-mode 'sh-mode)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((current-line (substring
			       (thing-at-point 'line t) 0 -1)))
	    (if (string-match-p "=" current-line)
		(let ((name (car (split-string current-line "=")))
		      (value (remove-quotes (car (last (split-string current-line "="))))))
		  (message (concat "Setting " name " to " value))
		  (setenv name value))))
	  (forward-line 1)))
    (message "read-env only works in sh-mode.")))

(defun compile-in-project-root ()
  (interactive)
  (let ((default-directory (git-root)))
    (call-interactively #'compile)))

(package-setup)

(use-package flycheck :config (global-flycheck-mode))
(use-package ripgrep)
;; (use-package auto-complete)
;; (use-package go-autocomplete)

(defun mikepjb:go-mode-hook ()
  "Personal configuration for go-mode."
  ;; (auto-complete-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-T") 'go-test-current-file)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(use-package go-mode
  :init (progn
	  (setq gofmt-command "goimports")
	  ;; (with-eval-after-load 'go-mode ;; gocode issue 325
	  ;;   (require 'go-autocomplete))
	  (add-hook 'go-mode-hook 'mikepjb:go-mode-hook)))

(use-package go-rename)
(use-package go-guru)
(use-package flymake :init (setq flymake-run-in-place nil))
(use-package flymake-go)
(use-package gotest :ensure t)
(use-package yaml-mode)
(use-package flycheck-yamllint)
(use-package flymake-yaml)
;; (use-package rainbow-mode)
(use-package protobuf-mode
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
	    (lambda () (c-add-style "my-style" my-protobuf-style t))))

(use-package diminish :ensure t)
(diminish 'eldoc-mode 'ivy-mode)

(use-package cider
  :init (progn
          (add-hook 'cider-repl-mode-hook 'paredit-mode)
          (setq
           cider-jdk-src-paths
           '("/usr/lib/jvm/java-10-openjdk/src.zip"
             "~/src/clojure-1.10.0-sources/"))
          ;; (set-variable 'cider-default-cljs-repl 'figwheel-main)
	  ;; (set-variable 'cider-session-name-template "%j:%r:%p")
	  ;; (set-variable 'nrepl-repl-buffer-name-template "%j:%r:%p")
	  ;; (set-variable 'cider-clojure-cli-global-options "-A:build-dev")
          ;; (set-variable 'cider-figwheel-main-default-options "dev")
          ;; (set-variable
          ;;  'cider-lein-parameters
          ;;  (concat "update-in :jvm-opts conj \"\\\"-Xmx5g\\\"\""
          ;;          " -- repl :headless :host localhost"))
	  ))

(use-package clojure-mode
  :mode
  (("\\.edn\\'" . clojure-mode)))

(use-package flycheck-clojure
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config (use-package flycheck :config (flycheck-clojure-setup)))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (defadvice he-substitute-string (after he-paredit-fix)
    "remove extra paren when expanding line in paredit"
    (if (and paredit-mode (equal (substring str -1) (or ")" "]" "}")))
	(progn (backward-delete-char 1) (forward-char)))))

;; Recentf comes with Emacs but it should always be enabled.

(use-package recentf
  :init (recentf-mode t)
  :config
  (add-to-list 'recentf-exclude "\\.emacs.d")
(add-to-list 'recentf-exclude ".+tmp......\\.org"))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (setq ivy-height 30
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-fuzzy)))
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-w"     . ivy-backward-kill-word)
	 )
  :diminish)

;; ivy-rich makes Ivy look a little bit more like Helm.

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

(use-package gnu-elpa-keyring-update)

;; Counsel applies Ivy-like behavior to other builtin features of
;; emacs, e.g. search.

(use-package counsel
  :ensure t
  :after ivy
  :init
  (counsel-mode 1)
  :bind (("C-;" . counsel-M-x)
         ("C-c U" . counsel-unicode-char)
         ("C-c i" . counsel-imenu)
         ("C-x f" . counsel-find-file)
         ("C-c y" . counsel-yank-pop)
	 ("C-c r" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history)
	 ("C-w" . ivy-backward-kill-word))
  :diminish)

(use-package deadgrep
  :bind (("C-c h" . deadgrep)))

(add-hook 'code-mode-hook
	  (lambda ()
	    (linum-mode t)
	    (hl-line-mode t)))

(use-package projectile
  :bind (("C-c p" . projectile-find-file))
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  :diminish)

;; Counsel and projectile should work together.

(use-package counsel-projectile
  :bind (("C-c f" . counsel-projectile))
  :init
  (counsel-projectile-mode))

;; Sort commands by recency in ivy windows.

(use-package smex)

;; Company is the best Emacs completion system.

;; (use-package company
;;   :bind (("C-." . company-complete))
;;   :diminish company-mode
;;   :custom
;;   (company-dabbrev-downcase nil "Don't downcase returned candidates.")
;;   (company-show-numbers t "Numbers are helpful.")
;;   (company-tooltip-limit 20 "The more the merrier.")
;;   (company-abort-manual-when-too-short t "Be less enthusiastic about completion.")
;;   :config
;;   (global-company-mode)

;;   ;; use numbers 0-9 to select company completion candidates
;;   (let ((map company-active-map))
;;     (mapc (lambda (x) (define-key map (format "%d" x)
;;                         `(lambda () (interactive) (company-complete-number ,x))))
;; 	  (number-sequence 0 9))))

;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.

(use-package magit
  :bind (("C-c g" . magit-status))
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :custom
  (magit-remote-set-if-missing t)
  (magit-diff-refine-hunk t)
  :config
  (magit-auto-revert-mode t)

  ;; Magit, and Emacs in general, has a nasty habit of prompting to save buffers
  ;; that are identical to those on disk. This is an attempt at remedying that,
  ;; one that I should probably attach to other functions like save-buffers-kill-emacs.
  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  (advice-add 'magit-commit  :before #'maybe-unset-buffer-modified)
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes))


;; Haskell and Elisp are made a lot easier when delimiters are nicely color-coded.

(use-package rainbow-delimiters
  :disabled
  :hook (prog-mode . rainbow-delimiters-mode))

;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :bind (("C-j" . markdown-enter-key))
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

;; ;; Avy is better than ace-jump.
;; (use-package avy
;;   :defer ivy
;;   :bind (("C-c l l" . avy-goto-line)
;;          ("C-c l c" . avy-goto-char-timer)
;;          ("C-c l w" . avy-goto-word-1)
;; ("C-'" . ivy-avy)))

(use-package typescript-mode
  :hook ((typescript-mode . tide-setup)))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package web-mode
  :bind (("C-c ]" . emmet-next-edit-point)
         ("C-c [" . emmet-prev-edit-point)
         ("C-c o b" . browse-url-of-file))
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.jsx$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(use-package polymode
  :init (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))
(use-package poly-markdown)

(dolist
    (mode-hook
     '(ruby-mode-hook
       go-mode-hook
       protobuf-mode-hook
       emacs-lisp-mode-hook
       typescript-mode-hook
       clojure-mode-hook
       clojurescript-mode-hook
       sh-mode-hook
       python-mode-hook
       markdown-mode-hook
       sql-mode-hook
       css-mode-hook
       yaml-mode-hook
       conf-mode-hook))
  (add-hook mode-hook (lambda () (run-hooks 'code-mode-hook))))

(setq debug-on-error t)
(setq inhibit-splash-screen t)

(defun kill-backward-or-region ()
  "Kill region when mark is set, other kill previous word."
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun git-root ()
  "Return the root directory in git or current directory if not."
  (let ((response (shell-command-to-string
                   "echo -ne $(git rev-parse --show-toplevel)")))
    (if (string-match-p (regexp-quote "fatal") response)
	default-directory
      response)))

(defun async-from-root ()
  "Run 'async-sheel-command' from your project root."
  (interactive)
  (let ((default-directory (git-root)))
    (call-interactively 'async-shell-command)))

(defun find-notes ()
  "Search for notes."
  (interactive)
  (let ((default-directory "~/notes/"))
    (ido-find-file)))

(defun comment-line-or-region ()
  "Comments the current line or region if active."
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun join-below ()
  "Join line below or all lines for a given region."
  (interactive)
  (if mark-active
      (replace-match "\n" "" nil (region-beginning) (region-end))
        (progn (forward-line 1) (join-line))))

(defun maybe-unset-buffer-modified (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (current-buffer-matches-file-p))
	(set-buffer-modified-p nil)))))

(defun iceberg-postgres ()
  "Start docker service if not running; start local postgres in docker-compose."
  (interactive)
  (let ((default-directory (git-root)))
    (async-shell-command "cd ./scripts/local-postgres && docker-compose up -d")))

;; There is an extant bug where magit-refresh prompts to save files that haven't
;; been modified. We work around this with some defadvice over maybe-unset-buffer-modified. SO:
;; https://emacs.stackexchange.com/questions/24011/make-emacs-diff-files-before-asking-to-save

(autoload 'diff-no-select "diff")

(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (when (and buffer-file-name (buffer-modified-p))
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

(defvar sql-ms-program "sqlcmd")
(defcustom sql-ms-options '("-w" "30000" "-y" "79" "-s" "|" "-k")
  "List of additional options for `sql-ms-program'."
  :type '(repeat string)
  :version "22.1"
  :group 'SQL)
(use-package sql
  :config
  (sql-set-product-feature 'ms :prompt-regexp "^[0-9]*>") ; existing line
  (sql-set-product-feature 'ms :prompt-cont-regexp "^[0-9]*>")) ; new line

(defvar sql-postgres-login-params
  '((user :default "postgres")
    (database :default "postgres")
    (server :default "localhost")
    (password :default "psql")
    (port :default 5432)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
	    (set-default 'truncate-lines t)))

(add-hook 'sql-mode-hook
	  (lambda ()
	    (setq-local tab-width 2)))

;; (defalias 'sql-get-login 'ignore) ;; do not confirm connection details

(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position))
  (end-of-line)
  (newline)
  (beginning-of-line)
  (yank))

(defun clojure-repl ()
  "Start a leiningen REPL."
  (interactive)
  (run-lisp "lein repl"))

(dolist
    (binding
     '(("M-o" . other-window)
       ("M-O" . (lambda () (interactive) (other-window -1)))
       ("M-T" . cider-test-run-test)
       ("M-U" . sync)
       ("M-z" . zap-up-to-char)
       ("C-6" . mode-line-other-buffer)
       ("C-c g" . magit-status)
       ("C-c l" . magit-log-current)
       ("C-c P" . magit-pull-from-upstream)
       ("C-c b" . compile-in-project-root)
       ("C-x p" . list-processes)
       ("C-j" . newline)
       ("C-w" . kill-backward-or-region)
       ("M-D" . duplicate-line)
       ("M-G" . projectile-ripgrep)
       ("C-t" . projectile-find-file)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("M-j" . join-below)
       ("C-h" . delete-backward-char)
       ("M-/" . hippie-expand)
       ("C-c /" . comment-line-or-region)
       ("M-&" . async-from-root)
       ("C-c i" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
       ("C-c m" . (lambda () (interactive) (find-file "~/toolkit/system.edn")))
       ("C-c n" . find-notes)
       ("M-RET" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(global-set-key (kbd "M-H") help-map)

(add-hook 'paredit-setup-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-s .") 'isearch-forward-symbol-at-point)
            (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)))

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'backward-kill-word)
            (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(load-theme 'lumo t)

;; The Doom Emacs themes look really good.

;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-vibrant t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config)
;;   (custom-theme-set-faces
;;    'doom-vibrant
;;    '(font-lock-doc-face ((t (:foreground "#D8D2C1"))))))

;; (use-package winum
;;   :config (winum-mode))

;; (use-package doom-modeline
;;   :config
;;   (setq doom-modeline-height 22)
;;   (doom-modeline-def-modeline
;;     main
;;    (workspace-number window-number bar evil-state matches " " buffer-info buffer-position  " " selection-info)
;;    (global major-mode process vcs flycheck))
;;   (doom-modeline-init))


(provide 'init)
;;; init.el ends here

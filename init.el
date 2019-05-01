;; align args - clojure-align for clojure.. align-regexp general solution - how to use?
;; allow C-w company-mode delete backward word...
;; prevent C-w from unbalancing parens in paredit-mode...
;; search under cursor...
;; replace all origin-0 etc with black (descriptive variables)
;; NO temp #. files please!

;; Use Cases:
;; 1. Postgres - sql-postgres
;; 2. Mongo?

;; Useful Commands:
;; C-u C-x = (describes font-lock under cursor)

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(if (not (package-installed-p 'use-package))
    (progn (package-refresh-contents)
           (package-install 'use-package)))

(require 'use-package)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq debug-on-error t)
(setq inhibit-splash-screen t)


(electric-pair-mode 1)
(show-paren-mode 1)
(ido-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(defvar welcome-messages
  '("I used to live by a code."
    "What is broken can be reforged."
    "Shoot for the moon. Even if you miss it you will land among the stars."))

(defun display-startup-echo-area-message ()
  (message (seq-random-elt welcome-messages)))

(defvar frame-config
  '((width . 180)
    (height . 70)
    (vertical-scroll-bars . nil)))

(setq
 initial-frame-alist frame-config
 default-frame-alist frame-config)

(set-frame-font "Inconsolata-14")

(setq-default
 ido-enable-flex-matching t
 make-backup-files nil
 auto-save-default nil
 column-number-mode t
 indent-tabs-mode nil
 ring-bell-function 'ignore
 vc-follow-symlinks t
 tab-width 2
 mac-command-modifier 'meta
 ido-auto-merge-work-directories-length -1
 case-fold-search t
 dired-listing-switches "-alh"
 custom-theme-load-path (list "~/toolkit")
 custom-file (make-temp-file "")
 ns-use-native-fullscreen nil
 auto-fill-function nil ;; do not wrap lines
 fill-column 80
 css-indent-offset 2
 js-indent-level 2
 pop-up-windows nil ;; do not open new buffers by default
 display-buffer-function 'buffer-policy
 package-enable-at-startup nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'load-path "~/.emacs.d/lib")

(defun toggle-line-wrap ()
  (interactive)
  (setq
   auto-fill-function
   (if (equal auto-fill-function 'do-auto-fill)
       nil 'do-auto-fill)))

(defun kill-backward-or-region ()
  "kill region when mark is set, other kill previous word"
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun comment-line-or-region ()
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun git-root ()
  (let ((response (shell-command-to-string
                   "echo -ne $(git rev-parse --show-toplevel || echo \".\")")))
    (if (string-match-p (regexp-quote "fatal") response) "." response)))

(defun async-from-root ()
  (interactive)
  (let ((default-directory (git-root)))
    (call-interactively 'async-shell-command)))

(defun build-tags ()
  (interactive)
  (shell-command (concat "cd " (git-root) " && ctags -f TAGS -e -R .")))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun join-below ()
  "join line below or all lines for a given region"
  (interactive)
  (if mark-active
      (replace-string "\n" "" nil (region-beginning) (region-end))
        (progn (next-line 1) (join-line))))

(defun open-repl ()
  (interactive)
  (if (= (count-windows) 1)
      (progn (split-window-below)
             (other-window 1)))
  (let ((default-directory (git-root)))
    (pcase major-mode
      ('clojure-mode (inferior-lisp "boot dev"))
      ('ruby-mode (comint-run "irb"))
      ('python-mode (comint-run "python"))
      (_ (message "major mode has no defined repl")))))

(defun backtrace-split ()
  "replace \n with newline chars for long backtrace buffers"
  (interactive)
  (read-only-mode 0)
  (replace-string "\\n" "\n" nil (point-min) (point-max) nil))

(defun duplicate-line ()
  "copy and paste the current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun buffer-policy (buf not-this-window)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or not-this-window
               (not (eq (window-buffer (selected-window)) buf)))
           (> (frame-height) 70))
      (split-window-vertically))
  ;; Note: Some modules set `pop-up-windows' to t before calling `display-buffer'
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    (display-buffer buf not-this-window))
  ;; (balance-windows) ;; does not seem to trigger here.
  )

(dolist
    (binding
     '(("M-o" . other-window)
       ("M-O" . (lambda () (interactive) (other-window -1)))
       ("M-g" . mark-paragraph)
       ("M-D" . duplicate-line)
       ("C-c g" . magit)
       ("C-c l" . magit-log-current)
       ("C-x p" . (lambda () (interactive) (ido-find-file-in-dir "~/src")))
       ("C-j" . newline)
       ("C-w" . kill-backward-or-region)
       ("C-;" . hippie-expand)
       ("M-G" . projectile-ripgrep)
       ("C-t" . projectile-find-file)
       ("C-h" . delete-backward-char)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("C-T" . cider-test-run-ns-tests) ;; intellij compatability
       ("C-L" . cider-load-buffer)
       ("C-c t" . run-tests)
       ("C-z" . open-shell)
       ("M-j" . join-below)
       ("M-/" . comment-line-or-region)
       ("M-z" . zap-up-to-char)
       ("M-&" . async-from-root)
       ("C-c M-j" . open-repl)
       ("C-c p" . list-processes)
       ("C-c i" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
       ("C-c d" . (lambda () (interactive) (find-file "~/notes/debug.org")))
       ("M-RET" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(global-set-key (kbd "M-H") help-map)

;; M-s -> . searches under cursor, it would be good to bind this.

;; paredit overwrites M-s with paredit-splice-sexp, reverse this
(add-hook 'paredit-setup-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-s .") 'isearch-forward-symbol-at-point)
            (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)))

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'backward-kill-word)
            (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(add-hook 'lisp-interaction-mode-hook
          (lambda () (define-key lisp-interaction-mode-map (kbd "C-j") 'newline)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-t") 'projectile-find-file)
            ))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs
                        '(
                          (?\" . ?\")
                          (?\{ . ?\})
                          (?\( . ?\))
                          (?\[ . ?\])))))

(use-package clojure-mode :ensure t)

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) (or ")" "]" "}")))
      (progn (backward-delete-char 1) (forward-char)))))

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :init
;;   (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;;   (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
;;   )

(use-package cider
  :ensure t
  :init (progn
          (add-hook 'cider-repl-mode-hook 'paredit-mode)
          (setq
           cider-jdk-src-paths
           '("/usr/lib/jvm/java-8-openjdk/src.zip"
             "~/src/clojure-1.10.0-sources/")
           cider-repl-display-help-banner nil)
          (set-variable 'cider-default-cljs-repl 'figwheel-main)
          (set-variable 'cider-figwheel-main-default-options ":dev")
          (set-variable
           'cider-lein-parameters
           (concat "update-in :jvm-opts conj \"\\\"-Xmx10g\\\"\""
                   " -- repl :headless :host localhost"))))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package ripgrep
  :ensure t
  :init (set-variable 'ripgrep-arguments '("-M" "120")))

(use-package magit :ensure t)
(use-package projectile :ensure t)
(use-package js2-mode :ensure t)
(use-package haskell-mode
  :ensure t
  :init (progn
          (add-hook 'haskell-mode 'turn-on-haskell-indentation)))
(use-package inf-ruby :ensure t)

(load-theme 'bare t)

(if (eq system-type 'darwin)
    (let ((path-from-shell
	         (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

;;; init.el --- An elegant weapon -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keybindings are provided in under use-package or together in a dolist macro.
;; There is a 'code-mode-hook' to add minor modes like linum to your code
;; editing buffers.

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
 use-package-verbose)

(prefer-coding-system 'utf-8)

(require 'cl)

(menu-bar-mode -1)

(electric-pair-mode t)
(show-paren-mode t)
(savehist-mode t) ;; save minibuffer commands between sessions

(ido-mode t)

(defun gui-setup ()
  "Disable otiose GUI settings, 'fringe-mode' ruins performance."
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1)
  (ignore-errors
    (set-frame-font "Inconsolata-14")))

(if window-system (gui-setup))

(defun package-setup ()
  (package-initialize)
  (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(package-setup)

(use-package flycheck)
(use-package ripgrep)
(use-package go-mode
  :init (progn
	  (setq gofmt-command "goimports")
	  (add-hook 'before-save-hook 'gofmt-before-save)))
(use-package go-rename)
(use-package go-guru)
(use-package flymake)
(use-package flymake-go)
(use-package magit)
(use-package projectile)
(use-package yaml-mode)
(use-package flycheck-yamllint)
(use-package flymake-yaml)
(use-package protobuf-mode)
;; (use-package diminish)
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

(add-hook 'code-mode-hook
	  (lambda ()
	    (linum-mode t)
	    (hl-line-mode t)))

(dolist
    (mode-hook
     '(ruby-mode-hook
       go-mode-hook
       proto-mode-hook
       emacs-lisp-mode-hook
       yaml-mode-hook))
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
  (let ((response (shell-command-to-string
                   "echo -ne $(git rev-parse --show-toplevel || echo \".\")")))
    (if (string-match-p (regexp-quote "fatal") response) "." response)))

(defun async-from-root ()
  (interactive)
  (let ((default-directory (git-root)))
    (call-interactively 'async-shell-command)))

(defun find-notes ()
  (interactive)
  (let ((default-directory "~/notes/"))
    (ido-find-file)))

(defun comment-line-or-region ()
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(dolist
    (binding
     '(("M-o" . other-window)
       ("M-O" . (lambda () (interactive) (other-window -1)))
       ("C-c g" . magit)
       ("C-c l" . magit-log-current)
       ("C-c P" . magit-pull-from-upstream)
       ("C-j" . newline)
       ("C-w" . kill-backward-or-region)
       ("C-;" . hippie-expand)
       ("M-G" . projectile-ripgrep)
       ("C-t" . projectile-find-file)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("C-h" . delete-backward-char)
       ("M-/" . comment-line-or-region)
       ("M-&" . async-from-root)
       ("C-c i" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
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

(provide 'init)
;;; init.el ends here

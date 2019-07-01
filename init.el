(require 'cl)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(electric-pair-mode t)
(show-paren-mode t)
(savehist-mode t) ;; save minibuffer commands between sessions

(ido-mode t)

(defun gui-setup ()
  (set-frame-font "Inconsolata-14"))

(if window-system (gui-setup))

(defun package-setup ()
  (package-initialize)
  (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
  (if (not (package-installed-p 'use-package))
      (progn (package-refresh-contents)
	     (package-install 'use-package))))

(package-setup)

(use-package ripgrep :ensure t)
(use-package go-mode
  :ensure t
  :init (add-hook 'before-save-hook 'gofmt-before-save))
(use-package go-rename :ensure t)
(use-package go-guru :ensure t)
(use-package flymake :ensure t)
(use-package flymake-go :ensure t)
(use-package magit :ensure t)
(use-package projectile :ensure t)
(use-package yaml-mode :ensure t)
(use-package flycheck-yamllint :ensure t)
(use-package flymake-yaml :ensure t)
(use-package protobuf-mode :ensure t)

(add-hook 'code-mode-hook
	  (lambda ()
	    (linum-mode t)
	    (hl-line-mode t)))

(dolist
    (mode-hook
     '(ruby-mode-hook
       go-mode-hook
       emacs-lisp-mode-hook))
  (add-hook mode-hook (lambda () (run-hooks 'code-mode-hook))))

(setq-default
 vc-follow-symlinks t
 column-number-mode t
 ido-enable-flex-matching t
 custom-theme-load-path (list "~/.emacs.d/lib/")
 backup-directory-alist `(("." . "~/.emacs.d/saves"))
 custom-file (make-temp-file ""))

(setq debug-on-error t)
(setq inhibit-splash-screen t)

(defun kill-backward-or-region ()
  "kill region when mark is set, other kill previous word"
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
       ("C-h" . delete-backward-char)
       ("M-/" . comment-line-or-region)
       ("M-&" . async-from-root)
       ("C-c i" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
       ("C-c n" . find-notes)
       ("M-RET" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(global-set-key (kbd "M-H") help-map)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'backward-kill-word)
            (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(load-theme 'lumo t)

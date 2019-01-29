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
 backup-directory-alist '(("" . "~/.emacs.d/backup"))
 column-number-mode t
 indent-tabs-mode nil
 ring-bell-function 'ignore
 vc-follow-symlinks t
 tab-width 2
 mac-command-modifier 'meta
 case-fold-search t
 custom-theme-load-path (list "~/.emacs.d/lib")
 ns-use-native-fullscreen nil
 package-enable-at-startup nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'load-path "~/.emacs.d/lib")

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

(defun ido-search ()
  (interactive)
  (save-excursion
    (find-file
     (concat
      (git-root)
      "/"
      (ido-completing-read
       "Open: "
       (split-string
        (shell-command-to-string
         (concat "cd " (git-root) " && find * -type f")) "\n")
       nil
       t)))))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(defun join-below () (interactive) (next-line 1) (join-line))

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

(dolist
    (binding
     '(("M-o" . other-window)
       ("M-g" . mark-paragraph)
       ("C-c g d" . vc-diff)
       ("C-c g b" . vc-annotate) ;; git blame
       ("C-c g p" . git-ff)
       ("C-c g l" . vc-print-root-log)
       ("C-j" . newline)
       ("C-w" . kill-backward-or-region)
       ("C-;" . hippie-expand)
       ("C-t" . ido-search)
       ;; ("M-k" . backward-left-bracket)
       ;; ("M-l" . forward-right-bracket)
       ("M-k" . paredit-forward-barf-sexp)
       ("M-l" . paredit-forward-slurp-sexp)
       ("C-c t" . run-tests)
       ("C-z" . open-shell)
       ("M-j" . join-below)
       ("M-/" . comment-line-or-region)
       ("M-z" . zap-up-to-char)
       ("C-c M-j" . open-repl)
       ("C-c p" . list-processes)
       ("C-c i" . (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
       ("C-c d" . (lambda () (interactive) (find-file "~/notes/debug.org")))
       ("M-RET" . toggle-frame-fullscreen)))
  (global-set-key (kbd (car binding)) (cdr binding)))

;; M-s -> . searches under cursor, it would be good to bind this.

;; paredit overwrites M-s with paredit-splice-sexp, reverse this
(add-hook 'paredit-setup-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-s ." isearch-forward-symbol-at-point))))

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'backward-kill-word)
            (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(add-hook 'lisp-interaction-mode-hook
          (lambda () (define-key lisp-interaction-mode-map (kbd "C-j") 'newline)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-t") 'ido-search)))

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
 :config (lambda ()
           (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
           (add-hook 'lisp-mode-hook 'enable-paredit-mode)
           (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
           (add-hook 'clojure-mode-hook 'enable-paredit-mode)))
(use-package rainbow-delimiters
  :ensure t
  :config (lambda ()
            (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))
(use-package cider
  :ensure t
  :config (lambda ()
            (add-hook 'cider-repl-mode-hook 'paredit-mode)))

(if (eq system-type 'darwin)
    (let ((path-from-shell
	         (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (cider paredit clojure-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

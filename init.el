(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq debug-on-error t)
(setq inhibit-splash-screen t)

(global-font-lock-mode -1)
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

(setq grep-command "grep")

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

(defun build-tags ()
  (interactive)
  (shell-command
   (concat "ctags -e -f " (git-root) "/.git/etags -R " (git-root))))

(defun run-tests ()
  (interactive)
  (if (eq major-mode 'go-mode)
      (shell-command (concat "cd " (git-root) "&& go test ./..."))))

(defvar left-brackets '("(" "[" "{" "<"))
(defun backward-left-bracket ()
  (interactive) (re-search-backward (regexp-opt left-brackets) nil t))
(defvar right-brackets '(")" "]" "}" ">"))
(defun forward-right-bracket ()
  (interactive) (re-search-forward (regexp-opt right-brackets) nil t))
;; (defun select-sexp () (interactive))
(defun group-select ()
  "select current grouping of text, a sexp, a string or extend to the parent group"
  )

(defun join-below () (interactive) (next-line 1) (join-line))

;; last option for compiile you can specify a minor mode (to make error lines clickable)
(defun rspec-compile ()
  (interactive)
  (let ((compile-target (buffer-file-name)))
    (use-latest-ruby)
    (let ((default-directory (git-root)))
      (compile (concat "bundle exec rspec --no-color " compile-target) nil))))


(defun read-dotenv ()
  (interactive)
  (let ((buffer-lines (split-string (buffer-string) "\n" t)))
    (mapcar
     (lambda (y)
       (let ((key (car y))
             (value (replace-regexp-in-string "'" "" (car (cdr y)))))
         (setenv key value)))
     (mapcar (lambda (x) (split-string x "=" t)) buffer-lines))))


(defun kanji ()
  (interactive)
  (if (eq current-input-method nil)
      (set-input-method 'japanese)
    (deactivate-input-method)))

(defun open-shell ()
  (interactive)
  (if (= (count-windows) 1)
      (progn (split-window-below)
             (other-window 1)))
  (let ((default-directory (git-root)))
    (eshell)))

(defun git-ff ()
  (interactive) (async-shell-command (concat "cd " (git-root) " && git ff")))

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
       ("M-k" . backward-left-bracket)
       ("M-l" . forward-right-bracket)
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

(add-hook 'ido-setup-hook
	        (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'backward-kill-word)
            (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(add-hook 'lisp-interaction-mode-hook
          (lambda () (define-key lisp-interaction-mode-map (kbd "C-j") 'newline)))

;; M-DEL backward-kill-word (global-map) on ido-search
;; M-DEL on find-file - ido-delete-backward-word-updir (found in ido-completion-map)

;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
;;             (define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)))

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

(if (string-match "\\`hades-desktop" (system-name))
    (set-frame-font "Inconsolata 19" nil t)
  (set-frame-font "Inconsolata 16" nil t))

(load-theme 'bare t)

(require 'go-mode)
(require 'clojure-mode)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Interesting - it seems you can order a mode to be evaluated when a function is called
;; e.g useful for clojurescript-mode, loading clojure-mode which contains the definition.
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code."
;;   t)

(add-hook 'after-save-hook
          (lambda () (interactive)
            (if (eq major-mode 'go-mode)
                (progn (shell-command (concat "goimports -w " (buffer-file-name)))
                       (revert-buffer nil t)))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "cat")
            (setenv "EDITOR" "emacsclient")
            (setenv "GOPATH" (expand-file-name "~"))))

(if (eq system-type 'darwin)
    (let ((path-from-shell
	   (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

(require 'elruby)

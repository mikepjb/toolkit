(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq debug-on-error t)

(global-font-lock-mode -1)
(electric-pair-mode 1)
(show-paren-mode 1)
(ido-mode 1)

(setq-default
 ido-enable-flex-matching t
 backup-directory-alist '(("" . "~/.emacs.d/backup"))
 column-number-mode t
 indent-tabs-mode nil
 ring-bell-function 'ignore
 vc-follow-symlinks t
 tab-width 2
 mac-command-modifier 'meta
 custom-theme-load-path (list "~/.emacs.d/lib")
 package-enable-at-startup nil)

(add-to-list 'load-path "~/.emacs.d/lib")

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


(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-g") 'mark-paragraph)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-w") 'kill-backward-or-region)
(global-set-key (kbd "C-;") 'hippie-expand)
(global-set-key (kbd "C-t") 'ido-search)
(global-set-key (kbd "M-k") 'backward-left-bracket)
(global-set-key (kbd "M-l") 'forward-right-bracket)
(global-set-key (kbd "C-c t") 'run-tests)
(global-set-key (kbd "C-z") 'eshell)

;; M-s -> . searches under cursor, it would be good to bind this.

(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(add-hook 'ido-setup-hook
	        (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(set-frame-font "Inconsolata 19" nil t)

(load-theme 'bare t)

(autoload 'go-mode "go-mode" "golang major mode" t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'after-save-hook
          (lambda () (interactive) (if (eq major-mode 'go-mode)
                                       (progn (shell-command (concat "goimports -w " (buffer-file-name)))
                                              (revert-buffer nil t)))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "cat")
            (setenv "EDITOR" "emacsclient")))

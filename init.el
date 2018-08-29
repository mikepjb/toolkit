;; i. aesthetics
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-default-font "Inconsolata 14" nil t)

;; ii. behaviour
(setq-default
 column-number-mode t
 vc-follow-symlinks t
 ido-enable-flex-matching t
 ring-bell-function 'ignore)

(ido-mode 1)
(show-paren-mode 1)
(electric-pair-mode t)
(global-font-lock-mode -1)

;; iii. functions
(defvar matching-brackets "()[]{}<>")

(defun select-sexp ()
  "select the current sexp that you are working in."
  (interactive)
  (let ()))

;; iv. controls
(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "M-o") 'other-window)

(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "C-z") 'eshell))

(add-to-list 'focus-out-hook
	     (lambda ()
	       (if (buffer-file-name) (save-buffer (current-buffer)))))

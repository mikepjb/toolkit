(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-default-font "Inconsolata 14" nil t)

(setq-default
 column-number-mode t
 vc-follow-symlinks t
 ido-enable-flex-matching t)

(ido-mode 1)
(show-paren-mode 1)
(electric-pair-mode t)
(global-font-lock-mode -1)

(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-updir)))

(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac* (setq mac-command-modifier 'meta))

(add-to-list 'focus-out-hook (lambda () (save-buffer (current-buffer))))

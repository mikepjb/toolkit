(require 'cl)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(electric-pair-mode t)
(show-paren-mode t)

(ido-mode t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(setq-default vc-follow-symlinks t)

;; (unless (require 'el-get nil t)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
;;     (end-of-buffer)
;; (eval-print-last-sexp)))

(global-set-key (kbd "M-o") 'other-window)

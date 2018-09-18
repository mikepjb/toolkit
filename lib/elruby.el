;; (require 'cl-lib)
(require 'seq)

;; (defvar elruby-current-ruby-binary-path nil)

;; (defun elruby-collect-rubies (rubies-dir)
;;   (cddr
;;    (and rubies-dir
;;         (and
;;          (file-directory-p rubies-dir)
;;          (directory-files rubies-dir)))))

;; (defun elruby-rubies ()
;;   (cl-mapcan
;;    'elruby-collect-rubies
;;    (list "/opt/rubies/"
;;          (concat (file-name-as-directory (getenv "HOME")) ".rubies"))))

;; (defun elruby-util-basename (path)
;;   (file-name-nondirectory (directory-file-name path)))

;; (defun elruby--available-names ()
;;   (reverse (mapcar 'elruby-util-basename (elruby-rubies))))

;; (defun elruby-change-path (new-binaries)
;;   (let ((current-binaries-for-path
;;          (mapconcat 'identity elruby-current-ruby-binary-path ":"))
;;         (new-binaries-for-path (mapconcat 'identity new-binaries ":")))
;;     (if (and elruby-current-ruby-binary-path
;;              (not (string= (cl-first elruby-current-ruby-binary-path) "/bin")))
;;         (progn
;;           (setenv "PATH" (replace-regexp-in-string
;;                           (regexp-quote current-binaries-for-path)
;;                           new-binaries-for-path
;;                           (getenv "PATH")))
;;           (dolist (binary elruby-current-ruby-binary-path)
;;             (setq exec-path (remove binary exec-path))))
;;       (setenv "PATH" (concat new-binaries-for-path ":" (getenv "PATH"))))
;;     (dolist (binary new-binaries)
;;       (add-to-list 'exec-path binary))
;;     (setq eshell-path-env (getenv "PATH"))
;;     (setq elruby-current-ruby-binary-path new-binaries)))

;; (defun elruby-find (name)
;;   (cl-first
;;    (delq nil
;;          (cl-mapcar
;;           (lambda (ruby)
;;             (and (string-match name ruby) ruby))
;;           (elruby-rubies)))))

;; (defun elruby-query-version (ruby-bin)
;;   "Shell out to Ruby to find out the current engine (ruby, jruby, etc), the
;; ruby version, and the gem path"
;;   (split-string
;;    (shell-command-to-string
;;     (concat ruby-bin "/ruby -rrubygems -e 'print [(defined?(RUBY_ENGINE) ? RUBY_ENGINE : %[ruby]), (RUBY_VERSION), (Gem.default_dir)].join(%[##])' 2>/dev/null")) "##"))

;; (defun elruby-set-gemhome (gemhome gempath)
;;   (if (and gemhome gempath)
;;       (progn
;;         (setenv "GEM_HOME" gemhome)
;;         (setenv "GEM_PATH" gempath))
;;     (setenv "GEM_HOME" "")
;;     (setenv "GEM_PATH" "")))

;; (defun elruby-activate (name)
;;   (let ((ruby-root (elruby-find name)))
;;     (message ruby-root)
;;     (when ruby-root
;;       (setq elruby-current-ruby-name (elruby-util-basename ruby-root))
;;       (elruby-change-path (list (concat ruby-root "/bin")))

;;       (let ((engine_version_gempath (elruby-query-version (concat ruby-root "/bin"))))
;;         (message (format "%s" engine_version_gempath))
;;         (let ((engine (cl-first engine_version_gempath))
;;               (version (cl-second engine_version_gempath))
;;               (gemroot (cl-third engine_version_gempath)))
;;           (let ((gemhome (concat (getenv "HOME") "/.gem/" engine "/" version)))
;;             (elruby-set-gemhome gemhome
;;                                 (concat gemhome ":" gemroot))
;;             (elruby-change-path
;;              (list (concat gemhome "/bin") (concat ruby-root "/bin")))))))))

;; ;;;###autoload
;; (defun elruby-use (ruby-version)
;;   "choose what ruby you want to activate"
;;   (interactive
;;    (let ((picked-ruby (completing-read "Ruby version: " (elruby--available-names))))
;;      (list picked-ruby)))
;;   (if (elruby-activate ruby-version)
;;       (message (concat "[elruby] using " ruby-version))
;;     (message (concat "[elruby] couldn't find " ruby-version))))

;;;###autoload
(defun use-latest-ruby ()
  (interactive)
  (setenv "GEM_HOME" (concat (getenv "HOME") "/.gem/ruby/2.5.0"))
  (setenv "GEM_PATH" (concat
                      (getenv "HOME") "/.gem/ruby/2.5.0:"
                      (getenv "HOME") "/.rubies/ruby-2.5.0/lib/ruby/gems/2.5.0"))
  (let ((rubyless-path (mapconcat 'identity
                                  (seq-filter
                                   (lambda (x) (not (string-match-p (regexp-quote "ruby") x)))
                                   (split-string (getenv "PATH") ":")) ":")))
    (let ((new-path (concat
                     (getenv "HOME") "/.gem/ruby/2.5.0/bin:"
                     (getenv "HOME") "/.rubies/ruby-2.5.0/lib/ruby/gems/2.5.0/bin:"
                     (getenv "HOME") "/.rubies/ruby-2.5.0/bin:"
                     rubyless-path)))
      (setenv "PATH" new-path)
      (setq eshell-path-env new-path))))

(provide 'elruby)

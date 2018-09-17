(require 'cl-lib)

(defvar chruby-current-ruby-binary-path nil)

(defun chruby-collect-rubies (rubies-dir)
  (cddr
   (and rubies-dir
        (and
         (file-directory-p rubies-dir)
         (directory-files rubies-dir)))))

(defun chruby-rubies ()
  (cl-mapcan
   'chruby-collect-rubies
   (list "/opt/rubies/"
         (concat (file-name-as-directory (getenv "HOME")) ".rubies"))))

(defun chruby-util-basename (path)
  (file-name-nondirectory (directory-file-name path)))

(defun chruby--available-names ()
  (reverse (mapcar 'chruby-util-basename (chruby-rubies))))

(defun chruby-change-path (new-binaries)
  (let ((current-binaries-for-path
         (mapconcat 'identity chruby-current-ruby-binary-path ":"))
        (new-binaries-for-path (mapconcat 'identity new-binaries ":")))
    (if (and chruby-current-ruby-binary-path
             (not (string= (cl-first chruby-current-ruby-binary-path) "/bin")))
        (progn
          (setenv "PATH" (replace-regexp-in-string
                          (regexp-quote current-binaries-for-path)
                          new-binaries-for-path
                          (getenv "PATH")))
          (dolist (binary chruby-current-ruby-binary-path)
            (setq exec-path (remove binary exec-path))))
      (setenv "PATH" (concat new-binaries-for-path ":" (getenv "PATH"))))
    (dolist (binary new-binaries)
      (add-to-list 'exec-path binary))
    (setq eshell-path-env (getenv "PATH"))
    (setq chruby-current-ruby-binary-path new-binaries)))

(defun chruby-find (name)
  (cl-first
   (delq nil
         (cl-mapcar
          (lambda (ruby)
            (and (string-match name ruby) ruby))
          (chruby-rubies)))))

(defun chruby-query-version (ruby-bin)
  "Shell out to Ruby to find out the current engine (ruby, jruby, etc), the
ruby version, and the gem path"
  (split-string
   (shell-command-to-string
    (concat ruby-bin "/ruby -rrubygems -e 'print [(defined?(RUBY_ENGINE) ? RUBY_ENGINE : %[ruby]), (RUBY_VERSION), (Gem.default_dir)].join(%[##])' 2>/dev/null")) "##"))

(defun chruby-set-gemhome (gemhome gempath)
  (if (and gemhome gempath)
      (progn
        (setenv "GEM_HOME" gemhome)
        (setenv "GEM_PATH" gempath))
    (setenv "GEM_HOME" "")
    (setenv "GEM_PATH" "")))

(defun chruby-activate (name)
  (let ((ruby-root (chruby-find name)))
    (message ruby-root)
    (when ruby-root
      (setq chruby-current-ruby-name (chruby-util-basename ruby-root))
      (chruby-change-path (list (concat ruby-root "/bin")))

      (let ((engine_version_gempath (chruby-query-version (concat ruby-root "/bin"))))
        (message (format "%s" engine_version_gempath))
        (let ((engine (cl-first engine_version_gempath))
              (version (cl-second engine_version_gempath))
              (gemroot (cl-third engine_version_gempath)))
          (let ((gemhome (concat (getenv "HOME") "/.gem/" engine "/" version)))
            (chruby-set-gemhome gemhome
                                (concat gemhome ":" gemroot))
            (chruby-change-path
             (list (concat gemhome "/bin") (concat ruby-root "/bin")))))))))

;;;###autoload
(defun chruby-use (ruby-version)
  "choose what ruby you want to activate"
  (interactive
   (let ((picked-ruby (completing-read "Ruby version: " (chruby--available-names))))
     (list picked-ruby)))
  (if (chruby-activate ruby-version)
      (message (concat "[chruby] using " ruby-version))
    (message (concat "[chruby] couldn't find " ruby-version))))

(provide 'chruby)

;; TODO reached clojure-indent-function - (:require is indented 4 not 2 currently.)

(require 'cl-lib)

(defvar clojure-mode-hook nil)

(defvar clojure-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for Clojure major mode")

;; (defvar clojure-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     st)
;;   "Syntax table for Clojure mode")

;; (put 'ns 'lisp-indent-function 'defun)

;; (defcustom clojure-align-binding-forms
;;   '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
;;     "doseq" "for" "with-open" "with-local-vars" "with-redefs")
;;   "List of strings matching forms that have binding forms"
;;   :safe #'listp
;;   :type '(repeat string))

;; TODO search and complete this function
;; (defun put-clojure-indent (sym indent)
;;   "Instruct `clojure-indent-function' to indent the body of SYM by INDENT"
;;   (put sym 'clojure-indent-function indent))

(put 'ns 'lisp-indent-function 'defun)
;; (put 'defn 'lisp-indent-function 'defun)
;; (put ':require 'lisp-indent-function 2)


(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "({" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] "([" table)
    (modify-syntax-entry ?\? "_ p" table)
    (modify-syntax-entry ?# "_ p" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for Clojure mode.")

(defun clojure-indent-line ()
  (interactive)
  (lisp-indent-line))

(defun clojure-mode-variables ()
  "set up initial buffer-local variables"
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local command-add 1)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'clojure-indent-line)
  ;; (setq-local indent-region-function #'clojure-indent-region)
  ;; (setq-local lisp-indent-function #'clojure-indent-function)
  )

(define-derived-mode clojure-mode lisp-mode "Clojure"
  "Major mode for editing Clojure Language files"
  (clojure-mode-variables)
  ;; (set (make-local-variable 'lisp-indent-function) #'clojure-indent-function)
  (set (make-local-variable 'font-lock-defaults) '(clojure-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'clojure-indent-line))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs
                        '(
                          (?\" . ?\")
                          (?\{ . ?\})
                          (?\( . ?\))
                          (?\[ . ?\])))))

(provide 'my-clojure-mode)

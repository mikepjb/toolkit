(defvar clojure-mode-hook nil)

(defvar clojure-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for Clojure major mode")

(defconst clojure-font-lock-keywords
  (list
   `(,(concat "\\b" (regexp-opt '("package" "import" "type" "func" "len") t) "\\b") . 'font-lock-builtin-face)
   `(,(concat "\\b" (regexp-opt '("ns" "defn" "defmacro" "if" "else") t) "\\b") . 'font-lock-keyword-face)
   `(,(concat "\\b" (regexp-opt '("loop" "let" "nil" "int") t) "\\b") . 'font-lock-type-face)
   )
  "Minimal highlighting expressions for Clojure mode")

;; (defvar clojure-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     st)
;;   "Syntax table for Clojure mode")

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

(define-derived-mode clojure-mode lisp-mode "Clojure"
  "Major mode for editing Clojure Language files"
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

(provide 'clojure-mode)

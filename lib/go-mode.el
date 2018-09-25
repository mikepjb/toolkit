(require 'cl-lib)

(defvar go-mode-hook nil)

(defvar go-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for Go major mode")

(defconst go-mode-keywords
  '("break" "default" "func" "interface" "select"
    "case" "defer" "go" "map" "struct" "chan" "else"
    "goto" "package" "type"))

(defconst go-font-lock-keywords
  (list
   `(,(regexp-opt '("package" "import" "type" "func" "len") t) . 'font-lock-builtin-face)
   `(,(concat "\\b" (regexp-opt go-mode-keywords t) "\\b") . 'font-lock-keyword-face)
   `(,(concat "\\b" (regexp-opt '("bool" "string" "nil") t) "\\b") . 'font-lock-type-face)
   )
  "Minimal highlighting expressions for Go mode")

(defvar go-dangling-cache)

(defconst go-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst go-label-regexp go-identifier-regexp)
(defconst go-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst go--max-dangling-operator-length 2
  "The maximum length of dangling operators.
This must be at least the length of the longest string matched by
‘go-dangling-operators-regexp.’, and must be updated whenever
that constant is changed.")

(defvar go-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Set comment syntax
    (modify-syntax-entry ?\/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for Go mode")

(defun go--reset-dangling-cache-before-change (&optional _beg _end)
  "Reset `go-dangling-cache'.

This is intended to be called from `before-change-functions'."
  (setq go-dangling-cache (make-hash-table :test 'eql)))

(defmacro go-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

(defmacro go-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro go-in-comment-p ()
  `(nth 4 (syntax-ppss)))

(defmacro go-paren-level ()
  `(car (syntax-ppss)))

(defmacro go-goto-beginning-of-string-or-comment ()
  `(goto-char (nth 8 (syntax-ppss))))

(defun go-goto-opening-parenthesis ()
  "move up one level of parens"
  (condition-case nil (backward-up-list) (scan-error nil)))

(defun go--buffer-narrowed-p ()
  "return non-nil if the current buffer is narrowed."
  (/= (buffer-size) (- (point-max) (point-min))))

(defun go--backward-irrelevant (&optional stop-at-string)
  "skip backwards over any characters that are irrelevant for indentation and related tasks."
  (let (pos (start-pos (point)))
    (skip-chars-backward "\n\s\t")
    (if (and (save-excursion (beginning-of-line) (go-in-string-p))
             (= (char-before) ?`)
             (not stop-at-string))
        (backward-char))
    (if (and (go-in-string-p)
             (not stop-at-string))
        (go-goto-beginning-of-string-or-comment))
    (if (looking-back "\\*/" (line-beginning-position))
        (backward-char))
    (if (go-in-comment-p)
        (go-goto-beginning-of-string-or-comment))
    (setq pos (point))
    (beginning-of-line)
    (if (or (looking-at (concat "^" go-label-regexp ":"))
            (looking-at "^[[:space:]]*\\(case .+\\|default\\):"))
        (end-of-line 0)
      (goto-char pos))
    (if (/= start-pos (point))
        (go--backward-irrelevant stop-at-string))
    (/= start-pos (point))))

(defun go-previous-line-has-dangling-op-p ()
  "return non-nil if the current line is a continuation line."
  (let* ((cur-line (line-number-at-pos))
         (val (gethash cur-line go-dangling-cache 'nope)))
    (if (or (go--buffer-narrowed-p) (equal val 'nope))
        (save-excursion
          (beginning-of-line)
          (go--backward-irrelevant t)
          (setq val (looking-back go-dangling-operators-regexp
                                  (- (point) go--max-dangling-operator-length)))
          (if (not (go--buffer-narrowed-p))
              (puthash cur-line val go-dangling-cache))))))

(defun go--at-function-definition ()
  "return non-nil if point is on opening curly brace of function def"
  (if (= (char-after) ?\{)
      (save-excursion
        (let ((old-point (point))
              start-nesting)
          (beginning-of-defun)
          (when (looking-at "func ")
            (setq start-nesting (go-paren-level))
            (skip-chars-foward "^{")
            (while (> (go-paren-level) start-nesting)
              (forward-char)
              (skip-chars-forward "^{") 0)
            (if (and (= (go-paren-level) start-nesting) (= old-point (point)))
                t))))))

(defun go--indentation-for-opening-parenthesis ()
  "return semantic indentation for current opening parens"
  (save-excursion
    (if (go--at-function-definition)
        (progn
          (beginning-of-defun)
          (current-indentation))
      (current-indentation))))

(defun go-indentation-at-point ()
  (save-excursion
    (let (start-nesting)
      (back-to-indentation)
      (setq start-nesting (go-paren-level))

      (cond
       ((go-in-string-p)
        (current-indentation))
       ((looking-at "[])}]")
        (go-goto-opening-parenthesis)
        (if (go-previous-line-has-dangling-op-p)
            (- (current-indentation) tab-width)
          (go--indentation-for-opening-parenthesis)))
       ((progn (go--backward-irrelevant t)
               (looking-back go-dangling-operators-regexp
                             (- (point) go--max-dangling-operator-length)))
        (if (go-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (current-indentation) tab-width)))
       ((zerop (go-paren-level)) 0)
       ((progn (go-goto-opening-parenthesis) (< (go-paren-level) start-nesting))
        (if (go-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (go--indentation-for-opening-parenthesis) tab-width)))
       (t
        (current-indentation))))))

(defun go-indent-line ()
  (interactive)
  (let (indent
        shift-amt
        (pos (- (point-max) (point)))
        (point (point))
        (beg (line-beginning-position)))
    (back-to-indentation)
    (if (go-in-string-or-comment-p)
        (goto-char point)
      (setq indent (go-indentation-at-point))
      (if (looking-at (concat go-label-regexp ":\\([[:space:]]*/.+\\)?$\\|case .+:\\|default:"))
          (cl-decf indent tab-width))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

;;;###autoload
(define-derived-mode go-mode prog-mode "Go"
  "Major mode for editing Go Language files"
  (set (make-local-variable 'font-lock-defaults) '(go-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'go-indent-line)

  (set (make-local-variable 'go-dangling-cache) (make-hash-table :test 'eql))
  (add-hook 'before-change-functions #'go--reset-dangling-cache-before-change t t)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *"))

(provide 'go-mode)

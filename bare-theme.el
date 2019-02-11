(deftheme bare "Code illuminated.")

(let ((class '((class color) (min-colors 89)))
      (origin-1 "#1d1e1a")
      (origin-10 "#eeeeee")
      (origin-9 "#cccccc")
      (magenta-5 "#ff3399")
      (mint-5 "#00ffcc")
      (mint-6 "#66ffcc")
      (aqua-3 "#00cccc")
      (aqua-4 "#00e6e6")
      (aqua-5 "#00ffff")
      (daffodil-5 "#ffff66")
      (lavender-5 "#e6e6ff")
      (bare-light-grey "#eeeeee")
      (bare-grey "#444444")
      (bare-black "#1b1d1e")
      (bare-white "#eeeeee")
      (bare-yellow "#cf8f2e")
      (bare-blue "#486ab4")
      (bare-green "#365d2e")
      (bare-magenta "#e13dfc")
      (bare-purple "#8b008b")
      (bare-region "#dcb9b9"))

  ;; view font-lock under cursor with C-u C-x =
  (custom-theme-set-faces
   'bare
   `(default ((,class (:foreground ,origin-10 :background ,origin-1))))
   `(cursor ((,class (:background ,aqua-5))))
   `(region ((,class (:background ,origin-9))))
   `(highlight ((,class (:background ,origin-10))))
   `(font-lock-builtin-face ((,class (:foreground ,aqua-5))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta-5))))
   `(font-lock-type-face ((,class (:foreground ,mint-6))))
   `(font-lock-function-name-face ((,class (:foreground ,daffodil-5))))
   `(font-lock-string-face ((,class (:foreground ,lavender-5))))
   `(font-lock-comment-face ((,class (:foreground ,magenta-5))))
   `(comint-highlight-prompt ((,class (:foreground ,origin-10))))
   `(diff-added ((,class (:foreground ,origin-10))))
   `(diff-removed ((,class (:foreground ,origin-10))))
   `(diff-hunk-header ((,class (:foreground ,origin-10))))
   `(diff-file-header ((,class (:foreground ,origin-10))))
   `(diff-header ((,class (:foreground ,origin-10))))
   `(ido-first-match ((,class (:foreground ,aqua-5))))
   `(ido-only-match ((,class (:foreground ,magenta-5))))
   `(ido-subdir ((,class (:foreground ,daffodil-5))))
   `(ido-indicator ((,class (:foreground ,origin-10))))
   `(match ((,class (:background ,origin-10))))
   `(compilation-info ((,class (:foreground ,origin-10))))
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,aqua-5))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,aqua-4))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,aqua-3))))
   (custom-theme-set-variables
    'bare
    `(ansi-color-names-vector
      [,bare-black
       ,bare-magenta ;; red
       ,bare-green
       ,bare-yellow ;; yellow
       ,bare-blue
       ,bare-magenta
       ,bare-blue ;; cyan
       ,bare-white]))
   `(minibuffer-prompt ((,class (:foreground ,origin-10))))
   ))

;; parens?
;; rainbow parens?

(provide-theme 'bare)

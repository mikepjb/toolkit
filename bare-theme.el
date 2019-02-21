(deftheme bare "Code illuminated.")

(let ((class '((class color) (min-colors 89)))
      (origin-1 "#1d1e1a")
      (origin-2 "#282924")
      (origin-3 "#35372f")
      (origin-4 "#42453b")
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
   `(default ((t (:foreground ,origin-10 :background ,origin-1))))
   `(cursor ((t (:background ,aqua-5))))
   `(region ((t (:background ,origin-9))))
   `(highlight ((t (:background ,origin-2))))
   `(font-lock-builtin-face ((t (:foreground ,aqua-5))))
   `(font-lock-keyword-face ((t (:foreground ,magenta-5))))
   `(font-lock-type-face ((t (:foreground ,mint-6))))
   `(font-lock-function-name-face ((t (:foreground ,daffodil-5))))
   `(font-lock-string-face ((t (:foreground ,lavender-5))))
   `(font-lock-comment-face ((t (:foreground ,magenta-5))))
   `(comint-highlight-prompt ((t (:foreground ,origin-2))))
   `(diff-added ((t (:foreground ,origin-10))))
   `(diff-removed ((t (:foreground ,origin-10))))
   `(diff-hunk-header ((t (:foreground ,origin-10))))
   `(diff-file-header ((t (:foreground ,origin-10))))
   `(diff-header ((t (:foreground ,origin-10))))
   `(ido-first-match ((t (:foreground ,aqua-5))))
   `(ido-only-match ((t (:foreground ,magenta-5))))
   `(ido-subdir ((t (:foreground ,daffodil-5))))
   `(ido-indicator ((t (:foreground ,origin-10))))
   `(match ((t (:background ,origin-4))))
   `(compilation-info ((t (:foreground ,origin-10))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,aqua-5))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,aqua-4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua-3))))
   `(company-tooltip ((t (:foreground ,origin-10 :background ,origin-2))))
   `(company-tooltip-selection ((t (:foreground ,origin-10 :background ,origin-3))))
   `(company-tooltip-annotation ((t (:foreground ,aqua-3))))
   `(company-tooltip-annotation-selection ((t (:foreground ,aqua-5))))
   `(company-tooltip-common ((t (:foreground ,aqua-3))))
   `(company-tooltip-common-selection ((t (:foreground ,magenta-5))))
   `(company-scrollbar-fg ((t (:background ,origin-9))))
   `(company-scrollbar-bg ((t (:background ,origin-1))))
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

(deftheme bare "Code illuminated.")

(let ((class '((class color) (min-colors 89)))
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
   `(default ((,class (:foreground ,bare-grey :background ,bare-light-grey))))
   `(cursor ((,class (:background ,bare-grey))))
   `(region ((,class (:background ,bare-region))))
   `(highlight ((,class (:background ,bare-grey))))
   `(font-lock-builtin-face ((,class (:foreground ,bare-magenta))))
   `(font-lock-keyword-face ((,class (:foreground ,bare-magenta))))
   `(font-lock-type-face ((,class (:foreground ,bare-green))))
   `(font-lock-function-name-face ((,class (:foreground ,bare-green))))
   `(font-lock-string-face ((,class (:foreground ,bare-magenta))))
   `(font-lock-comment-face ((,class (:foreground ,bare-purple))))
   `(comint-highlight-prompt ((,class (:foreground ,bare-blue))))
   `(diff-added ((,class (:foreground ,bare-green))))
   `(diff-removed ((,class (:foreground ,bare-magenta))))
   `(diff-hunk-header ((,class (:foreground ,bare-blue))))
   `(diff-file-header ((,class (:foreground ,bare-blue))))
   `(diff-header ((,class (:foreground ,bare-blue))))
   `(ido-first-match ((,class (:foreground ,bare-green))))
   `(ido-only-match ((,class (:foreground ,bare-green))))
   `(ido-subdir ((,class (:foreground ,bare-yellow))))
   `(ido-indicator ((,class (:foreground ,bare-green))))
   `(match ((,class (:background ,bare-region))))
   `(compilation-info ((,class (:foreground ,bare-green))))
   (custom-theme-set-variables 'bare
    `(ansi-color-names-vector
      [,bare-black
       ,bare-magenta ;; red
       ,bare-green
       ,bare-yellow ;; yellow
       ,bare-blue
       ,bare-magenta
       ,bare-blue ;; cyan
       ,bare-white]))
   `(minibuffer-prompt ((,class (:foreground ,bare-blue))))
   ))

(provide-theme 'bare)

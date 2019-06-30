(deftheme bare "Code illuminated.")

(let ((class '((class color) (min-colors 89)))
      (origin-0 "#eeeeee") ;; black
      (black "#1d1e1a")
      (grey "#7c7c7c")
      (light-grey "#b0b0b0")
      (dark-cloud "#d0d0d0")
      (cloud "#e0e0e0")
      
      (red "#800000") ;; red
      ;; (origin-9 "#f44336") ;; bright red
      
      (green "#365d2e")
      
      (yellow "#cf8f2e")

      ;; (blue "#486ab4")

      ;; (purple "#9933ff")

      ;; (cyan "#486ab4") ;; currently the same as blue

      (white "#eeeeee")

      (origin-10 "#1d1e1a")
      (origin-9 "#800000")
      (magenta-5 "#ff3399")
      (mint-5 "#800000")
      (mint-6 "#800000")
      (aqua-3 "#800000")
      (aqua-4 "#800000")
      (aqua-5 "#800000")
      (daffodil-5 "#800000")
      (lavender-5 "#800000")
      (bare-light-grey "#800000")
      (bare-grey "#800000")
      (bare-black "#1b1d1e")
      (bare-white "#eeeeee")
      (bare-yellow "#cf8f2e")
      (bare-blue "#800000")
      (bare-green "#365d2e")
      (bare-magenta "#e13dfc")
      (bare-purple "#8b008b")
      (bare-region "#dcb9b9"))
  
  ;; view font-lock under cursor with C-u C-x =
  (custom-theme-set-faces
   'bare
   `(default ((t (:foreground ,black :background ,white))))
   `(cursor ((t (:background ,red))))
   `(region ((t (:background ,cloud))))
   `(highlight ((t (:background ,cloud))))
   `(font-lock-builtin-face ((t (:foreground ,aqua-5))))
   `(font-lock-keyword-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,mint-6))))
   `(font-lock-function-name-face ((t (:foreground ,daffodil-5))))
   `(font-lock-string-face ((t (:foreground ,lavender-5))))
   `(font-lock-comment-face ((t (:foreground ,grey))))
   `(comint-highlight-prompt ((t (:foreground ,origin-10))))
   `(diff-added ((t (:foreground ,origin-10))))
   `(diff-removed ((t (:foreground ,origin-10))))
   `(diff-hunk-header ((t (:foreground ,origin-10))))
   `(diff-file-header ((t (:foreground ,origin-10))))
   `(diff-header ((t (:foreground ,origin-10))))
   
   `(ido-first-match ((t (:foreground ,aqua-5))))
   `(ido-only-match ((t (:foreground ,red))))
   `(ido-subdir ((t (:foreground ,daffodil-5))))
   `(ido-indicator ((t (:foreground ,origin-10))))

   `(mode-line
     ((t (:background ,white
                      :foreground ,black
                      :box (:line-width -1 :color ,light-grey)))))
   `(mode-line-inactive
     ((t (:background ,white
                      :foreground ,black
                      :box (:line-width -1 :color ,cloud)))))
   
   `(match ((t (:background ,yellow))))
   `(compilation-info ((t (:foreground ,origin-10))))
   
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,aqua-5))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,aqua-4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua-3))))
   
   `(company-tooltip ((t (:foreground ,light-grey :background ,cloud))))
   `(company-tooltip-selection ((t (:foreground ,grey :background ,dark-cloud))))
   `(company-tooltip-annotation ((t (:foreground ,aqua-3))))
   `(company-tooltip-annotation-selection ((t (:foreground ,aqua-5))))
   `(company-tooltip-common ((t (:foreground ,aqua-3))))
   `(company-tooltip-common-selection ((t (:foreground ,cloud))))
   `(company-scrollbar-fg ((t (:background ,origin-9))))
   `(company-scrollbar-bg ((t (:background ,origin-0))))
   `(company-preview ((t (:foreground ,origin-9 :background ,cloud))))
   `(company-preview-common ((t (:foreground ,origin-9 :background ,cloud))))
   (custom-theme-set-variables
    'bare
    `(ansi-color-names-vector
      [,bare-black
       ,red
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

(deftheme lumo "Code illuminated.")

(let ((class '((class color) (min-colors 89)))
      (foreground "#fcfcfa")
      (background "#2d2a2e")
      (cursor-color "#fcfcfa")
      
      (black "#383539")
      (bright-black "#989599")
      
      (red "#ff6188")
      (bright-red "#ff89a6")
      
      (green "#a9dc76")
      (bright-green "#c5e0a9")

      (yellow "#fc9867")
      (bright-yellow "#ffd866")

      (blue "#78dce8")
      (bright-blue "#c9edf1")

      (magenta "#ab9df2")
      (bright-magenta "#b294bb")

      (cyan "#78e8cf")
      (bright-cyan "#c4f9ed")

      (white "#c3c1c3")
      (bright-white "#f8f8f2")
      
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
      (lumo-light-grey "#eeeeee")
      (lumo-grey "#444444")
      (lumo-black "#1b1d1e")
      (lumo-white "#eeeeee")
      (lumo-yellow "#cf8f2e")
      (lumo-blue "#486ab4")
      (lumo-green "#365d2e")
      (lumo-magenta "#e13dfc")
      (lumo-purple "#8b008b")
      (lumo-region "#dcb9b9"))

  ;; view font-lock under cursor with C-u C-x =
  (custom-theme-set-faces
   'lumo
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor ((t (:background ,cursor-color))))
   `(region ((t (:background ,green))))
   `(highlight ((t (:background ,black))))
   `(font-lock-builtin-face ((t (:foreground ,green))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-function-name-face ((t (:foreground ,green))))
   `(font-lock-string-face ((t (:foreground ,bright-yellow))))
   `(font-lock-comment-face ((t (:foreground ,bright-black))))
   `(font-lock-constant-face ((t (:foreground ,magenta))))
   
   `(comint-highlight-prompt ((t (:foreground ,green))))
   
   `(diff-added ((t (:foreground ,green))))
   `(diff-removed ((t (:foreground ,red))))
   `(diff-hunk-header ((t (:foreground ,green))))
   `(diff-file-header ((t (:foreground ,green))))
   `(diff-header ((t (:foreground ,origin-10))))
   
   `(ido-first-match ((t (:foreground ,bright-yellow))))
   `(ido-only-match ((t (:foreground ,green))))
   `(ido-subdir ((t (:foreground ,blue))))
   `(ido-indicator ((t (:foreground ,bright-yellow))))
   
   `(match ((t (:foreground ,black :background ,bright-yellow)))) ;; used in ripgrep
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
   `(company-preview ((t (:foreground ,origin-9 :background ,origin-2))))
   `(company-preview-common ((t (:foreground ,origin-9 :background ,origin-2))))
   (custom-theme-set-variables
    'lumo
    `(ansi-color-names-vector
      [,lumo-black
       ,lumo-magenta ;; red
       ,lumo-green
       ,lumo-yellow ;; yellow
       ,lumo-blue
       ,lumo-magenta
       ,lumo-blue ;; cyan
       ,lumo-white]))
   `(minibuffer-prompt ((,class (:foreground ,origin-10))))
   ))

;; parens?
;; rainbow parens?

(provide-theme 'lumo)

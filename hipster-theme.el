;; mode line sexy maker
(set-cursor-color "#d33682")
(set-face-attribute 'mode-line nil
                    :background "#FFFFEA"
                    :box '(:line-width 2 :color "black" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :background "#FFFFEA"
                    :box '(:line-width 4 :color "#FFFFEA" :style nil))

;; Fonts and whatnot
(set-face-attribute 'default nil
                    ;:family "Source Code Pro"
                    :height 125
                    :weight 'normal
                    :width 'normal)

;; Colors (or lack thereof)
(let ((blue "#55B5DB")
      (green "#9FCA56")
      (yellow "#DCCD69")
      (red "#CE4045")
      (dark-blue "#4F99D3")
      (intense-green "#8BE03C"))

  (custom-set-faces
   ;; Fancy Colors
   `(default ((t (:background ,"#FFFFEA"))))
   `(error ((t (:foreground ,red :weight bold :underline (:color ,red :style line)))))
   `(isearch ((t (:background ,"white" :foreground ,"blue" :box (:line-width 1 :color ,dark-blue) :weight bold))))
   `(lazy-highlight ((t (:background ,"white" :foreground ,"#858D8A" :box (:line-width 1 :color ,dark-blue)))))
   `(secondary-selection ((t (:background ,"grey"))))
   `(trailing-whitespace ((t (:background ,"grey"))))
   `(match ((t (:weight bold :foreground ,"white" :background ,intense-green))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,"black"))))
   `(font-lock-warning-face ((t (:foreground , "red" :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,"#708090")))) ;; Perhaps this should be black too?
   `(font-lock-constant-face ((t (:foreground ,"black"))))
   `(font-lock-doc-face ((t (:foreground ,"black"))))
   `(font-lock-function-name-face ((t (:foreground ,"black"))))
   `(font-lock-keyword-face ((t (:foreground ,"#3F00FF"))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,"#555555")))) ;; Perhaps this should just be black as well?
   `(font-lock-type-face ((t (:foreground ,"black"))))
   `(font-lock-variable-name-face ((t (:foreground ,"black"))))
   ))

(provide 'hipster-theme)

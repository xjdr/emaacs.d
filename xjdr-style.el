;;; xjdr-style.el

;;; UI
(if window-system
    (progn
      (custom-set-faces
       '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#586e75"))))
       '(show-paren-match ((nil (:background "#1793d0"))))
       '(region ((nil (:background "#eee8d5"))))
       '(font-lock-function-name-face ((t (:foreground "#6c71c4"))))
       '(font-lock-type-face ((t ( :foreground "#859900"))))
       '(font-lock-keyword-face ((t ( :foreground "#d33682"))))
    )

      (set-cursor-color "#d33682")
      (set-face-attribute 'default nil
                          :family "Lato"
                          :height 150
                          :weight 'Light
                          :width 'normal)
      (set-face-attribute 'mode-line nil
                          :foreground "#859900"
                          :background "#073642"
                          :box '(:line-width 6 :color "#073642" :style nil))
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "#859900"
                          :background "#eee8d5"
                          :box '(:line-width 6 :color "#eee8d5" :style nil))
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (set-fringe-mode 0))
;;     (global-font-lock-mode 0))

;; No menu bar when running from a terminal.
  (menu-bar-mode 0)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#111111" :foreground "White"))))
   '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
   '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
   '(hl-line ((nil (:background "#222222"))))
   '(region ((nil (:background "#657489"))))
   '(show-paren-match ((nil (:background "#1793d0"))))
   '(show-paren-mismatch ((((class color)) (:background "red")))))
  )

(provide 'xjdr-style)

;;; xjdr-style.el ends here

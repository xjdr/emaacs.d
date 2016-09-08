;;; xjdr-style.el

;;; UI
(let ((blue "#55B5DB")
      (green "#9FCA56")
      (yellow "#DCCD69")
      (red "#CE4045")
      (orange "#cb4b16")
      (magenta "#d33682")
      (purple "#A074C4")
      (background "#151718")
      (background-2 "#1E2326")
      (background-3 "#0D1011")
      (background-4 "#101112")
      (text "#D4D7D6")
      (text-2 "#858D8A")
      (text-3 "#41535B")
      (text-4 "#2F3C42")
      (text-highlight "#FFFFFF")
      (text-region "#434546")
      (text-dired "#A0A0A0")
      (input-text "#CCCCCC")
      (light-blue "#75E5F4")
      (dark-blue "#4F99D3")
      (intense-green "#8BE03C"))

  (custom-set-faces
   ;; Basics
   `(default ((t (:background ,background :foreground ,text))))
   `(cursor ((t (:background ,input-text :foreground ,background))))
   `(highlight ((t (:background ,text-highlight))))
   `(minibuffer-prompt ((t (:foreground ,dark-blue :weight bold))))
   `(region ((t (:background ,text-region))))
   `(error ((t (:foreground ,red :weight bold :underline (:color ,red :style line)))))

   `(isearch ((t (:background ,background :foreground ,text :box (:line-width 1 :color ,dark-blue) :weight bold))))
   `(lazy-highlight ((t (:background ,background :foreground ,text-2 :box (:line-width 1 :color ,dark-blue)))))
   `(secondary-selection ((t (:background ,background-2))))
   `(trailing-whitespace ((t (:background ,background-3))))
   `(match ((t (:weight bold :foreground ,background :background ,intense-green))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,text-3))))
   `(font-lock-constant-face ((t (:foreground ,orange))))
   `(font-lock-doc-face ((t (:foreground ,blue))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,green))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,blue))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-warning-face ((t (:weight bold :inherit (error)))))

   ;; Parens
   `(show-paren-match ((t (:foreground ,text-2 :underline (:color ,dark-blue :style line)))))
   `(show-paren-mismatch ((t (:foreground ,text-2 :underline (:color ,red :style line)))))

   ;; Dired
   `(dired-directory ((t (:foreground ,text :weight extrabold))))
   `(dired-header ((t (:foreground "white"  :background ,blue :weight bold))))
   `(dired-ignored ((t (:foreground ,text-3))))
   `(dired-flagged ((t (:foreground ,red :weight bold))))
   `(dired-marked ((t (:background ,blue :foreground "white" :weight normal))))
   `(dired-perm-write ((t (:foreground ,yellow :weight ultra-bold))))
   `(dired-symlink ((t (:foreground ,light-blue :weight normal))))
   `(dired-warning ((t (:inherit (font-lock-warning-face)))))
   )

  (set-cursor-color "#d33682")
  (set-face-attribute 'default nil
                      :family "Lato"
                      :height 150
                      :weight 'Light
                      :width 'normal)
  (set-face-attribute 'mode-line nil
                      :foreground "#859900"
                      :background "#111111"
                      :box '(:line-width 6 :color "#111111" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#859900"
                      :background "#222222"
                      :box '(:line-width 6 :color "#222222" :style nil))
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (set-fringe-mode 0))

(unless window-system
  (menu-bar-mode 0))

(provide 'xjdr-style)

;;; xjdr-style.el ends here

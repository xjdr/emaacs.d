;;  init.el
;;  xjdr - here be dragons and whatnot

;; shh
(setq ring-bell-function 'ignore)

;; home sweet home
(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;; Annoyances
(setq inhibit-splash-screen t
      ring-bell-function 'ignore)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode 0)
(unless window-system
  (menu-bar-mode 0))
(fset 'yes-or-no-p 'y-or-n-p)

;; NO SAVES
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-use-virtual-buffers t
      recentf-save-file (emacs-d "var/recentf")
      save-place-file (emacs-d "var/saved-places")
      ido-save-directory-list-file (emacs-d "var/ido-last.el"))

;; Display completions vertically
(setq ido-decorations (quote ("\n> " "" "\n  " "\n  ..." "[" "]"
			      " [No Match]" " [Matched]" " [Not Readable]"
			      " [Too Big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys ()
  "C-(n|p) is more intuitive in vertical layout."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
	mac-command-modifier 'super
	delete-by-moving-to-trash t
	trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;; Whitespace
(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

;; Custom Key Bindings
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-t") (lambda () (interactive) (compile "make -k test")))

;; Misc
(show-paren-mode)

;;;; Tramp is good
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))

;; eshell stuffs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq eshell-prompt-function (lambda nil
			       (concat
				(propertize (eshell/pwd) 'face `(:foreground "#A074C4"))
				(propertize " $ " 'face `(:foreground "black")))))
(setq eshell-highlight-prompt nil)

(fset 'eshell-on
      "\C-x1\M-xeshell\n")
(fset 'eshell-off
      "\C-x3\M-xbury-buffer\n\C-xo\M-xbury-buffer\n\M-xwindmove-left")

(defun toggle-eshell ()
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (execute-kbd-macro (symbol-function 'eshell-off))
    (execute-kbd-macro (symbol-function 'eshell-on))))

(global-set-key (kbd "C-x t") 'toggle-eshell)

;; Org
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "english")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)
(add-hook 'org-mode-hook 'visual-line-mode t)
(org-babel-do-load-languages 'org-babel-load-languages
			     '((sh         . t)
			       (C           .t)
			       (java       . t)
			       (js         . t)
			       (emacs-lisp . t)
			       (ditaa      . t)
			       (scala      . t)
			       (clojure    . t)
			       (python     . t)))

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
;		      :family "DejaVu Sans Mono"
		      :height 140
		      :weight 'Light
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

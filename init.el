;;  init.el
;;  xjdr - here be dragons and whatnot

;; shhhhhhhhh
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

;; Stop the garbage
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

;;;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))

;; eshell stuffs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
	     (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (concat "["
	      (if (> (length git-output) 0)
		  (substring git-output 0 -1)
		"(no branch)")
	      "]")
      )))


(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
	     (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat " ["
	      (if (> (length git-output) 0)
		  (substring git-output 0 -1)
		"(no branch)")
	      "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize ((lambda (p-lst)
	    (if (> (length p-lst) 3)
		(concat
		 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
					    (substring elm 0 1)))
			    (butlast p-lst 3)
			    "/")
		 "/"
		 (mapconcat (lambda (elm) elm)
			    (last p-lst 3)
			    "/"))
	      (mapconcat (lambda (elm) elm)
			 p-lst
			 "/")))
	  (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "#C678DD"))
	 (or (curr-dir-git-branch-string (eshell/pwd)))
	 (propertize "$ " 'face 'default))))

(setq eshell-highlight-prompt nil)

(fset 'eshell-on
      "\C-x1\M-xeshell\n")
(fset 'eshell-off
      "\M-xbury-buffer\n")
;      "\C-x3\M-xbury-buffer\n\C-xo\M-xbury-buffer\n\M-xwindmove-left")

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

;; Fonts and whatnot
  (set-face-attribute 'default nil
		      ;;:family "Source Code Pro"
		      :height 125
		      :weight 'normal
		      :width 'normal)

;; color test
(defvar atom-one-dark-colors-alist
  '(("atom-one-dark-accent"   . "#528BFF")
    ("atom-one-dark-fg"       . "#ABB2BF")
    ("atom-one-dark-bg"       . "#282C34")
    ("atom-one-dark-bg-1"     . "#121417")
    ("atom-one-dark-bg-hl"    . "#2F343D")
    ("atom-one-dark-gutter"   . "#666D7A")
    ("atom-one-dark-accent"   . "#AEB9F5")
    ("atom-one-dark-mono-1"   . "#ABB2BF")
    ("atom-one-dark-mono-2"   . "#828997")
    ("atom-one-dark-mono-3"   . "#5C6370")
    ("atom-one-dark-cyan"     . "#56B6C2")
    ("atom-one-dark-blue"     . "#61AFEF")
    ("atom-one-dark-purple"   . "#C678DD")
    ("atom-one-dark-green"    . "#98C379")
    ("atom-one-dark-red-1"    . "#E06C75")
    ("atom-one-dark-red-2"    . "#BE5046")
    ("atom-one-dark-orange-1" . "#D19A66")
    ("atom-one-dark-orange-2" . "#E5C07B")
    ("atom-one-dark-gray"     . "#3E4451")
    ("atom-one-dark-silver"   . "#AAAAAA")
    ("atom-one-dark-black"    . "#0F1011"))
  "List of Atom One Dark colors.")

(defmacro atom-one-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
	 ,@ (mapcar (lambda (cons)
		      (list (intern (car cons)) (cdr cons)))
		    atom-one-dark-colors-alist))
     ,@body))

(atom-one-dark-with-color-variables
  (custom-set-faces

   `(default ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg))))
   `(success ((t (:foreground ,atom-one-dark-green))))
   `(warning ((t (:foreground ,atom-one-dark-orange-2))))
   `(error ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(link ((t (:foreground ,atom-one-dark-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,atom-one-dark-blue :underline t :weight normal))))
   `(cursor ((t (:background ,atom-one-dark-accent))))
   `(fringe ((t (:background ,atom-one-dark-bg))))
   `(region ((t (:background ,atom-one-dark-gray))))
   `(highlight ((t (:background ,atom-one-dark-gray))))
   `(hl-line ((t (:background ,atom-one-dark-bg-hl))))
   `(vertical-border ((t (:foreground ,atom-one-dark-mono-3))))
   `(secondary-selection ((t (:background ,atom-one-dark-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,atom-one-dark-silver))))

   `(font-lock-builtin-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-comment-face ((t (:foreground ,atom-one-dark-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,atom-one-dark-blue))))
   `(font-lock-keyword-face ((t (:foreground ,atom-one-dark-purple))))
   `(font-lock-preprocessor-face ((t (:foreground ,atom-one-dark-mono-2))))
   `(font-lock-string-face ((t (:foreground ,atom-one-dark-green))))
   `(font-lock-type-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,atom-one-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,atom-one-dark-mono-3 :bold t))))

   ;; ido
   `(ido-first-match ((t (:foreground ,atom-one-dark-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,atom-one-dark-blue))))
   `(ido-virtual ((t (:foreground ,atom-one-dark-mono-3))))

   ;; org-mode
   `(org-date ((t (:foreground ,atom-one-dark-cyan))))
   `(org-footnote ((t (:foreground ,atom-one-dark-cyan))))
   `(org-sexp-date ((t (:foreground ,atom-one-dark-cyan))))

   ;; isearch
   `(isearch ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-purple))))
   `(isearch-fail ((t (:foreground ,atom-one-dark-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-bg-1 :underline ,atom-one-dark-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ))

(set-cursor-color "#d33682")
(set-face-attribute 'mode-line nil
		    :background "black"
		    :foreground "#AAAAAA"
		    :box '(:line-width 4 :color "black" :style nil))
(set-face-attribute 'mode-line-inactive nil
		    :background  "#282C34"
		    :foreground "#AAAAAA"
		    :box '(:line-width 4 :color "#282C34" :style nil))

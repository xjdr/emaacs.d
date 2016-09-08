;;;  init.el
;;  xjdr

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

;; Get rid of some superfluous cruft
(setq ring-bell-function 'ignore)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;; External Packages
;;(load (emacs-d "packages"))
;;; Personal Elisp functions
(load (emacs-d "xjdr-defuns") 'missing-ok)
;;; Personal Style
(load (emacs-d "xjdr-style") 'missing-ok)
;;; org
(load (emacs-d "xjdr-org") 'missing-ok)
;;; no backup files, no auto-saving
(setq make-backup-files nil)
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;;;; Ido
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

;;;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        delete-by-moving-to-trash t
        trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;;;; Mouse
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))


;;; TAB behavior
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)   ; never use tabs to indent.


;; Use auto indentation only in programming modes.
;; (hook-into-modes '(lambda ()
;;                     (local-set-key (kbd "RET") 'newline-and-indent))
;;                  '(prog-mode-hook))

;; Line wrap at 100 char for all programming modes.
;; An indicator line will be drawn by `fci-mode` defined in `packages.el`
(hook-into-modes '(lambda ()
                    (set-fill-column 100))
                 '(prog-mode-hook))

;;;; Whitespace
(require 'whitespace)
;;(hook-into-modes 'whitespace-mode '(prog-mode-hook))
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

;;;; Annoyances
(setq inhibit-splash-screen t
      ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable commonly unintended key presses.
(global-unset-key (kbd "C-z")) ; suspend-frame
(global-unset-key (kbd "s-p")) ; ns-print-buffer
(global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel

;;; Custom Key Bindings
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
;;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c t r") (lambda () (interactive) (compile "make -k test")))


;;;; Disabled commands
(dolist (cmd
         '(erase-buffer
           upcase-region
           downcase-region
           dired-find-alternate-file
           narrow-to-region))
  (put cmd 'disabled nil))

;;;; Misc
(show-paren-mode)
(global-auto-revert-mode)

;;;; Internal Packages
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(when window-system
  (add-hook 'after-init-hook 'server-start t))

(defun init-duration-message ()
  "Print time spent in initialization to *Messages*."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Initialization complete.  (%.3fs)\n%s" elapsed (make-string 80 ?\-))))

(add-hook 'after-init-hook 'init-duration-message 'append)

;; Do the correct thing for markdown
(defun my-markdown-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.md\\'" buffer-file-name))
    (insert "OK")
    (org-mode)
    ))

;; eshell visor
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

  (setq eshell-prompt-function (lambda nil
    (concat
     (propertize (eshell/pwd) 'face `(:foreground "#A074C4"))
     (propertize " $ " 'face `(:foreground "white")))))
  (setq eshell-highlight-prompt nil)

;;; init.el ends here

;;; xjdr-org.el

;; Org
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "english")

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)
(add-hook 'org-mode-hook 'org-src-color-blocks-dark)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

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

(defun org-src-color-blocks-dark ()
  "Colors the block headers and footers to make them stand out more for dark themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background "#002E41"))))
   '(org-block-background
     ((t (:background "#000000"))))
   '(org-block
     ((t (:background "#000000"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background "#002E41"))))))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(custom-set-faces
    '(org-level-1 ((t (:foreground "purple" :bold t)))))

(provide 'xjdr-org)

;;; xjdr-org.el ends here

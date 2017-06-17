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

(provide 'org)

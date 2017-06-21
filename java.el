;; Java Stuffs
(use-package javadoc-lookup
  :ensure t
  :config
  (javadoc-add-artifacts [io.netty netty-all 4.1.11.Final])
  (javadoc-add-artifacts [com.google.guava guava 21.0])
  (javadoc-add-artifacts [junit junit 4.12])
  (javadoc-add-artifacts [org.apache.thrift libthrift 0.9.3])
  )


(use-package java-imports
  :ensure t
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  )

(require 'autoinsert)
(require 'skeleton)
(setq java-boilerplate-skeleton '(nil
                                  '(text-mode)
                                  "package "
                                  (replace-regexp-in-string "/" "."
                                                            (replace-regexp-in-string "/$" ""
                                                                                      (elt (split-string (file-name-directory (buffer-file-name)) "java/") 1)))
                                  ";" \n \n
                                  "public class " (file-name-base (buffer-file-name)) " {" \n \n
                                  "  " _ \n
                                  "  public " (file-name-base (buffer-file-name)) "() {" \n
                                  "}" \n
                                  \n
                                  "}" \n
                                  '(java-mode)))

(setq java-use-infer nil)

(define-auto-insert
  '("\\.java\\'" . "Java skeleton")
  java-boilerplate-skeleton)

(defun java-test-name (test-kind)
  "convert from java implementation name to test name"
  (let ((basename (replace-regexp-in-string "main/java" "test/java" (file-name-sans-extension (buffer-file-name)))))
    (concat basename (if test-kind (symbol-name test-kind) "") "Test.java")))

(defun java-impl-name ()
  "convert from java test name to implementation"
  (concat
   (replace-regexp-in-string "FunctionalTest\\|IntegrationTest\\|UnitTest\\|Test" ""
                             (replace-regexp-in-string "test/java" "main/java"
                                                       (file-name-sans-extension (buffer-file-name)))) ".java"))

(defun java-open-functional-test ()
  (interactive)
  (find-file (java-test-name 'Functional)))

(defun java-open-generic-test ()
  (interactive)
  (find-file (java-test-name nil)))

(defun java-open-integration-test ()
  (interactive)
  (find-file (java-test-name 'Integration)))

(defun java-open-unit-test ()
  (interactive)
  (find-file (java-test-name 'Unit)))

(defun java-open-implementation ()
  (interactive)
  (find-file (java-impl-name)))

(require 'dash)

(flycheck-define-checker infer
  "A Java syntax and style checker using infer.
See URL http://fbinfer.com/"
  :command ("infer" "--" "mvn" "-f"
            (eval (-> (projectile-project-root)
                      (concat "pom.xml")
                      (expand-file-name)))
            "compile")
  :error-patterns
  ((error line-start (file-name) ":" line ":" " error:" (message) line-end)
   (warning line-start (file-name) ":" line ":" " warning:" (message) line-end)
   (info line-start (file-name) ":" line ":" " info:" (message) line-end))
  :modes java-mode)


(flycheck-define-checker mvn
  "A Maven Java synax checker."
  :command ("mvn" "-f"
            (eval (-> (projectile-project-root)
                      (concat "pom.xml")
                      (expand-file-name)))
            "compile")
  :error-patterns ((error line-start "[ERROR] " (file-name) ":[" line "," column "]"
                          (message) line-end)
                   (warning line-start "[WARNING] " (file-name) ":[" line "," column "]"
                            (message) line-end))
  :modes java-mode)

(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'mvn 'infer))

(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq-local compilation-environment (list
                                                 (concat "FILE_NAME=" (buffer-file-name))
                                                 (concat "NOINFER=" (if java-use-infer "" "1"))))
            (editorconfig-mode)
            (flycheck-mode)
            (setq flycheck-checker 'mvn
            (java-imports-scan-file)
            (subword-mode)
            (c-set-style "custom-java")
            (define-key java-mode-map (kbd "C-c i") 'java-imports-add-import-dwim)
            (define-key java-mode-map (kbd "C-c t f") 'java-open-functional-test)
            (define-key java-mode-map (kbd "C-c t g") 'java-open-generic-test)
            (define-key java-mode-map (kbd "C-c t i") 'java-open-integration-test)
            (define-key java-mode-map (kbd "C-c t t") 'java-open-implementation)
            (define-key java-mode-map (kbd "C-c t u") 'java-open-unit-test)
            )))

(provide 'java)

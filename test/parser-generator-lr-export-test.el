;;; parser-generator-lr-export-test.el --- Tests for LR(k) Parser Export -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator-lr-export)
(require 'ert)

(defun parser-generator-lr-export-test-to-elisp ()
  "Test `parser-generator-lr-export'."
  (message "Started tests for (parser-generator-lr-export-to-elisp)")

  ;; Generate parser
  (parser-generator-set-grammar
   '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))

  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))

  ;; Test parse
  (should
   (equal
    '(2 2 2 1 1)
    (parser-generator-lr-parse)))
  (message "Passed parse before export")

  ;; Export parser
  (let ((export (parser-generator-lr-export-to-elisp "e--")))

    (with-temp-buffer
      (insert export)
      (eval-buffer)
      (should
       (equal
        t
        (fboundp 'e---parse)))
      (when (fboundp 'e---parse)
        (should
         (equal
          '(2 2 2 1 1)
          (e---parse))))
      (message "Passed parse for exported parser")))

  (let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (insert "aabb")

    (parser-generator-set-grammar '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b" (lambda(args) (let ((list "")) (dolist (item args) (when item (setq list (format "%s%s" item list)))) list)))) (S e)) Sp))
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (setq
     parser-generator-lex-analyzer--function
     (lambda (index)
       (with-current-buffer "*a*"
         (when (<= (+ index 1) (point-max))
           (let ((start index)
                 (end (+ index 1)))
             (let ((token (buffer-substring-no-properties start end)))
               `(,token ,start . ,end)))))))

    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer "*a*"
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end))))))

    (should
     (equal
      "bbaaba"
      (parser-generator-lr-translate)))

    (message "Passed translate before export")

    ;; Export parser
    (let ((export (parser-generator-lr-export-to-elisp "fa")))
      (message "export:\n%s\n" export)
      (with-temp-buffer
        (insert export)
        (eval-buffer)
        (should
         (equal
          t
          (fboundp 'fa-translate))))

      (message "Executing exported translater")

      (when (fboundp 'fa-translate)
        (should
         (equal
          "bbaaba"
          (fa-translate))))
      (message "Passed translate for exported parser")))

  (message "Passed tests for (parser-generator-lr-export-to-elisp)"))

(defun parser-generator-lr-export-test ()
  "Run test."
  (parser-generator-lr-export-test-to-elisp))


(provide 'parser-generator-lr-export-test)

;;; parser-generator-lr-export-test.el ends here

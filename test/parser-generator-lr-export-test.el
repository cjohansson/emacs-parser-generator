;;; parser-generator-lr-export-test.el --- Tests for LR(k) Parser Export -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator-lr-export)
(require 'ert)

(defun parser-generator-lr-export-test-incremental ()
  "Test incremental parse and translate."
  (message "Started incremental tests")

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
      '(2 2 2 1 1)
      (parser-generator-lr-parse)))
    (should
     (equal
      "bbaaba"
      (parser-generator-lr-translate)))

    ;; Export parser
    (let ((export (parser-generator-lr-export-to-elisp "ba")))
      (message "export:\n%s\n" export)
      (with-temp-buffer
        (insert export)
        (eval-buffer)
        (should
         (equal
          t
          (fboundp 'ba-parse)))
        (should
         (equal
          t
          (fboundp 'ba-translate))))

      (when (fboundp 'ba-parse)
        (should
         (equal
          '(2 2 2 1 1)
          (ba-parse))))

      (when (fboundp 'ba-translate)
        (should
         (equal
          "bbaaba"
          (ba-translate))))))

  (should
   (equal
    t
    (fboundp 'ba--parse)))

  (when (fboundp 'ba--parse)
    (let ((regular-parse (ba--parse)))
      (let ((regular-parse-history (nth 3 regular-parse)))
        ;; (message "regular-parse-history: %s" regular-parse-history)
        (let ((history-length (length regular-parse-history))
              (history-index 0)
              (history)
              (iterated-history))
          (while (< history-index history-length)
            (setq history (nth history-index regular-parse-history))
            (let ((input-tape-index (nth 0 history))
                  (pushdown-list (nth 1 history))
                  (output (nth 2 history))
                  (translation (nth 3 history))
                  (translation-symbol-table (nth 4 history))
                  (history-list iterated-history))

              (message "\nIncremental %s" history-index)
              (message "regular-parse: %s" regular-parse)
              (message "input-tape-index: %s" input-tape-index)
              (message "pushdown-list: %s" pushdown-list)
              (message "output: %s" output)
              (message "translation: %s" translation)
              (message "translation-symbol-table: %s" translation-symbol-table)
              (message "history-list: %s\n" history-list)

              (let ((incremental-parse
                     (ba--parse
                      input-tape-index
                      pushdown-list
                      output
                      translation
                      translation-symbol-table
                      history-list)))
                ;; (message "incremental-parse: %s" incremental-parse)
                (should
                 (equal
                  regular-parse
                  incremental-parse))
                (message "Passed incremental parse test %s" (1+ history-index)))

              (push history iterated-history)
              (setq history-index (1+ history-index))))))))

  (message "Passed incremental tests"))

(defun parser-generator-lr-export-test-parse ()
  "Test exported parser."
  (message "Started parse tests")

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

    (message "export:\n%s\n" export)
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

  (message "Passed parse tests"))

(defun parser-generator-lr-export-test-translate ()
  "Test exported translater."
  (message "Started translate tests")

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
      (with-temp-buffer
        (insert export)
        (eval-buffer)
        (should
         (equal
          t
          (fboundp 'fa-translate))))

      (when (fboundp 'fa-translate)
        (should
         (equal
          "bbaaba"
          (fa-translate))))
      (message "Passed translate for exported parser")))

  (message "Passed translate tests"))

(defun parser-generator-lr-export-test ()
  "Run test."
  (parser-generator-lr-export-test-parse)
  (parser-generator-lr-export-test-translate)
  (parser-generator-lr-export-test-incremental))


(provide 'parser-generator-lr-export-test)

;;; parser-generator-lr-export-test.el ends here

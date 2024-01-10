;;; parser-generator-lex-analyzer-test.el --- Tests for lex-analyzer -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator-lex-analyzer)
(require 'ert)

(defun parser-generator-lex-analyzer-test--peek-next-look-ahead ()
  "Test `parser-generator-lex-analyzer--peek-next-look-ahead'."
  (message "Starting tests for (parser-generator-lex-analyzer--peek-next-look-ahead)")

  (setq parser-generator-lex-analyzer--index nil)
  (setq parser-generator-lex-analyzer--function nil)
  (setq parser-generator--look-ahead-number nil)

  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens)
            (next-token))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (setq next-token (nth (1- index) string))
         (push next-token tokens)
         (setq index (1+ index)))
       (list (nreverse tokens)))))
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))
  (parser-generator-lex-analyzer--reset)
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))

  (message "Passed failing lex analysis")

  (setq parser-generator--look-ahead-number 1)
  (parser-generator-lex-analyzer--peek-next-look-ahead)
  (should
   (equal
    '(("a" 1 . 2))
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (setq parser-generator--look-ahead-number 2)
  (should
   (equal
    '(("a" 1 . 2) ("b" 2 . 3))
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (setq parser-generator--look-ahead-number 10)
  (should
   (equal
    '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5) ($) ($) ($) ($) ($) ($))
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens)
            (next-token))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (setq next-token (nth (1- index) string))
         (when (string= (car next-token) "d")
           (error "Invalid token: %s" next-token))
         (push next-token tokens)
         (setq index (1+ index)))
       (list (nreverse tokens)))))

  (parser-generator-lex-analyzer--reset)
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))

  (setq parser-generator--look-ahead-number 6)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5) ("e" 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens)
            (new-state)
            (next-token))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (setq next-token (nth (1- index) string))
         (cond
          ((equal state nil)
           (if (string= (car next-token) "c")
               (progn
                 (push next-token tokens)
                 (setq index (1+ index))
                 (setq next-token (nth (1- index) string))
                 (push next-token tokens)
                 (setq index (1+ index))
                 (setq new-state (list 'epislon)))
             (if (string= (car next-token) "e")
                 (error "Invalid token: %s" next-token)
               (setq next-token (nth (1- index) string))
               (when (string= (car next-token) "d")
                 (error "Invalid token: %s" next-token))
               (push next-token tokens)
               (setq index (1+ index)))))
          ((equal state (list 'epislon))
           (setq next-token (nth (1- index) string))
           (when (string= (car next-token) "d")
             (error "Invalid token: %s" next-token))
           (push next-token tokens)
           (setq index (1+ index)))
          (t
           (error "Invalid state: %s" state))))
       (list (nreverse tokens) nil new-state))))
  (parser-generator-lex-analyzer--reset)
  (should
   (equal
    '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5) ("e" 5 . 6) ($))
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (message "Passed peek for state-based lexer")

  (message "Ended tests for (parser-generator-lex-analyzer--peek-next-look-ahead)"))

(defun parser-generator-lex-analyzer-test--pop-token ()
  "Test `parser-generator-lex-analyzer--pop-token'."
  (message "Starting tests for (parser-generator-lex-analyzer--pop-token)")

  (setq parser-generator-lex-analyzer--index nil)
  (setq parser-generator-lex-analyzer--function nil)
  (setq parser-generator--look-ahead-number nil)

  (should-error
   (parser-generator-lex-analyzer--pop-token))
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (nreverse tokens)))))
  (should-error
   (parser-generator-lex-analyzer--pop-token))

  (message "Passed failing lex analysis 2")

  (setq parser-generator--look-ahead-number 1)
  (parser-generator-lex-analyzer--reset)

  (should
   (equal
    '(("a" 1 . 2))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(("b" 2 . 3))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(($))
    (parser-generator-lex-analyzer--pop-token)))

  (setq parser-generator--look-ahead-number 1)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5) ("e" 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens)
            (new-state)
            (next-token))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (setq next-token (nth (1- index) string))
         (cond
          ((equal state nil)
           (if (string= (car next-token) "c")
               (progn
                 (push next-token tokens)
                 (setq index (1+ index))
                 (setq next-token (nth (1- index) string))
                 (push next-token tokens)
                 (setq index (1+ index))
                 (setq new-state (list 'epislon)))
             (if (string= (car next-token) "e")
                 (error "Invalid token: %s" next-token)
               (setq next-token (nth (1- index) string))
               (when (string= (car next-token) "d")
                 (error "Invalid token: %s" next-token))
               (push next-token tokens)
               (setq index (1+ index)))))
          ((equal state (list 'epislon))
           (setq next-token (nth (1- index) string))
           (when (string= (car next-token) "d")
             (error "Invalid token: %s" next-token))
           (push next-token tokens)
           (setq index (1+ index)))
          (t
           (error "Invalid state: %s" state))))
       (list (nreverse tokens) nil new-state))))
  (parser-generator-lex-analyzer--reset)
  (should
   (equal
    '(("a" 1 . 2))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(("b" 2 . 3))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(("c" 3 . 4))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(("d" 4 . 5))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(("e" 5 . 6))
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(($))
    (parser-generator-lex-analyzer--pop-token)))

  (message "Passed pop for state-based lexer")

  (message "Ended tests for (parser-generator-lex-analyzer--pop-token)"))

(defun parser-generator-lex-analyzer-test ()
  "Run test."
  ;; (setq debug-on-error t)
  (parser-generator-lex-analyzer-test--peek-next-look-ahead)
  (parser-generator-lex-analyzer-test--pop-token))


(provide 'parser-generator-lex-analyzer-test)

;;; parser-generator-lex-analyzer-test.el ends here

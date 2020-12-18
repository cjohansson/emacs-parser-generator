;;; parser-generator-lex-analyzer.el --- Lex-analyzer library -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator)


;;; Variables:


(defvar parser-generator-lex-analyzer--function
  nil
  "Function used as lex-analyzer.")

(defvar parser-generator-lex-analyzer--index
  nil
  "Index in lex-analyzer.")

(defvar parser-generator-lex-analyzer--reset-function
  nil
  "Function used when resetting lex-analyzer.")


;; Functions


(defun parser-generator-lex-analyzer--peek-next-look-ahead ()
  "Peek next look-ahead number of tokens via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead number!"))
  (funcall
   parser-generator-lex-analyzer--function
   parser-generator-lex-analyzer--index
   parser-generator--look-ahead-number))

(defun parser-generator-lex-analyzer--pop-token ()
  "Pop next token via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))
  (let ((token (funcall
                parser-generator-lex-analyzer--function
                parser-generator-lex-analyzer--index
                1)))
    (setq parser-generator-lex-analyzer--index
          (1+ parser-generator-lex-analyzer--index))
    token))

(defun parser-generator-lex-analyzer--reset ()
  "Reset lex-analyzer."
  (setq parser-generator-lex-analyzer--index 0)
  (when parser-generator-lex-analyzer--reset-function
    (funcall parser-generator-lex-analyzer--reset-function)))


(provide 'parser-generator-lex-analyzer)

;;; parser-generator-lex-analyzer.el ends here

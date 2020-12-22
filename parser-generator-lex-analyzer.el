;;; parser-generator-lex-analyzer.el --- Lex-analyzer library -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator)


;;; Variables:


(defvar parser-generator-lex-analyzer--get-function
  nil
  "Get token information function.")

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


(defun parser-generator-lex-analyzer--get-function (token)
  "Get information about TOKEN."
  (unless parser-generator-lex-analyzer--get-function
    (error "Missing lex-analyzer get function!"))
  (funcall parser-generator-lex-analyzer--get-function token))


(defun parser-generator-lex-analyzer--peek-next-look-ahead ()
  "Peek next look-ahead number of tokens via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead number!"))
  (let ((look-ahead)
        (look-ahead-length 0)
        (index parser-generator-lex-analyzer--index))
    (while (< look-ahead-length parser-generator--look-ahead-number)
      (let ((next-look-ahead
             (funcall
              parser-generator-lex-analyzer--function
              index)))
        (if next-look-ahead
            (progn
              (unless (listp (car next-look-ahead))
                (setq next-look-ahead (list next-look-ahead)))
              (dolist (next-look-ahead-item next-look-ahead)
                (when (< look-ahead-length parser-generator--look-ahead-number)
                  (push next-look-ahead-item look-ahead)
                  (setq look-ahead-length (1+ look-ahead-length))
                  (setq index (cdr (cdr next-look-ahead-item))))))
          (push (list parser-generator--e-identifier) look-ahead)
          (setq look-ahead-length (1+ look-ahead-length))
          (setq index (1+ index)))))
    (nreverse look-ahead)))

(defun parser-generator-lex-analyzer--pop-token ()
  "Pop next token via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))
  (let ((token (funcall
                     parser-generator-lex-analyzer--function
                     parser-generator-lex-analyzer--index)))
    (unless (listp (car token))
      (setq token (list token)))
    (let ((first-token (car token)))
      (setq parser-generator-lex-analyzer--index
            (cdr (cdr first-token)))
      first-token)))

(defun parser-generator-lex-analyzer--reset ()
  "Reset lex-analyzer."
  (setq parser-generator-lex-analyzer--index 1)
  (when parser-generator-lex-analyzer--reset-function
    (funcall parser-generator-lex-analyzer--reset-function)))


(provide 'parser-generator-lex-analyzer)

;;; parser-generator-lex-analyzer.el ends here

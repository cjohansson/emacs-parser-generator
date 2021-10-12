;;; parser-generator-lex-analyzer.el --- Lex-analyzer library -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator)


;;; Variables:


(defvar
  parser-generator-lex-analyzer--get-function
  nil
  "Get token contents.  Anything other than nil is expected.")

(defvar
  parser-generator-lex-analyzer--function
  nil
  "Get next token like '(a b . c) or nil, expects signal if input-tape is invalid.")

(defvar-local
  parser-generator-lex-analyzer--index
  nil
  "Index in lex-analyzer.")

(defvar
  parser-generator-lex-analyzer--reset-function
  nil
  "Function used when resetting lex-analyzer.")

(defvar-local
  parser-generator-lex-analyzer--move-to-index-flag
  nil
  "Non-nil means move index to value.")


;; Functions


(defun parser-generator-lex-analyzer--get-function (token)
  "Get information about TOKEN."
  (unless parser-generator-lex-analyzer--get-function
    (error "Missing lex-analyzer get function! Token: %s" token))
  (let ((meta-information))
    (condition-case error
        (progn
          (setq meta-information
                (funcall
                 parser-generator-lex-analyzer--get-function
                 token)))
      (error (error
              "Lex-analyze failed to get token meta-data of %s, error: %s"
              token
              (car (cdr error)))))
    (unless meta-information
      (error "Could not find any token meta-information for: %s" token))
    meta-information))

(defun parser-generator-lex-analyzer--peek-next-look-ahead ()
  "Peek next look-ahead number of tokens via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead-number!"))
  (let ((look-ahead)
        (look-ahead-length 0)
        (index parser-generator-lex-analyzer--index)
        (k (max
            1
            parser-generator--look-ahead-number)))
    (while (<
            look-ahead-length
            k)
      (condition-case error
          (progn
            (setq-local
             parser-generator-lex-analyzer--move-to-index-flag
             nil)
            (let ((next-look-ahead
                   (funcall
                    parser-generator-lex-analyzer--function
                    index)))
              (if parser-generator-lex-analyzer--move-to-index-flag
                  (progn
                    (setq-local
                     parser-generator-lex-analyzer--index
                     parser-generator-lex-analyzer--move-to-index-flag)
                    (setq
                     index
                     parser-generator-lex-analyzer--index))
                (if next-look-ahead
                    (progn
                      (unless (listp (car next-look-ahead))
                        (setq next-look-ahead (list next-look-ahead)))
                      (dolist (next-look-ahead-item next-look-ahead)
                        (when (<
                               look-ahead-length
                               k)
                          (push next-look-ahead-item look-ahead)
                          (setq look-ahead-length (1+ look-ahead-length))
                          (setq index (cdr (cdr next-look-ahead-item))))))
                  (push (list parser-generator--eof-identifier) look-ahead)
                  (setq look-ahead-length (1+ look-ahead-length))
                  (setq index (1+ index))))))
        (error
         (error
          "Lex-analyze failed to peek next look-ahead at %s, error: %s"
          index
          error))))
    (nreverse look-ahead)))

(defun parser-generator-lex-analyzer--pop-token ()
  "Pop next token via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead-number!"))
  (let ((continue t)
        (tokens))
    (while continue
      (condition-case error
          (progn
            (setq-local
             parser-generator-lex-analyzer--move-to-index-flag
             nil)
            (let ((token
                   (funcall
                    parser-generator-lex-analyzer--function
                    parser-generator-lex-analyzer--index)))
              (if parser-generator-lex-analyzer--move-to-index-flag
                  (progn
                    (setq-local
                     parser-generator-lex-analyzer--index
                     parser-generator-lex-analyzer--move-to-index-flag))
                (when token
                  (unless (listp (car token))
                    (setq token (list token)))
                  (let ((first-token (car token)))
                    (setq
                     parser-generator-lex-analyzer--index
                     (cdr (cdr first-token)))
                    (push
                     first-token
                     tokens)))
                (setq
                 continue
                 nil))))
        (error
         (error
          "Lex-analyze failed to pop token at %s, error: %s"
          parser-generator-lex-analyzer--index
          (car (cdr error))))))
    (nreverse tokens)))

(defun parser-generator-lex-analyzer--reset ()
  "Reset lex-analyzer."
  (setq parser-generator-lex-analyzer--index 1)
  (when parser-generator-lex-analyzer--reset-function
    (funcall parser-generator-lex-analyzer--reset-function)))


(provide 'parser-generator-lex-analyzer)

;;; parser-generator-lex-analyzer.el ends here

;;; parser-generator-lex-analyzer.el --- Lex-analyzer library -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.


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
  "Get next token like \='(a b . c) or nil, expects signal if input-tape is invalid.")

(defvar
  parser-generator-lex-analyzer--state-init
  nil
  "Initial value of state.")

(defvar
  parser-generator-lex-analyzer--reset-function
  nil
  "Function used when resetting lex-analyzer.")

(defvar
  parser-generator-lex-analyzer--index-init
  1
  "Initial value of index.")


;;; Buffer-Local Variables:


(defvar-local
  parser-generator-lex-analyzer--buffered-response
  nil
  "Buffered tokens of lex-analyzer.")

(defvar-local
  parser-generator-lex-analyzer--index
  nil
  "Index in lex-analyzer.")

(defvar-local
  parser-generator-lex-analyzer--state
  nil
  "State of lex-analyzer.")


;; Functions


(defun parser-generator-lex-analyzer--get-function (token)
  "Get information about TOKEN."
  (unless parser-generator-lex-analyzer--get-function
    (signal
     'error
     (list
      (format "Missing lex-analyzer get function! Token: %s" token)
      token)))
  (let ((meta-information))
    (condition-case error
        (progn
          (setq
           meta-information
           (funcall
            parser-generator-lex-analyzer--get-function
            token)))
      (error
       (signal
        'error
        (list
         (format
          "Lex-analyze failed to get token meta-data of %s, error: %s"
          token
          (car (cdr error)))
         token
         (car (cdr error))))))
    (unless meta-information
      (signal
       'error
       (list
        (format "Could not find any token meta-information for: %s" token)
        token)))
    meta-information))

(defun parser-generator-lex-analyzer--peek-next-look-ahead ()
  "Peek next look-ahead number of tokens via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index when peeking!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead-number when peeking!"))
  (let ((look-ahead)
        (look-ahead-length
         0)
        (index
         parser-generator-lex-analyzer--index)
        (state
         parser-generator-lex-analyzer--state)
        (k
         (max
          1
          parser-generator--look-ahead-number)))

    (while (<
            look-ahead-length
            k)

      (let* ((result-list
              (parser-generator-lex-analyzer--get-buffered-lex
               index
               state))
             (token
              (nth 0 result-list))
             (new-index
              (nth 2 result-list))
             (new-state
              (nth 3 result-list)))
        (push
         token
         look-ahead)
        (setq
         look-ahead-length
         (1+ look-ahead-length))
        (setq
         index
         new-index)
        (setq
         state
         new-state)))

    (nreverse look-ahead)))

(defun parser-generator-lex-analyzer--pop-token ()
  "Pop next token via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index when popping!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead-number when popping!"))
  (let* ((result-list
          (parser-generator-lex-analyzer--get-buffered-lex
           parser-generator-lex-analyzer--index
           parser-generator-lex-analyzer--state))
         (token
          (nth 0 result-list))
         (new-index
          (nth 2 result-list))
         (new-state
          (nth 3 result-list)))
    (setq-local
     parser-generator-lex-analyzer--index
     new-index)
    (setq-local
     parser-generator-lex-analyzer--state
     new-state)
    (list token)))

(defun parser-generator-lex-analyzer--reset ()
  "Reset lex-analyzer."
  (setq
   parser-generator-lex-analyzer--buffered-response
   (make-hash-table :test 'equal))
  (setq
   parser-generator-lex-analyzer--index
   parser-generator-lex-analyzer--index-init)
  (setq
   parser-generator-lex-analyzer--state
   parser-generator-lex-analyzer--state-init)
  (when parser-generator-lex-analyzer--reset-function
    (funcall parser-generator-lex-analyzer--reset-function)))

(defun parser-generator-lex-analyzer--get-buffered-lex (index state)
  "Get next token in stream, use buffer to only call function when needed."

  (unless (gethash
           index
           parser-generator-lex-analyzer--buffered-response)
    (let ((continue t)
          (tmp-index index)
          (tmp-state state))
      (unless parser-generator-lex-analyzer--function
        (error "Missing lex-analyzer function!"))
      (while continue
        (condition-case error
            (progn
              (let* ((result-list
                      (funcall
                       parser-generator-lex-analyzer--function
                       tmp-index
                       tmp-state))
                     (tokens
                      (nth 0 result-list))
                     (move-to-index-flag
                      (nth 1 result-list))
                     (new-state
                      (nth 2 result-list)))
                (if move-to-index-flag
                    (progn
                      (setq
                       tmp-index
                       move-to-index-flag)
                      (setq
                       tmp-state
                       new-state))

                  (if tokens

                      (progn
                        (unless (listp (car tokens))
                          (setq tokens (list tokens)))

                        (let* ((first-token (car tokens))
                               (first-token-start (car (cdr first-token)))
                               (first-token-end (cdr (cdr first-token))))
                          (when (< index first-token-start)
                            (let ((token-start index))
                              (while (< token-start first-token-start)
                                (puthash
                                 token-start
                                 (list
                                  first-token
                                  nil
                                  first-token-end
                                  new-state)
                                 parser-generator-lex-analyzer--buffered-response)
                                (setq
                                 token-start
                                 (1+ token-start))))))

                        (dolist (token tokens)
                          (let ((token-start (car (cdr token)))
                                (token-end (cdr (cdr token))))
                            (puthash
                             token-start
                             (list token nil token-end new-state)
                             parser-generator-lex-analyzer--buffered-response))))

                    ;; Fill up look-ahead with EOF-identifier if we found nothing
                    (puthash
                     index
                     (list (list parser-generator--eof-identifier) nil (1+ index) nil)
                     parser-generator-lex-analyzer--buffered-response))

                  (setq
                   continue
                   nil))))

          (error
           (error
            "Lex-analyze failed to get next token at: %s in state: %s, error: %s"
            index
            state
            error))))))

  (gethash
   index
   parser-generator-lex-analyzer--buffered-response))


(provide 'parser-generator-lex-analyzer)

;;; parser-generator-lex-analyzer.el ends here

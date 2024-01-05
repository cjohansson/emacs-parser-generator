;;; parser-generator-lex-analyzer.el --- Lex-analyzer library -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.


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

(defvar-local
  parser-generator-lex-analyzer--index
  nil
  "Index in lex-analyzer.")

(defvar
  parser-generator-lex-analyzer--index-init
  1
  "Initial value of index.")

(defvar-local
  parser-generator-lex-analyzer--state
  nil
  "State of lex-analyzer.")

(defvar
  parser-generator-lex-analyzer--state-init
  nil
  "Initial value of state.")

(defvar
  parser-generator-lex-analyzer--reset-function
  nil
  "Function used when resetting lex-analyzer.")


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
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function when peeking!"))
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
      (condition-case error
          (progn
            (let* ((result-list
                   (funcall
                    parser-generator-lex-analyzer--function
                    index
                    state))
                   (token
                    (nth 0 result-list))
                   (move-to-index-flag
                    (nth 1 result-list))
                   (new-index
                    (nth 2 result-list))
                   (new-state
                    (nth 3 result-list)))
              (if move-to-index-flag
                  (progn
                    (setq
                     index
                     move-to-index-flag)
                    (setq
                     state
                     new-state))
                (if token
                    (progn
                      (setq index new-index)
                      (unless (listp (car token))
                        (setq token (list token)))
                      (let ((token-count (length token))
                            (token-index 0))
                        (while
                            (and
                             (<
                              look-ahead-length
                              k)
                             (<
                              token-index
                              token-count))
                          (let ((next-look-ahead-item
                                 (nth token-index token)))
                            (push
                             next-look-ahead-item
                             look-ahead)
                            (setq
                             look-ahead-length
                             (1+ look-ahead-length))
                            (setq
                             token-index
                             (1+ token-index))))))

                  ;; Fill up look-ahead with EOF-identifier if we found nothing
                  (push (list parser-generator--eof-identifier) look-ahead)
                  (setq look-ahead-length (1+ look-ahead-length))
                  (setq index (1+ index))))))

        (error
         (error
          "Lex-analyze failed to peek next look-ahead at %s, error: %s, look-ahead: %S"
          index
          error
          look-ahead))))
    (nreverse look-ahead)))

(defun parser-generator-lex-analyzer--pop-token ()
  "Pop next token via lex-analyzer."
  (unless parser-generator-lex-analyzer--index
    (error "Missing lex-analyzer index when popping!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function when popping!"))
  (unless parser-generator--look-ahead-number
    (error "Missing look-ahead-number when popping!"))
  (let ((continue t)
        (tokens))
    (while continue
      (condition-case error
          (progn
            (let* ((result-list
                    (funcall
                     parser-generator-lex-analyzer--function
                     parser-generator-lex-analyzer--index
                     parser-generator-lex-analyzer--state))
                   (token
                    (nth 0 result-list))
                   (move-to-index-flag
                    (nth 1 result-list))
                   (new-index
                    (nth 2 result-list))
                   (new-state
                    (nth 3 result-list)))
              (if move-to-index-flag
                  (progn
                    (setq-local
                     parser-generator-lex-analyzer--index
                     move-to-index-flag)
                    (setq-local
                     parser-generator-lex-analyzer--state
                     new-state))
                (setq
                 parser-generator-lex-analyzer--index
                 new-index)
                (when token
                  (unless (listp (car token))
                    (setq token (list token)))
                  (let ((first-token (car token)))
                    (push
                     first-token
                     tokens)))
                (setq
                 continue
                 nil))))
        (error
         (error
          "Lex-analyze failed to pop token at %s %s, error: %s"
          parser-generator-lex-analyzer--index
          parser-generator-lex-analyzer--state
          (car (cdr error))))))
    (nreverse tokens)))

(defun parser-generator-lex-analyzer--reset ()
  "Reset lex-analyzer."
  (setq
   parser-generator-lex-analyzer--index
   parser-generator-lex-analyzer--index-init)
  (setq
   parser-generator-lex-analyzer--state
   parser-generator-lex-analyzer--state-init)
  (when parser-generator-lex-analyzer--reset-function
    (funcall parser-generator-lex-analyzer--reset-function)))


(provide 'parser-generator-lex-analyzer)

;;; parser-generator-lex-analyzer.el ends here

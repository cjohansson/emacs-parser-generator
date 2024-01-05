;;; parser-generator-ll-export.el --- Export LL(k) Parser -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator-ll)

(defun parser-generator-ll-export-to-elisp (namespace &optional header copyright)
  "Export parser with NAMESPACE and a optional HEADER and COPYRIGHT."
  (message "\n;; Starting generation of elips..\n")

  ;; Make sure all requisites are defined
  (unless parser-generator-ll--table
    (error "Missing generated table!"))
  (unless parser-generator--table-productions-number-reverse
    (error "Table for reverse production-numbers is undefined!"))
  (unless parser-generator--table-look-aheads-p
    (error "Table for valid look-aheads is undefined!"))
  (unless parser-generator--look-ahead-number
    (error "Missing a look-ahead number!"))
  (unless parser-generator--e-identifier
    (error "Missing definition for e-identifier!"))
  (unless parser-generator--eof-identifier
    (error "Missing definition for EOF-identifier!"))
  (unless parser-generator--table-non-terminal-p
    (error "Table for non-terminals is undefined!"))
  (unless parser-generator--table-terminal-p
    (error "Table for terminals is undefined!"))
  (unless parser-generator--table-translations
    (error "Table for translations by production-number is undefined!"))
  (unless parser-generator-lex-analyzer--get-function
    (error "Missing lex-analyzer get function!"))
  (unless parser-generator-lex-analyzer--function
    (error "Missing lex-analyzer function!"))

  (let ((code))
    (with-temp-buffer
      (goto-char (point-min))

      ;; Header
      (insert
       (format
        ";;; %s.el --- Exported Emacs Parser Generator -*- lexical-binding: t -*-\n\n"
        namespace))

      ;; Optional copyright
      (when copyright
        (insert copyright))

      (insert ";;; Commentary:\n\n\n;;; Code:\n\n")

      ;; Optional header
      (when header
        (insert header))

      (insert "\n;;; Variables:\n\n\n")

      ;; Grammar start
      (insert
       (format
        "(defvar\n  %s--grammar-start\n %s\n  \"The start of grammar.\")\n\n"
        namespace
        (if (symbolp (parser-generator--get-grammar-start))
            (format "'%s" (parser-generator--get-grammar-start))
          (format "\"%s\"" (parser-generator--get-grammar-start)))))

      ;; Generated table
      (insert
       (format
        "(defvar\n  %s--table\n  %S\n  \"The generated table.\")\n\n"
        namespace
        parser-generator-ll--table))

      ;; Table production-number
      (insert
       (format
        "(defvar\n  %s--table-productions-number-reverse\n  %S\n  \"The hash-table indexed by production-number and value is production.\")\n\n"
        namespace
        parser-generator--table-productions-number-reverse))

      ;; Table terminals
      (insert
       (format
        "(defvar\n  %s--table-terminal-p\n  %S\n  \"The hash-table of valid terminals.\")\n\n"
        namespace
        parser-generator--table-terminal-p))

      ;; Table non-terminals
      (insert
       (format
        "(defvar\n  %s--table-non-terminal-p\n  %S\n  \"The hash-table of valid non-terminals.\")\n\n"
        namespace
        parser-generator--table-non-terminal-p))

      ;; Table translations
      (insert
       (format
        "(defvar\n  %s--table-translations\n  %S\n  \"The hash-table of translations.\")\n\n"
        namespace
        parser-generator--table-translations))

      ;; E-identifier
      (insert
       (format
        "(defvar\n  %s--e-identifier\n  '%S\n  \"The e-identifier.\")\n\n"
        namespace
        parser-generator--e-identifier))

      ;; EOF-identifier
      (insert
       (format
        "(defvar\n  %s--eof-identifier\n  '%S\n  \"The end-of-file-identifier.\")\n\n"
        namespace
        parser-generator--eof-identifier))

      ;; Look-ahead number
      (insert
       (format
        "(defvar\n  %s--look-ahead-number\n  %S\n  \"The look-ahead number.\")\n\n"
        namespace
        parser-generator--look-ahead-number))

      (insert "\n;;; Local Variables:\n\n")

      ;; Index
      (insert
       (format
        "(defvar-local\n  %s-lex-analyzer--index\n  0\n  \"The current index of the lex-analyzer.\")\n\n"
        namespace))

      ;; State
      (insert
       (format
        "(defvar-local\n  %s-lex-analyzer--state\n  nil\n  \"The current state of the lex-analyzer.\")\n\n"
        namespace))

      (insert "\n;;; Variable Functions:\n\n")

      ;; Lex-Analyzer Get Function
      (insert
       (format
        "(defvar\n  %s-lex-analyzer--get-function\n  (lambda %S %S)\n  \"The lex-analyzer get function.\")\n\n"
        namespace
        (nth 2 parser-generator-lex-analyzer--get-function)
        (nth 3 parser-generator-lex-analyzer--get-function)))

      ;; Lex-Analyzer Function
      (insert
       (format
        "(defvar\n  %s-lex-analyzer--function\n  (lambda %S %S)\n  \"The lex-analyzer function.\")\n\n"
        namespace
        (nth 2 parser-generator-lex-analyzer--function)
        (nth 3 parser-generator-lex-analyzer--function)))

      ;; Lex-Analyzer Reset Function
      (insert
       (format
        "(defvar\n  %s-lex-analyzer--reset-function\n  "
        namespace))
      (if parser-generator-lex-analyzer--reset-function
          (insert
           (format
            "(lambda %S %S)\n"
            (nth 2 parser-generator-lex-analyzer--reset-function)
            (nth 3 parser-generator-lex-analyzer--reset-function)))
        (insert "nil\n"))
      (insert "  \"The lex-analyzer reset function.\")\n\n")

      (insert "\n;;; Functions:\n\n")

      (insert "\n;;; Functions for Lex-Analyzer:\n\n\n")

      ;; Lex-Analyzer Get Function
      (insert
       (format
        "(defun
  %s-lex-analyzer--get-function (token)
  \"Get information about TOKEN.\"
  (let ((meta-information))
    (condition-case
      error
      (progn
        (setq
          meta-information
          (funcall
            %s-lex-analyzer--get-function
            token)))"
        namespace
        namespace))
      (insert "
      (error (error
        \"Lex-analyze failed to get token meta-data of %s, error: %s\"
        token
        (car (cdr error)))))
    (unless meta-information
      (error \"Could not find any token meta-information for: %s\" token))
    meta-information))\n")

      ;; Lex-Analyzer Reset Function
      (insert
       (format "
(defun
  %s-lex-analyzer--reset
  ()
  \"Reset Lex-Analyzer.\"
  (setq
    %s-lex-analyzer--index
    1)
  (when
    %s-lex-analyzer--reset-function
    (funcall
      %s-lex-analyzer--reset-function)))\n"
               namespace
               namespace
               namespace
               namespace))

      ;; Lex-Analyzer Peek Next Look Ahead
      (insert
       (format "
(defun
  %s-lex-analyzer--peek-next-look-ahead
  ()
  \"Peek next look-ahead number of tokens via lex-analyzer.\"
  (let ((look-ahead)
        (look-ahead-length
          0)
        (index
          %s-lex-analyzer--index)
        (state
          %s-lex-analyzer--state)
        (k (max
            1
            %s--look-ahead-number)))
    (while (<
            look-ahead-length
            k)
      (condition-case error
          (progn
            (let* ((result-list
                   (funcall
                    %s-lex-analyzer--function
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
                  (push (list %s--eof-identifier) look-ahead)
                  (setq look-ahead-length (1+ look-ahead-length))
                  (setq index (1+ index))))))"
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace))
      (insert "
        (error
         (error
          \"Lex-analyze failed to peek next look-ahead at %s, error: %s, look-ahead: %S\"
          index
          error
          look-ahead))))
    (nreverse look-ahead)))\n")

      ;; Lex-Analyzer Pop Token
      (insert
       (format "
(defun
  %s-lex-analyzer--pop-token ()
  \"Pop next token via lex-analyzer.\"
  (let ((continue t)
        (tokens))
    (while continue
      (condition-case error
          (progn
            (let* ((result-list
                    (funcall
                     %s-lex-analyzer--function
                     %s-lex-analyzer--index
                     %s-lex-analyzer--state))
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
                     %s-lex-analyzer--index
                     move-to-index-flag)
                    (setq-local
                     %s-lex-analyzer--state
                     new-state))
                (setq
                 %s-lex-analyzer--index
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
                 nil))))"
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace))
      (insert "
        (error (error
                \"Lex-analyze failed to pop token at %s, error: %s\"")
      (insert (format "
                %s-lex-analyzer--index
                (car (cdr error))))))
    (nreverse tokens)))\n"
                      namespace))

      (insert "\n\n;;; Functions for Syntax-Analyzer / Parser:\n\n");

      ;; Get grammar production by number
      (insert
       (format "
(defun
  %s--get-grammar-production-by-number
  (production-number)
  \"If PRODUCTION-NUMBER exist, return it's production.\"
  (gethash
   production-number
   %s--table-productions-number-reverse))\n"
               namespace
               namespace))

      ;; Valid symbol p
      (insert
       (format "
(defun
  %s--valid-symbol-p
  (symbol)
  \"Return whether SYMBOL is valid or not.\"
  (let ((is-valid t))
    (unless (or
             (%s--valid-e-p symbol)
             (%s--valid-eof-p symbol)
             (%s--valid-non-terminal-p symbol)
             (%s--valid-terminal-p symbol))
      (setq is-valid nil))
    is-valid))\n"
               namespace
               namespace
               namespace
               namespace
               namespace))

      ;; Valid e-p
      (insert
       (format "
(defun
  %s--valid-e-p
  (symbol)
  \"Return whether SYMBOL is the e identifier or not.\"
  (eq
   symbol
   %s--e-identifier))\n"
               namespace
               namespace))

      ;; Valid EOF-p
      (insert
       (format "
(defun
  %s--valid-eof-p
  (symbol)
  \"Return whether SYMBOL is the EOF identifier or not.\"
  (eq
    symbol
    %s--eof-identifier))\n"
               namespace
               namespace))

      ;; Valid non-terminal-p
      (insert
       (format "
(defun
  %s--valid-non-terminal-p (symbol)
  \"Return whether SYMBOL is a non-terminal in grammar or not.\"
  (gethash
   symbol
   %s--table-non-terminal-p))\n"
               namespace
               namespace))

      ;; Valid terminal-p
      (insert
       (format "
(defun
  %s--valid-terminal-p (symbol)
  \"Return whether SYMBOL is a terminal in grammar or not.\"
  (gethash
   symbol
   %s--table-terminal-p))\n"
               namespace
               namespace))

      ;; Generate list of symbol
      (insert
       (format "
(defun %s--generate-list-of-symbol (k symbol)
  \"Generate list of K number of SYMBOL.\"
  (let ((list-index 0)
        (list))
    (while (< list-index k)
      (push symbol list)
      (setq list-index (1+ list-index)))
    list))
"
  namespace))

      ;; Get grammar translation by number
      (insert
       (format "
(defun
  %s--get-grammar-translation-by-number
  (production-number)
  \"If translation for PRODUCTION-NUMBER exist, return it.\"
  (gethash
    production-number
    %s--table-translations))\n"
               namespace
               namespace))

      ;; Parse / translate function
      (insert
       (format "
(defun %s--parse (&optional translate-p)
  \"Parse input via lex-analyzer and return parse trail.\"
  (let ((accept)
        (stack
         (if (> %s--look-ahead-number 1)
             (list
              (list
               (list
                %s--grammar-start)
               (%s--generate-list-of-symbol
                %s--look-ahead-number
                %s--eof-identifier))
              %s--eof-identifier)
           (list
            %s--grammar-start
            %s--eof-identifier)))
        (output)
        (eof-look-ahead
         (%s--generate-list-of-symbol
          %s--look-ahead-number
          %s--eof-identifier))
        (e-reduction
         (list %s--e-identifier))
        (translation)
        (translation-stack)
        (translation-symbol-table
         (make-hash-table :test 'equal))
        (terminal-stack '()))
    (%s-lex-analyzer--reset)
    (while (not accept)
      (let* ((state (car stack))
             (state-action-table
              (gethash
               (format \"%%S\" state)
               %s--table))
             (look-ahead-list
              (%s-lex-analyzer--peek-next-look-ahead))
             (look-ahead))

        (unless state-action-table
          (signal
           'error
           (list
            (format
             \"State action table lacks actions for state: '%%S'!\"
             state)
            state)))

        (if look-ahead-list
            (progn
              (dolist (look-ahead-list-item look-ahead-list)
                (push (car look-ahead-list-item) look-ahead))
              (setq look-ahead (reverse look-ahead)))
          (setq
           look-ahead
           eof-look-ahead))

        (unless (gethash
                 (format \"%%S\" look-ahead)
                 state-action-table)
          (let ((possible-look-aheads))
            (maphash
             (lambda (k _v) (push k possible-look-aheads))
             state-action-table)
            (signal
             'error
             (list
              (format
               \"Invalid look-ahead '%%S' in state: '%%S', valid look-aheads: '%%S'\"
               look-ahead
               state
               possible-look-aheads)
              look-ahead
              state
              possible-look-aheads))))

        (let* ((action
                (gethash
                 (format \"%%S\" look-ahead)
                 state-action-table))
               (action-type action))
          (when (listp action)
            (setq action-type (car action)))
          (cond

           ((equal action-type 'pop)
            (let ((popped-tokens
                   (%s-lex-analyzer--pop-token)))

              ;; Is it time for SDT?
              (when (and
                     translate-p
                     translation-stack
                     (string=
                      (car (car translation-stack))
                      (format \"%%S\" stack)))
                (let* ((translation-item (pop translation-stack))
                       (partial-translation
                        (%s--perform-translation
                         (nth 1 translation-item)
                         translation-symbol-table
                         (reverse (pop terminal-stack)))))
                  (setq
                   translation
                   partial-translation)))

              (pop stack)

              (when translate-p
                (let ((token-data)
                      (old-terminal-stack (car terminal-stack)))
                  (dolist (popped-token popped-tokens)
                    (push
                     popped-token
                     token-data))
                  (push
                   token-data
                   old-terminal-stack)
                  (setf
                   (car terminal-stack)
                   old-terminal-stack)))

              ;; Is it time for SDT?
              (when (and
                     translate-p
                     translation-stack
                     (string=
                      (car (car translation-stack))
                      (format \"%%S\" stack)))
                (let* ((translation-item (pop translation-stack))
                       (partial-translation
                        (%s--perform-translation
                         (nth 1 translation-item)
                         translation-symbol-table
                         (reverse (pop terminal-stack)))))
                  (setq
                   translation
                   partial-translation)))

              ))

           ((equal action-type 'reduce)

            ;; Is it time for SDT?
            (when (and
                   translate-p
                   translation-stack
                   (string=
                    (car (car translation-stack))
                    (format \"%%S\" stack)))
              (let* ((translation-item (pop translation-stack))
                     (partial-translation
                      (%s--perform-translation
                       (nth 1 translation-item)
                       translation-symbol-table
                       (reverse (pop terminal-stack)))))
                (setq
                 translation
                 partial-translation)))

            (pop stack)

            ;; Is it time for SDT?
            (when (and
                   translate-p
                   translation-stack
                   (string=
                    (car (car translation-stack))
                    (format  \"%%S\" stack)))
              (let* ((translation-item (pop translation-stack))
                     (partial-translation
                      (%s--perform-translation
                       (nth 1 translation-item)
                       translation-symbol-table
                       (reverse (pop terminal-stack)))))
                (setq
                 translation
                 partial-translation)))

            (when translate-p
              (push
               (list
                (format \"%%S\" stack)
                (nth 2 action))
               translation-stack)
              (push
               '()
               terminal-stack))

            (unless (equal (nth 1 action) e-reduction)
              (dolist (reduce-item (reverse (nth 1 action)))
                (push reduce-item stack)))
            (push
             (nth 2 action)
             output))

           ((equal action-type 'accept)
            (setq accept t))))))
    (list
     (reverse output)
     translation)))

(defun %s-parse ()
  (let ((parse (%s--parse)))
    (car parse)))

(defun %s-translate ()
  (let ((parse (%s--parse t)))
    (car (cdr parse))))

(defun %s--perform-translation (production-number symbol-table terminals)
  \"Perform translation by PRODUCTION-NUMBER, with SYMBOL-TABLE and TERMINALS.\"
  (let* ((production
          (%s--get-grammar-production-by-number
           production-number))
         (production-lhs
          (car (nth 0 production)))
         (production-rhs
          (nth 1 production))
         (translation)
         (args-1)
         (args-2))

    ;; Collect arguments for translation
    (let ((terminal-index 0))
      (dolist (rhs-item production-rhs)
        (cond

         ((%s--valid-non-terminal-p
           rhs-item)
          (let* ((non-terminal-value-list
                  (gethash rhs-item symbol-table))
                 (non-terminal-value
                  (pop non-terminal-value-list)))
            (push
             (car non-terminal-value)
             args-1)
            (push
             (car (cdr non-terminal-value))
             args-2)
            (puthash
             rhs-item
             non-terminal-value-list
             symbol-table)))

         ((%s--valid-terminal-p
           rhs-item)
          (push
           (%s-lex-analyzer--get-function
            (nth terminal-index terminals))
           args-1)
          (push
           (nth terminal-index terminals)
           args-2)
          (setq
           terminal-index
           (1+ terminal-index))))))
    (setq
     args-1
     (reverse args-1))
    (setq
     args-2
     (reverse args-2))

    (if (%s--get-grammar-translation-by-number
         production-number)
        (let ((partial-translation
               (funcall
                (%s--get-grammar-translation-by-number
                 production-number)
                args-1
                args-2))
              (old-symbol-value
               (gethash production-lhs symbol-table)))
          (push
           (list
            partial-translation
            args-2)
           old-symbol-value)
          (puthash
           production-lhs
           old-symbol-value
           symbol-table)
          (setq
           translation
           partial-translation))

      ;; When no translation is specified just use popped contents as translation
      (let ((partial-translation
             (list
              args-1
              args-2))
            (old-symbol-value
             (gethash production-lhs symbol-table)))
        (push
         partial-translation
         old-symbol-value)
        (puthash
         production-lhs
         old-symbol-value
         symbol-table)
        (setq
         translation
         (car partial-translation))))

    translation))

"
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               namespace
               ))

      ;; Footer
      (insert
       (format
        "\n(provide '%s)"
        namespace))

      (insert
       (format
        "\n\n;;; %s.el ends here"
        namespace))

      (setq
       code
       (buffer-substring-no-properties
        (point-min)
        (point-max))))
    (message "\n;; Completed generation of elips.\n")
    code))


(provide 'parser-generator-ll-export)

;;; parser-generator-ll-export.el ends here

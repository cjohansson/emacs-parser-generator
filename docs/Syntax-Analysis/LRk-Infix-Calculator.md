# Example LR(k) Infix Calculator

Example reproduced from GNU Bison manual [Infix Notation Calculator](https://www.gnu.org/software/bison/manual/html_node/Infix-Calc.html).

``` emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(setq
   parser-generator--e-identifier
   '%empty)
(parser-generator-set-look-ahead-number 1)
(setq
   parser-generator--global-attributes
   '(%left %precedence %right))
(setq
   parser-generator--context-sensitive-attributes
   '(%prec))
(setq
   parser-generator-lr--global-precedence-attributes
   '(%left %precedence %right))
(setq
   parser-generator-lr--context-sensitive-precedence-attribute
   '%prec)
(setq
   parser-generator--global-declaration
   '(
     (%left "-" "+")
     (%left "*" "/")
     (%precedence NEG)
     (%right "^")))
(parser-generator-set-grammar
   '(
     (start input line exp)
     ("+" "-" "*" "/" "^" "(" ")" "\n" NUM)
     (
      (start input)
      (input
       %empty
       (input line (lambda(args) (nth 1 args))))
      (line
       "\n"
       (exp "\n" (lambda(args) (nth 0 args))))
      (exp
       NUM
       (exp "+" exp (lambda(args) (+ (float (nth 0 args)) (nth 2 args))))
       (exp "-" exp (lambda(args) (- (float (nth 0 args)) (nth 2 args))))
       (exp "*" exp (lambda(args) (* (float (nth 0 args)) (nth 2 args))))
       (exp "/" exp (lambda(args) (/ (float (nth 0 args)) (nth 2 args))))
       ("-" exp %prec NEG (lambda(args) (- (float (nth 1 args)))))
       (exp "^" exp (lambda(args) (expt (float (nth 0 args)) (nth 2 args))))
       ("(" exp ")" (lambda(args) (nth 1 args)))))
     start))
(setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (with-current-buffer "*buffer*"
       (let ((token))
         (when
             (<
              index
              (point-max))
           (goto-char
            index)

           ;; Skip white-space(s)
           (when (looking-at-p "[\t ]+")
             (when
                 (search-forward-regexp "[^\t ]" nil t)
               (forward-char -1)))

           (cond
            ((looking-at "\\([0-9]+\\.[0-9]+\\|[0-9]+\\)")
             (setq
              token
              `(NUM ,(match-beginning 0) . ,(match-end 0))))
            ((looking-at "\\(\\+\\|-\\|*\\|/\\|\\^\\|)\\|(\\|\n\\)")
             (let ((symbol
                    (buffer-substring-no-properties
                     (match-beginning 0)
                     (match-end 0))))
               (setq
                token
                `(,symbol ,(match-beginning 0) . ,(match-end 0)))))
            (t (error "Unexpected input at %d!" index))))
         token))))

(setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (with-current-buffer "*buffer*"
       (let ((start (car (cdr token)))
             (end (cdr (cdr token))))
         (when (<= end (point-max))
           (let ((symbol
                  (buffer-substring-no-properties start end)))
             (when
                 (string-match-p "^\\([0-9]+\\.[0-9]+\\|[0-9]+\\)$" symbol)
               (setq
                symbol
                (string-to-number symbol)))
             symbol))))))

(parser-generator-process-grammar)
(parser-generator-lr-generate-parser-tables)

(let ((buffer (generate-new-buffer "*buffer*")))
    (switch-to-buffer buffer)
    (insert "4 + 4.5 - (34/(8*3+-3))\n")
    (let ((translate (parser-generator-lr-translate)))
      (should
      (equal
        6.880952380952381
        translate)))
    (message "Passed correct precedence of 4 + 4.5 - (34/(8*3+-3)) = 6.880952380952381")
      (kill-buffer))
```

[Back to LR(k) Parser](../LRk.md)
[Back to syntax analysis](../Syntax-Analysis.md)

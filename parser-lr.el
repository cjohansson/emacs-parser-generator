;;; parser-el.el --- LR(k) Parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser)


;;; Variables:

(defvar parser-lr--action-tables
  nil
  "Action-tables for grammar.")

(defvar parser-lr--goto-tables
  nil
  "Goto-tables for grammar.")

(defvar parser-lr--items
  nil
  "Hash-table for distinct LR-items in grammar.")


;; Helper Functions


(defun parser-lr--reset ()
  "Reset variables."
  (setq parser-lr--action-tables nil)
  (setq parser-lr--goto-tables nil)
  (setq parser-lr--items nil))


;; Main Algorithms


;; Algorithm 5.11, p. 393
(defun parser-lr--generate-action-tables ()
  "Generate action-tables for lr-grammar."
  (unless parser-lr--action-tables
    (let ((action-tables)
          (states '(shift reduce error))
          (added-actions (make-hash-table :test 'equal)))
      (dolist (goto-table parser-lr--goto-tables)
        (let ((goto-index (car goto-table))
              (gotos (car (cdr goto-table)))
              (found-action nil)
              (action-table))
          (let ((lr-items (gethash goto-index parser-lr--items)))
            (let ((lr-items-length (length lr-items)))
              ;; Where u is in (T U e)*k
              (dolist (state states)
                (let ((lr-item)
                      (lr-item-index 0)
                      (continue-loop t))
                  (while (and
                          (< lr-item-index lr-items-length)
                          continue-loop)
                    (setq lr-item (nth lr-item-index lr-items))
                    (cond

                     ((eq state 'shift)
                      ;; (a) f(u) = shift if [A -> B . C, v] is in LR-items, C != e and u is in EFF(Cv)
                      (when (nth 2 lr-item)
                        (let ((C (nth 2 lr-item))
                              (v (nth 3 lr-item)))
                          (let ((Cv (append C v)))
                            (when Cv
                              (let ((eff (parser--e-free-first Cv)))
                                (when eff
                                  ;; Go through eff-items and see if any item is a valid look-ahead of grammar
                                  ;; in that case save in action table a shift action here
                                  (let ((eff-index 0)
                                        (eff-item)
                                        (eff-length (length eff))
                                        (searching-match t))
                                    (while (and
                                            searching-match
                                            (< eff-index eff-length))
                                      (setq eff-item (nth eff-index eff))
                                      (when (parser--valid-look-ahead-p eff-item)
                                        (let ((hash-key (format "%s-%s-%s" goto-index state eff-item)))
                                          (unless (gethash hash-key added-actions)
                                            (puthash hash-key t added-actions)
                                            (setq searching-match nil))))
                                      (setq eff-index (1+ eff-index)))

                                    (unless searching-match
                                      (push (list eff-item 'shift) action-table)
                                      (setq found-action t))))))))))

                     ((eq state 'reduce)
                      ;; (b) f(u) = reduce i if [A -> B ., u] is in a and A -> B is production i in P, i > 1
                      (when (and
                             (nth 0 lr-item)
                             (not (nth 2 lr-item)))
                        (let ((A (nth 0 lr-item))
                              (B (nth 1 lr-item))
                              (u (nth 3 lr-item)))
                          (unless B
                            (setq B (list parser--e-identifier)))
                          (when (parser--valid-look-ahead-p u)
                            (let ((hash-key (format "%s-%s-%s" goto-index state u)))
                              (unless (gethash hash-key added-actions)
                                (puthash hash-key t added-actions)
                                (let ((production (list A B)))
                                  (let ((production-number (parser--get-grammar-production-number production)))
                                    (unless production-number
                                      (error "Expecting production number for %s from LR-item %s!" production lr-item))

                                    (if (and
                                         (= production-number 0)
                                         (= (length u) 1)
                                         (parser--valid-e-p (car u)))
                                        (progn
                                          ;; Reduction by first production
                                          ;; of empty look-ahead means grammar has been accepted
                                          (push (list u 'accept) action-table)
                                          (setq found-action t))

                                      ;; save reduction action in action table
                                      (push (list u 'reduce production-number) action-table)
                                      (setq found-action t))))))))))

                     ((eq state 'error)
                      (unless found-action
                        (error (format "Failed to find any action in set %s" lr-items)))
                      (setq continue-loop nil)))
                    (setq lr-item-index (1+ lr-item-index)))))))
          (parser--debug
           (message "%s actions %s" goto-index action-table))
          (when action-table
            (push (list goto-index (sort action-table 'parser--sort-list)) action-tables))))
      (setq parser-lr--action-tables (sort (nreverse action-tables) 'parser--sort-list))))
  parser-lr--action-tables)

;; Algorithm 5.9, p. 389
(defun parser-lr--generate-goto-tables ()
  "Calculate set of valid LR(k) items for grammar and a GOTO-table."
  (unless (or
           parser-lr--goto-tables
           parser-lr--items)
    (setq parser-lr--goto-tables nil)
    (setq parser-lr--items (make-hash-table :test 'equal))
    (let ((lr-item-set-new-index 0)
          (goto-table)
          (unmarked-lr-item-sets)
          (marked-lr-item-sets (make-hash-table :test 'equal))
          (symbols (append (parser--get-grammar-non-terminals) (parser--get-grammar-terminals))))

      (let ((e-set (parser-lr--items-for-prefix parser--e-identifier)))
        ;;(1) Place V(e) in S. The set V(e) is initially unmarked.
        (push `(,lr-item-set-new-index ,e-set) unmarked-lr-item-sets)
        (setq lr-item-set-new-index (1+ lr-item-set-new-index)))

      ;; (2) If a set of items a in S is unmarked
      ;; (3) Repeat step (2) until all sets of items in S are marked.
      (let ((popped-item)
            (lr-item-set-index)
            (lr-items)
            (goto-table-table))
        (while unmarked-lr-item-sets

          (setq popped-item (pop unmarked-lr-item-sets))
          (setq lr-item-set-index (car popped-item))
          (setq lr-items (car (cdr popped-item)))
          (parser--debug
           (message "lr-item-set-index: %s" lr-item-set-index)
           (message "lr-items: %s" lr-items)
           (message "popped-item: %s" popped-item))

          ;; (2) Mark a
          (puthash lr-items lr-item-set-index marked-lr-item-sets)

          (puthash lr-item-set-index lr-items parser-lr--items)
          (setq goto-table-table nil)

          ;; (2) By computing for each X in N u E, GOTO (a, X). (Algorithm 5.8 can be used here.)
          ;; V(X1,...,Xi) = GOTO(V(X1,...,Xi-1), Xi)
          (dolist (symbol symbols)
            (parser--debug
             (message "symbol: %s" symbol))

            (let ((prefix-lr-items (parser-lr--items-for-goto lr-items symbol)))

              ;; If a' = GOTO(a, X) is nonempty
              (when prefix-lr-items

                (parser--debug
                 (message "GOTO(%s, %s) = %s" lr-items symbol prefix-lr-items))

                ;; and is not already in S
                (let ((goto (gethash prefix-lr-items marked-lr-item-sets)))
                  (if goto
                      (progn
                        (parser--debug
                         (message "Set already exists in: %s" goto))
                        (push `(,symbol ,goto) goto-table-table))

                    (parser--debug
                     (message "Set is new"))

                    ;; Note that GOTO(a, X) will always be empty if all items in a
                    ;; have the dot at the right end of the production

                    ;; then add a' to S as an unmarked set of items
                    (push `(,symbol ,lr-item-set-new-index) goto-table-table)
                    (push `(,lr-item-set-new-index ,prefix-lr-items) unmarked-lr-item-sets)
                    (setq lr-item-set-new-index (1+ lr-item-set-new-index)))))))

          (setq goto-table-table (sort goto-table-table 'parser--sort-list))
          (push `(,lr-item-set-index ,goto-table-table) goto-table)))
      (setq parser-lr--goto-tables (sort goto-table 'parser--sort-list)))
    (unless
        (parser-lr--items-valid-p
         (parser--hash-values-to-list parser-lr--items t)) ;; TODO Should not use this debug function
      (error "Inconsistent grammar!")))
  parser-lr--goto-tables)

;; Algorithm 5.10, p. 391
(defun parser-lr--items-valid-p (lr-item-sets)
  "Return whether the set collection LR-ITEM-SETS is valid or not."
  (parser--debug
   (message "lr-item-sets: %s" lr-item-sets))
  (let ((valid-p t)
        (set-index 0)
        (set)
        (sets-length (length lr-item-sets))
        (set-length 0)
        (a)
        (a-look-ahead)
        (a-follow)
        (a-index 0)
        (b)
        (b-suffix)
        (b-follow)
        (b-suffix-follow)
        (b-suffix-follow-eff)
        (b-index 0))

    ;; Iterate each set
    (while (and
            valid-p
            (< set-index sets-length))
      (setq set (nth set-index lr-item-sets))
      (parser--debug
       (message "set: %s" set))

      ;; Iterate each set
      (setq a-index 0)
      (setq b-index 0)
      (setq set-length (length set))
      (while (and
              valid-p
              (< a-index set-length))
        (setq a (nth a-index set))
        (setq a-look-ahead (nth 2 a))

        (parser--debug
         (message "a: %s" a)
         (message "a-look-ahead: %s" a-look-ahead))

        ;; The only sets of LR items which need to be tested are those that contain a dot at the right end of a production
        (when (and
               (nth 1 a)
               (not a-look-ahead))
          (setq a-follow (nth 3 a))

          (parser--debug
           (message "a-follow: %s" a-follow))

          ;; Iterate each set again
          (while (and
                  valid-p
                  (< b-index set-length))
            (unless (= a-index b-index)
              (setq b (nth b-index set))
              (setq b-suffix (nth 2 b))
              (setq b-follow (nth 3 b))
              (setq b-suffix-follow (append b-suffix b-follow))
              (setq b-suffix-follow-eff (parser--e-free-first b-suffix-follow))

              (parser--debug
               (message "b: %s" b)
               (message "b-suffix: %s" b-suffix)
               (message "b-follow: %s" b-follow)
               (message "b-suffix-follow: %s" b-suffix-follow)
               (message "b-suffix-follow-eff: %s" b-suffix-follow-eff))

              (dolist (b-suffix-follow-eff-item b-suffix-follow-eff)
                (when (equal a-follow b-suffix-follow-eff-item)
                  (parser--debug
                   (message "Inconsistent grammar! %s conflicts with %s" a b))
                  (setq valid-p nil))))
            (setq b-index (1+ b-index))))
        (setq a-index (1+ a-index)))
      (setq set-index (1+ set-index)))

    valid-p))

;; Algorithm 5.8, p. 386
(defun parser-lr--items-for-prefix (γ)
  "Calculate valid LR-items for the viable prefix Γ."
  (let ((start (parser--get-grammar-start)))
    (unless (listp γ)
      (setq γ (list γ)))
    (unless (parser--valid-sentential-form-p γ)
      (error "Invalid sentential form γ!"))

    (let ((lr-item-exists (make-hash-table :test 'equal)))

      ;; 1

      ;; Iterate all productions in grammar
      (let ((lr-items-e)
            (start-productions (parser--get-grammar-rhs start)))

        ;; (a)
        (dolist (rhs start-productions)
          ;; Add [S -> . α] to V(e)
          (push `(,start nil ,rhs (e)) lr-items-e)
          (puthash `(,parser--e-identifier ,start nil ,rhs (,parser--e-identifier)) t lr-item-exists))

        ;; (b) Iterate every item in v-set(e), if [A -> . Bα, u] is an item and B -> β is in P
        ;; then for each x in FIRST(αu) add [B -> . β, x] to v-set(e), provided it is not already there
        (let ((found-new t))

          ;; Repeat this until no new item is found
          (while found-new
            (setq found-new nil)

            ;; Iterate every item in V(e)
            (dolist (item lr-items-e)
              (let ((prefix (nth 1 item))
                    (rhs (nth 2 item))
                    (suffix (nth 3 item)))

                ;; Without prefix
                (unless prefix

                  ;; Check if RHS starts with a non-terminal
                  (let ((rhs-first (car rhs)))
                    (parser--debug
                     (message "rhs-first: %s" rhs-first))
                    (when (parser--valid-non-terminal-p rhs-first)
                      (let ((rhs-rest (append (cdr rhs) suffix)))
                        (let ((rhs-rest-first (parser--first rhs-rest)))
                          (parser--debug
                           (message "rhs-rest-first: %s" rhs-rest-first))
                          (unless rhs-rest-first
                            (setq rhs-rest-first `((,parser--e-identifier))))
                          (let ((sub-production (parser--get-grammar-rhs rhs-first)))
                            (parser--debug
                             (message "sub-production: %s" sub-production))

                            ;; For each production with B as LHS
                            (dolist (sub-rhs sub-production)

                              ;; Set follow to nil if it's the e-identifier
                              (when (and
                                     (= (length sub-rhs) 1)
                                     (parser--valid-e-p (car sub-rhs)))
                                (setq sub-rhs nil))

                              (parser--debug
                               (message "sub-rhs: %s" sub-rhs))

                              ;; For each x in FIRST(αu)
                              (dolist (f rhs-rest-first)
                                (parser--debug
                                 (message "f: %s" f))

                                ;; Add [B -> . β, x] to V(e), provided it is not already there
                                (unless (gethash `(e ,rhs-first nil ,sub-rhs ,f) lr-item-exists)
                                  (puthash `(e ,rhs-first nil ,sub-rhs ,f) t lr-item-exists)
                                  (push `(,rhs-first nil ,sub-rhs ,f) lr-items-e)

                                  ;; (c) Repeat (b) until no more items can be added to V(e)
                                  (setq found-new t))))))))))))))

        (parser--debug
         (message "V(e) = %s" lr-items-e))

        (setq lr-items-e (sort lr-items-e 'parser--sort-list))

        ;; 2 Suppose that we have constructed V(X1,X2,...,Xi-1) we construct V(X1,X2,...,Xi) as follows:
        ;; Only do this step if prefix is not the e-identifier
        (let ((prefix-previous lr-items-e))
          (unless (and
                   (= (length γ) 1)
                   (parser--valid-e-p (car γ)))
            (dolist (prefix γ)
              (let ((lr-new-item))
                (setq lr-new-item (parser-lr--items-for-goto prefix-previous prefix))

                (parser--debug
                 (message "prefix: %s" prefix)
                 (message "prefix-previous: %s" prefix-previous)
                 (message "lr-new-item: %s" lr-new-item))

                (setq prefix-previous lr-new-item))))

          (parser--debug
           (message "γ: %s" γ))
          prefix-previous)))))

(defun parser-lr--items-for-goto (previous-lr-item x)
  "Calculate LR-items for GOTO(PREVIOUS-LR-ITEM, X)."
  (let ((lr-new-item)
        (lr-item-exists (make-hash-table :test 'equal)))
    (parser--debug (message "x: %s" x))

    (dolist (lr-item previous-lr-item)
      (let ((lr-item-lhs (nth 0 lr-item))
            (lr-item-prefix (nth 1 lr-item))
            (lr-item-suffix (nth 2 lr-item))
            (lr-item-look-ahead (nth 3 lr-item)))
        (let ((lr-item-suffix-first (car lr-item-suffix))
              (lr-item-suffix-rest (cdr lr-item-suffix)))

          ;; (a) If [A -> a . XiB, u] is in V(X1,...,Xi-1)
          (when (eq lr-item-suffix-first x)

            ;; Add [A -> aXi . B, u] to V(X1,...,Xi)
            (let ((combined-prefix (append lr-item-prefix (list x))))
              (parser--debug
               (message "lr-new-item-1: %s" `(,lr-item-lhs ,combined-prefix ,lr-item-suffix-rest ,lr-item-look-ahead)))
              (push `(,lr-item-lhs ,combined-prefix ,lr-item-suffix-rest ,lr-item-look-ahead) lr-new-item))))))

    ;; (c) Repeat step (2b) until no more new items can be added to V(X1,...,Xi)
    (let ((added-new t))
      (while added-new
        (setq added-new nil)
        (dolist (lr-item lr-new-item)
          (let ((lr-item-suffix (nth 2 lr-item)))
            (let ((lr-item-suffix-first (car lr-item-suffix))
                  (lr-item-suffix-rest (cdr lr-item-suffix)))

              ;; (b) If [A -> a . Bb, u] has been placed in V(X1,...,Xi)
              ;; and B -> D is in P
              (when (parser--valid-non-terminal-p lr-item-suffix-first)

                (let ((lr-item-suffix-rest-first (parser--first lr-item-suffix-rest)))
                  (unless lr-item-suffix-rest-first
                    (setq lr-item-suffix-rest-first (list nil)))
                  (let ((sub-production (parser--get-grammar-rhs lr-item-suffix-first)))

                    ;; For each production with B as LHS
                    (dolist (sub-rhs sub-production)

                      ;; Transform e-productions into nil
                      (when (and
                             (= (length sub-rhs) 1)
                             (parser--valid-e-p (car sub-rhs)))
                        (setq sub-rhs nil))

                      ;; For each x in FIRST(αu)
                      (dolist (f lr-item-suffix-rest-first)

                        ;; then add [B -> . D, x] to V(X1,...,Xi) for each x in FIRST(bu)
                        ;; provided it is not already there
                        (let ((lr-item-to-add `(,lr-item-suffix-first nil ,sub-rhs ,f)))
                          (unless (gethash lr-item-to-add lr-item-exists)
                            (setq added-new t)
                            (parser--debug (message "lr-item-to-add: %s" lr-item-to-add))
                            (puthash lr-item-to-add t lr-item-exists)
                            (push lr-item-to-add lr-new-item)))))))))))))

    (setq lr-new-item (sort lr-new-item 'parser--sort-list))
    lr-new-item))

;; Algorithm 5.7, p. 375
;; TODO Add support for lex-analyzer
;; TODO Add support for SDT
;; TODO Add support for semantic-actions
;; TODO Create hash-tables of parse-state -> action-table, parse-state -> goto-table
;; TODO Create hash-table of production-number -> production
(defun parser-lr--parse (input-tape &optional input-tape-index pushdown-list)
  "Perform a LR-parse of INPUT-TAPE optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST."
  (unless input-tape-index
    (setq input-tape-index 0))
  (unless pushdown-list
    (push 0 pushdown-list))

  (let ((accept nil)
        (input-tape-length (length input-tape))
        (output)
        (goto-tables (parser-lr--generate-goto-tables)))
    (let ((action-tables (parser-lr--generate-action-tables)))
      (while (and
              (not accept)
              (<= input-tape-index input-tape-length))

        ;; (1) The lookahead string u, consisting of the next k input symbols, is determined.
        (let ((look-ahead)
              (look-ahead-length 0)
              (look-ahead-input-tape-index input-tape-index))

          (while (and
                  (< look-ahead-input-tape-index input-tape-length)
                  (< look-ahead-length parser--look-ahead-number))
            (push (nth look-ahead-input-tape-index input-tape) look-ahead)
            (setq look-ahead-length (1+ look-ahead-length))
            (setq look-ahead-input-tape-index (1+ look-ahead-input-tape-index)))

          ;; If we reached end of input-tape and look-ahead is too small, append e-identifiers
          (while (< look-ahead-length parser--look-ahead-number)
            (push parser--e-identifier look-ahead)
            (setq look-ahead-length (1+ look-ahead-length)))

          (setq look-ahead (nreverse look-ahead))

          (let ((table-index (car pushdown-list)))
            (let ((action-table (car (cdr (nth table-index action-tables)))))

              (let ((action-match nil)
                    (action-table-length (length action-table))
                    (action-index 0)
                    (possible-look-aheads))

                ;; (2) The parsing action f of the table on top of the pushdown list is applied to the lookahead string u.
                (while (and
                        (not action-match)
                        (< action-index action-table-length))
                  (let ((action (nth action-index action-table)))
                    (let ((action-look-ahead (car action)))
                      (push action-look-ahead possible-look-aheads)
                      (when (equal action-look-ahead look-ahead)
                        (setq action-match (cdr action)))))
                  (setq action-index (1+ action-index)))

                (unless action-match
                  ;; (c) If f(u) = error, we halt parsing (and, in practice
                  ;; transfer to an error recovery routine).

                  (error (format
                          "Invalid syntax! Expected one of %s found %s at input-tape-index %s"
                          possible-look-aheads
                          look-ahead
                          input-tape-index)))

                (cond

                 ((equal action-match '(shift))
                  ;; (a) If f(u) = shift, then the next input symbol, say a
                  ;; is removed from the input and shifted onto the pushdown list.
                  ;; The goto function g of the table on top of the pushdown list
                  ;; is applied to a to determine the new table to be placed on
                  ;; top of the pushdown list. We then return to step(1). If
                  ;; there is no next input symbol or g(a) is undefined, halt
                  ;; and declare error.

                  (let ((a (nth input-tape-index input-tape)))
                    (let ((goto-table (car (cdr (nth table-index goto-tables)))))
                      (let ((goto-table-length (length goto-table))
                            (goto-index 0)
                            (searching-match t)
                            (next-index))

                        (while (and
                                searching-match
                                (< goto-index goto-table-length))
                          (let ((goto-item (nth goto-index goto-table)))
                            (let ((goto-item-look-ahead (car goto-item))
                                  (goto-item-next-index (car (cdr goto-item))))

                              (when (equal goto-item-look-ahead a)
                                (setq next-index goto-item-next-index)
                                (setq searching-match nil))))

                          (setq goto-index (1+ goto-index)))

                        (unless next-index
                          (error (format
                                  "In shift, found no goto-item for %s in index %s"
                                  a
                                  table-index)))

                        (push a pushdown-list)
                        (push next-index pushdown-list)
                        (setq input-tape-index (1+ input-tape-index))))))

                 ((equal (car action-match) 'reduce)
                  ;; (b) If f(u) = reduce i and production i is A -> a,
                  ;; then 2|a| symbols are removed from the top of the pushdown
                  ;; list, and production number i is placed in the output
                  ;; buffer. A new table T' is then exposed as the top table
                  ;; of the pushdown list, and the goto function of T' is applied
                  ;; to A to determine the next table to be placed on top of the
                  ;; pushdown list. We place A and this new table on top of the
                  ;; the pushdown list and return to step (1)

                  (let ((production-number (car (cdr action-match))))
                    (let ((production (parser--get-grammar-production-by-number production-number)))
                      (let ((production-lhs (car production))
                            (production-rhs (car (cdr production))))
                        (unless (equal production-rhs (list parser--e-identifier))
                          (let ((pop-items (* 2 (length production-rhs)))
                                (popped-items 0))
                            (while (< popped-items pop-items)
                              (pop pushdown-list)
                              (setq popped-items (1+ popped-items)))))
                        (push production-number output)

                        (let ((new-table-index (car pushdown-list)))
                          (let ((goto-table (car (cdr (nth new-table-index goto-tables)))))
                            (let ((goto-table-length (length goto-table))
                                  (goto-index 0)
                                  (searching-match t)
                                  (next-index))

                              (while (and
                                      searching-match
                                      (< goto-index goto-table-length))
                                (let ((goto-item (nth goto-index goto-table)))
                                  (let ((goto-item-look-ahead (car goto-item))
                                        (goto-item-next-index (car (cdr goto-item))))

                                    (when (equal goto-item-look-ahead production-lhs)
                                      (setq next-index goto-item-next-index)
                                      (setq searching-match nil))))

                                (setq goto-index (1+ goto-index)))

                              (when next-index
                                (push production-lhs pushdown-list)
                                (push next-index pushdown-list)))))))))

                 ((equal action-match '(accept))
                  ;;    (d) If f(u) = accept, we halt and declare the string
                  ;;    in the output buffer to be the right parse of the original
                  ;;    input string.

                  (setq accept t))

                 (t (error (format "Invalid action-match: %s!" action-match))))))))))
    (nreverse output)))

(provide 'parser-lr)

;;; parser-lr.el ends here

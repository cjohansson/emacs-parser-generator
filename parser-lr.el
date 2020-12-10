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
    (let ((action-tables nil)
          (terminals (parser--get-grammar-terminals)))
      (dolist (goto-table parser-lr--goto-tables)
        ;; (message "goto-table: %s" goto-table)
        (let ((goto-index (car goto-table))
              (gotos (car (cdr goto-table))))
          (let ((lr-items (gethash goto-index parser-lr--items)))
            (dolist (lr-item lr-items)
              ;; TODO Iterate all possible 
              ;; TODO (a) f(u) = shift if [A -> B . C, v] is in LR-items, C != e and u is in EFF(Cv)
              ;; TODO (b) f(u) = reduce i if [A -> B ., u] is in a and A -> B is product i in P, i > 1
              ;; TODO (c) f(e) = accept if [S' -> S ., e] is in a
              ;; TODO (d) f(u) = error otherwise
              ))))
      (setq parser-lr--action-table action-tables))))

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
         (parser--hash-values-to-list parser-lr--items t))
      (error "Inconsistent grammar!"))))

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
        (unless a-look-ahead
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


(provide 'parser-lr)

;;; parser-lr.el ends here

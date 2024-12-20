(library (cursor core)
         (export empty
                 fail
                 any
                 char
                 sequence
                 choice
                 maybe
                 repeat
                 repeat+1
                 is?
                 is-not?
                 one-of
                 none-of
                 call
                 grammar
                 capture
                 text)
         (import (rnrs)
                 (cursor data)
                 (cursor tools)
                 (cursor collections charset))

         ;; === Error Messages ===

         (define ERROR-TYPE-CHARACTER "not a character")
         (define ERROR-TYPE-SYMBOL    "not a symbol")
         (define ERROR-TYPE-STRING    "not a string")
         (define ERROR-TYPE-PEG       "not PEG expression")
         (define ERROR-TYPE-CODE      "undefined operation")
         (define ERROR-UNDEFINED-RULE "undefined rule in grammar")
         (define ERROR-NULLABLE       "expression within may cause infinite loop")
         (define ERROR-LEFT-RECURSION "rule may be left recursive")

         ;; === Helper Functions ===

         (define check-ast
           (lambda (x)
             (if (ast? x)
                 x
                 (raise (make-peg-error "undefined" x ERROR-TYPE-CODE)))))

         (define fold-ast
           (lambda (xs)
             (cond [(null? xs) xs]
                   [(let ([x (car xs)])
                      (and (ast? x)
                           (let ([type (ast-type x)])
                             (or (eq? type SEQUENCE)
                                 (eq? type CHOICE)))))
                    (append (ast-node-x x) (fold-ast (cdr xs)))]
                   [else (cons (check-ast (car xs))
                               (fold-ast (cdr xs)))])))

         (define nullable?
           (lambda (x)
             (let recur ([x x] [grammar '()])
               (letrec ([check-sequence (lambda (xs)
                                          (cond [(null? (cdr xs)) (recur (car xs) grammar)]
                                                [(recur (car xs) grammar) (check-sequence (cdr xs))]
                                                [else #f]))]
                        [check-choice (lambda (xs)
                                        (cond [(null? xs) #f]
                                              [(recur (cdr xs)) #t]
                                              [else (recur (car xs) grammar)]))])
               (let ([type   (ast-type x)]
                     [node-x (ast-node-x x)]
                     [node-y (ast-node-y x)])
                 (case type
                   [(FAIL ANY CHARACTER ONE-OF NONE-OF OPEN-CALL) #f]
                   [(EMPTY REPEAT IS IS-NOT) #t]
                   [(SEQUENCE)     (check-sequence node-x)]
                   [(CHOICE)       (check-choice node-x)]
                   [(GRAMMAR)      (recur (vector-ref node-x 0) x)]
                   [(CALL)         (recur (vector-ref (ast-node-x grammar) node-y) grammar)]
                   [(RULE CAPTURE) (recur node-y grammar)]
                   [else #f]))))))

         (define check-grammar
           (lambda (xs)
             (let ([nodes      xs]
                   [size       (vector-length xs)]
                   [rule-count (make-eqv-hashtable)]
                   [step-count 0]
                   [max-count  1000]
                   [error-flag #f])
               (let ([check-rule (lambda (start)
                                   (letrec ([traverse-rules
                                             (lambda (index nullable-flag)
                                               (cond [(or error-flag nullable-flag (>= index size)) #f]
                                                     [else (traverse-node (vector-ref nodes index) nullable-flag)]))]
                                            [traverse-sequence
                                             (lambda (xs nullable-flag)
                                               (cond [error-flag #f]
                                                     [(null? (cdr xs)) (traverse-node (car xs) nullable-flag)]
                                                     [(traverse-node (car xs) nullable-flag)
                                                      (traverse-sequence (cdr xs) nullable-flag)]
                                                     [else #f]))]
                                            [traverse-choice
                                             (lambda (xs nullable-flag)
                                               (cond [error-flag #f]
                                                     [(null? (cdr xs)) (traverse-node (car xs) nullable-flag)]
                                                     [else
                                                      (let ([nullable-flag (traverse-node (car xs) nullable-flag)])
                                                        (if error-flag
                                                            #f
                                                            (traverse-choice (cdr xs) nullable-flag)))]))]
                                            [traverse-node
                                             (lambda (x nullable-flag)
                                               (cond [error-flag #f]
                                                     [else
                                                      (let ([type   (ast-type x)]
                                                            [node-x (ast-node-x x)]
                                                            [node-y (ast-node-y x)])
                                                        (case type
                                                          [(RULE)
                                                           (set! step-count (+ step-count 1))
                                                           (cond [error-flag #f]
                                                                 [(> step-count max-count) (set! error-flag #t) #f]
                                                                 [else
                                                                  (let ([total (hashtable-ref rule-count node-x #f)])
                                                                    (if total
                                                                        (begin (hashtable-set! rule-count node-x (+ total 1))
                                                                               (traverse-node node-y nullable-flag))
                                                                        (begin (hashtable-set! rule-count node-x 1)
                                                                               (traverse-node node-y nullable-flag))))])]
                                                          [(EMPTY) #t]
                                                          [(FAIL ANY CHARACTER ONE-OF NONE-OF) nullable-flag]
                                                          [(SEQUENCE) (traverse-sequence node-x #f)]
                                                          [(CHOICE) (traverse-choice node-x nullable-flag)]
                                                          [(CALL) (traverse-rules node-y nullable-flag)]
                                                          [(REPEAT IS IS-NOT) (traverse-node node-x #t)]
                                                          [(CAPTURE) (traverse-node node-y nullable-flag)]
                                                          [else #f]))]))])
                                     (traverse-rules start #f)))]
                     [find-rule  (lambda ()
                                   (let ([rules (hashtable-keys rule-count)])
                                     (vector-fold (lambda (rule-x rule-y)
                                                    (let ([count-x (hashtable-ref rule-count rule-x 0)]
                                                          [count-y (hashtable-ref rule-count rule-y 0)])
                                                      (if (> count-x count-y)
                                                          rule-x
                                                          rule-y)))
                                                  rules)))])
                 (let loop ([index 0])
                   (cond [error-flag (raise (make-peg-error "(grammar _)" (find-rule) ERROR-LEFT-RECURSION))]
                         [(< index size)
                          (check-rule index)
                          (loop (+ index 1))]
                         [else nodes]))))))


         ;; === Terminals ===

         ;; empty = ε
         ;;
         ;; (ast EMPTY '() '())
         
         (define empty (encode-ast EMPTY))

         ;; fail
         ;;
         ;; (ast FAIL '() '())
         
         (define fail (encode-ast FAIL))

         ;; any = .
         ;;
         ;; (ast ANY '() '())
         
         (define any (encode-ast ANY))

         ;; (char #\a) = "a"
         ;;
         ;; (ast CHARACTER #\a '())
         
         (define char
           (lambda (x)
             (if (char? x)
                 (encode-ast CHARACTER x)
                 (raise (make-peg-error "(char _)" x ERROR-TYPE-CHARACTER)))))

         ;; === Concatenation ===

         ;; (sequence px py ...) = px • py • ...
         ;; (sequence px)        = px
         ;; (sequence)           = ε
         ;;
         ;; px py ... -> (ast SEQUENCE (list px py ...) '())
         ;; px        -> px
         ;;           -> empty
         
         (define sequence
           (case-lambda
             [()  empty]
             [(x) (check-ast x)]
             [xs  (encode-ast SEQUENCE (fold-ast xs))]))

         ;; === Ordered Choice: Limited Backtracking ===

         ;; (choice px py ...) = px / py / ...
         ;; (choice px)        = px
         ;; (choice)           = fail
         ;;
         ;; px py ... -> (ast CHOICE (list px py ...) '())
         ;; px        -> px
         ;;           -> fail
         
         (define choice
           (case-lambda
             [()  fail]
             [(x) (check-ast x)]
             [xs  (encode-ast CHOICE (fold-ast xs))]))

         ;; (maybe px) = px? = px / ε
         ;;
         ;; (ast CHOICE (list px empty) '())
         
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; === Repetition ===

         ;; (repeat px) = px*
         ;;
         ;; (ast REPEAT px '())
         
         (define repeat
           (lambda (px)
             (let ([pattern (check-ast px)])
               (if (not (nullable? pattern))
                   (encode-ast REPEAT pattern)
                   (raise (make-peg-error "(repeat _)"
                                          "possibly empty, (is? _), (is-not? _), or (repeat _)"
                                          ERROR-NULLABLE))))))

         ;; (repeat+1 px) = px+
         ;; (repeat+1 px) = px • px*
         ;;
         ;; (ast SEQUENCE (list px (ast REPEAT px '())) '())
         
         (define repeat+1
           (lambda (px)
             (sequence px (repeat px))))

         ;; === Syntactic Predicates: Unlimited Lookahead ===

         ;; (is? px) = &px
         ;;
         ;; (ast IS px '())
         
         (define is?
           (lambda (px)
             (encode-ast IS (check-ast px))))

         ;; (is-not? px) = !px
         ;;
         ;; (ast IS-NOT px '())
         
         (define is-not?
           (lambda (px)
             (encode-ast IS-NOT (check-ast px))))

         ;; (one-of "abc") = [abc]
         ;; (one-of "")    = ∅
         ;;   where ∅ = the empty set
         ;;
         ;; "abc" -> (ast ONE-OF (charset "abc") '())
         ;; ""    -> fail
         
         (define one-of
           (lambda (xs)
             (cond [(string? xs)
                    (if (string=? xs "")
                        fail
                        (encode-ast ONE-OF (make-charset xs)))]
                   [else (raise (make-peg-error "(one-of _)"
                                                xs
                                                ERROR-TYPE-STRING))])))

         ;; (none-of "abc") = [^abc]
         ;; (none-of "")    = U
         ;;   where U = the universal set
         ;;
         ;; "abc" -> (ast NONE-OF (charset "abc") '())
         ;; ""    -> any
         
         (define none-of
           (lambda (xs)
             (cond [(string? xs)
                    (if (string=? xs "")
                        any
                        (encode-ast NONE-OF (make-charset xs)))]
                   [else (raise (make-peg-error "(none-of _)"
                                                xs
                                                ERROR-TYPE-STRING))])))

         ;; === Grammar ===

         ;; (call x)
         ;;   where x = symbol
         ;;
         ;; x -> (ast OPEN-CALL x)
         
         (define-syntax call
           (syntax-rules ()
             [(_ x)
              (let ([id (quote x)])
                (if (symbol? id)
                    (encode-ast OPEN-CALL id)
                    (raise (make-peg-error "(call _)" id ERROR-TYPE-SYMBOL))))]))

         ;; (grammar [id pattern] ...) = id <- pattern
         ;;                              ...
         ;;
         ;; (ast GRAMMAR (vector (ast RULE id pattern) ...))
         
         (define-syntax grammar
           (syntax-rules ()
             [(_ [rule-x body-x] [rule-y body-y] ...)
              (let ([rule-x (encode-ast RULE (quote rule-x) (check-ast body-x))]
                    [rule-y (encode-ast RULE (quote rule-y) (check-ast body-y))]
                    ...)
                (let* ([symbols    (quote (rule-x rule-y ...))]
                       [offsets    (zip symbols (iota (length symbols)))]
                       ;; Collect grammar rules.
                       [open-rules (vector rule-x rule-y ...)]
                       ;; Close open calls.
                       [closed-rules
                        (vector-map (lambda (rule)
                                      (encode-ast RULE
                                                  (ast-node-x rule)
                                                  (let recur ([node (ast-node-y rule)])
                                                    (let ([type (ast-type node)])
                                                      (case type
                                                        ;; terminals
                                                        [(EMPTY FAIL ANY CHARACTER CALL) rule]
                                                        ;; sequences
                                                        [(SEQUENCE CHOICE)
                                                         (encode-ast type (map recur (ast-node-x node)))]
                                                        ;; non-terminals
                                                        [(REPEAT IS IS-NOT ONE-OF NONE-OF)
                                                         (encode-ast type (recur (ast-node-x node)))]
                                                        [(CAPTURE)
                                                         (encode-ast type (ast-node-x node) (recur (ast-node-y node)))]
                                                        ;; skip
                                                        [(GRAMMAR) rule]
                                                        ;; open call -> call
                                                        [(OPEN-CALL)
                                                         (let ([offset (assq (ast-node-x node) offsets)])
                                                           (if offset
                                                               (encode-ast CALL (car offset) (cdr offset))
                                                               (raise (make-peg-error "(grammar _)" (ast-node-x node) ERROR-UNDEFINED-RULE))))]
                                                        ;; wildcard
                                                        [else rule])))))
                                    open-rules)])
                  ;; Check grammar for possible left recursion.
                  (encode-ast GRAMMAR (check-grammar closed-rules))))]))

         ;; (capture fn px)
         ;;   where fn = function
         ;;         px = instruction-list
         
         (define capture
           (case-lambda
             [(px) (capture '() px)]
             [(fn px)
              (encode-ast CAPTURE (if (procedure? fn) fn '()) px)]))

         ;; (text "abc") = a • b • c
         ;; (text "")    = ε
         
         (define text
           (lambda (xs)
             (cond [(string? xs)
                    (let* ([characters (map (lambda (x) (encode-ast CHARACTER x)) (string->list xs))]
                           [size       (length characters)])
                      (cond [(< size 1) empty]
                            [(= size 1) (car characters)]
                            [else       (encode-ast SEQUENCE characters)]))]
                   [else (raise (make-peg-error "(text _)" xs ERROR-TYPE-STRING))])))

         )

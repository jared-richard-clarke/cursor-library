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
                 text
                 (rename (unit-tests core:unit-tests)))
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

         ;; (check-ast ast) -> ast | raise peg-error
         ;; Returns the given argument if it is an AST. Otherwise,
         ;; throws an error.

         (define check-ast
           (lambda (x)
             (if (ast? x)
                 x
                 (raise (make-peg-error "undefined" x ERROR-TYPE-CODE)))))

         ;; (flatten-ast symbol (list ast)) -> (list ast)
         ;; Flattens a nested list of ASTs of the given type by one level.

         (define flatten-ast
           (lambda (type xs)
             (cond [(null? xs) xs]
                   [(let ([x (car xs)])
                      (and (ast? x)
                           (eq? type (ast-type x))))
                    (append (ast-node-x (car xs)) (flatten-ast type (cdr xs)))]
                   [else (cons (check-ast (car xs))
                               (flatten-ast type (cdr xs)))])))

         ;; (nullable? ast) -> boolean
         ;; Checks if an AST might trigger an infinite loop.
         ;; This is an approximation.

         (define nullable?
           (lambda (x)
             (let recur ([x x] [grammar '()])
               (letrec ([check-sequence (lambda (xs grammar)
                                          (cond [(null? (cdr xs))
                                                 (recur (car xs) grammar)]
                                                [(recur (car xs) grammar)
                                                 (check-sequence (cdr xs) grammar)]
                                                [else #f]))]
                        [check-choice (lambda (xs grammar)
                                        (cond [(null? (cdr xs))
                                               (recur (car xs) grammar)]
                                              [(check-choice (cdr xs) grammar)
                                               (recur (car xs) grammar)]
                                              [else #f]))])
                 (let ([type   (ast-type x)]
                       [node-x (ast-node-x x)]
                       [node-y (ast-node-y x)])
                   (case type
                     [(FAIL ANY CHARACTER ONE-OF NONE-OF OPEN-CALL) #f]
                     [(EMPTY REPEAT IS IS-NOT) #t]
                     [(SEQUENCE)
                      (check-sequence node-x grammar)]
                     [(CHOICE)
                      (check-choice node-x grammar)]
                     [(GRAMMAR)
                      (recur (vector-ref node-x 0) x)]
                     [(CALL)
                      (recur (vector-ref (ast-node-x grammar) node-y) grammar)]
                     [(RULE CAPTURE)
                      (recur node-y grammar)]
                     [else #f]))))))

         ;; (check-grammar (vector ast)) -> (vector ast) | raise peg-error
         ;; Checks if a list of grammar rules is potentially left-recursive
         ;; by running a simulation on that list. This is an approximation.
         ;;
         ;; Raises an error on potential left recursion. Otherwise, returns
         ;; list unchanged.
         ;;
         ;; Side Note: The rules list is a vector for O(1) lookup.
         
         (define check-grammar
           (lambda (xs)
                   ;; The rules list being simulated over.
             (let ([nodes xs]
                   ;; The total number of rules. Provides an upper bound for
                   ;; looping over the rules list.
                   [size (vector-length xs)]
                   ;; Maps the number of calls per rule. If the grammar is
                   ;; left-recursive, the rule with the highest number of
                   ;; calls is most likely left-recursive.
                   [rule-count (make-eqv-hashtable)]
                   ;; Tracks the total number of rule calls.
                   [call-count 0]
                   ;; If a set of rules has accumulatively been called more than a 1000 times,
                   ;; then it is probably left-recursive. This limit is arbitrary.
                   [max-count 1000]
                   ;; Error flag allows a left-recursive simulation to terminate.
                   [error-flag #f])
               ;; Runs simulation over rule, checking for left recursion.
               (let ([check-rule (lambda (start)
                                            ;; Boundary and error check before indexing into the next rule.
                                   (letrec ([traverse-rules
                                             (lambda (index nullable-flag)
                                               (cond [(or error-flag (>= index size)) #f]
                                                     [else (traverse-node (vector-ref nodes index) nullable-flag)]))]
                                            ;; Traverses sequence pairwise, checking the subsequent node only if
                                            ;; the previous node is nullable and the error flag is set to false.
                                            [traverse-sequence
                                             (lambda (xs nullable-flag)
                                               (cond [error-flag #f]
                                                     [(null? (cdr xs))
                                                      (traverse-node (car xs) nullable-flag)]
                                                     [(traverse-node (car xs) nullable-flag)
                                                      (traverse-sequence (cdr xs) nullable-flag)]
                                                     [else #f]))]
                                            ;; Traverses choice pairwise. Each node must be checked unless
                                            ;; the error flag is set to true.
                                            [traverse-choice
                                             (lambda (xs nullable-flag)
                                               (cond [error-flag #f]
                                                     [(null? (cdr xs))
                                                      (traverse-node (car xs) nullable-flag)]
                                                     [else
                                                      (let ([nullable-flag (traverse-node (car xs) nullable-flag)])
                                                        (if error-flag
                                                            #f
                                                            (traverse-choice (cdr xs) nullable-flag)))]))]
                                            ;; Delegates the traversal of a node according to its type.
                                            [traverse-node
                                             (lambda (x nullable-flag)
                                               (cond [error-flag #f]
                                                     [else
                                                      (let ([type   (ast-type x)]
                                                            [node-x (ast-node-x x)]
                                                            [node-y (ast-node-y x)])
                                                        (case type
                                                          [(RULE)
                                                           (set! call-count (+ call-count 1))
                                                           (cond [error-flag #f]
                                                                 [(> call-count max-count)
                                                                  (set! error-flag #t)
                                                                  #f]
                                                                 [else
                                                                  (let ([total (hashtable-ref rule-count node-x #f)])
                                                                    (if total
                                                                        (begin (hashtable-set! rule-count node-x (+ total 1))
                                                                               (traverse-node node-y nullable-flag))
                                                                        (begin (hashtable-set! rule-count node-x 1)
                                                                               (traverse-node node-y nullable-flag))))])]
                                                          [(EMPTY) #t]
                                                          [(FAIL ANY CHARACTER ONE-OF NONE-OF)
                                                           nullable-flag]
                                                          [(SEQUENCE)
                                                           (traverse-sequence node-x #f)]
                                                          [(CHOICE)
                                                           (traverse-choice node-x nullable-flag)]
                                                          [(CALL)
                                                           (traverse-rules node-y nullable-flag)]
                                                          [(REPEAT IS IS-NOT)
                                                           (traverse-node node-x #t)]
                                                          [(CAPTURE)
                                                           (traverse-node node-y nullable-flag)]
                                                          [else #f]))]))])
                                     (traverse-rules start #f)))]
                     ;; Look up rule with the highest call count. This rule is most likely
                     ;; responsible for left recursion.
                     [find-rule  (lambda ()
                                   (let ([rules (hashtable-keys rule-count)])
                                     (vector-fold (lambda (rule-x rule-y)
                                                    (let ([count-x (hashtable-ref rule-count rule-x 0)]
                                                          [count-y (hashtable-ref rule-count rule-y 0)])
                                                      (if (> count-x count-y)
                                                          rule-x
                                                          rule-y)))
                                                  rules)))])
                 ;; Loop over and check each rule in grammar. Raise error if error flag is set to true.
                 (let loop ([index 0])
                   (cond [error-flag (raise (make-peg-error "(grammar _)" (find-rule) ERROR-LEFT-RECURSION))]
                         [(< index size)
                          (check-rule index)
                          (loop (+ index 1))]
                         [else nodes]))))))

         ;; === Terminals ===

         ;; empty = ε
         ;; empty -> (ast EMPTY)
         
         (define empty (encode-ast EMPTY))

         ;; fail
         ;; fail -> (ast FAIL)
         
         (define fail (encode-ast FAIL))

         ;; any = .
         ;; any -> (ast ANY)
         
         (define any (encode-ast ANY))

         ;; (char #\a) = "a"
         ;; (char #\a) -> (ast CHARACTER #\a)
         
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
         ;; (sequence px py ...) -> (ast SEQUENCE (list px py ...))
         ;; (sequence px)        -> px
         ;; (sequence)           -> empty
         
         (define sequence
           (case-lambda
             [()  empty]
             [(x) (check-ast x)]
             [xs  (encode-ast SEQUENCE (flatten-ast SEQUENCE xs))]))

         ;; === Ordered Choice: Limited Backtracking ===

         ;; (choice px py ...) = px / py / ...
         ;; (choice px)        = px
         ;; (choice)           = fail
         ;;
         ;; (choice px py ...) -> (ast CHOICE (list px py ...))
         ;; (choice px)        -> px
         ;; (choice)           -> fail

         ;; === Ordered Choice: Sets ===
         
         ;; Rules for consolidating sets within a choice operation.
         ;;
         ;; +-----------------------------------------------> y
         ;; | +-----------------------------------------------+
         ;; | |         | one-of           | none-of          |
         ;; | |---------+------------------+------------------|
         ;; | | one-of  | x ∪ y -> one-of  | y \ x -> none-of |
         ;; | |---------+------------------+------------------|
         ;; V | none-of | x \ y -> none-of | x ∪ y -> none-of |
         ;; x +-----------------------------------------------+
         
         (define choice
           (case-lambda
             [()  fail]
             [(x) (check-ast x)]
             [xs  (encode-ast CHOICE (flatten-ast CHOICE xs))]))

         ;; (maybe px) = px?
         ;;            = px / ε
         ;; (maybe px) -> (ast CHOICE (list px empty))
         
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; === Repetition ===

         ;; (repeat px) = px*
         ;; (repeat px) -> (ast REPEAT px)
         
         (define repeat
           (lambda (px)
             (let ([pattern (check-ast px)])
               (if (not (nullable? pattern))
                   (encode-ast REPEAT pattern)
                   (raise (make-peg-error "(repeat _)"
                                          "possibly empty, (is? _), (is-not? _), or (repeat _)"
                                          ERROR-NULLABLE))))))

         ;; (repeat+1 px) = px+
         ;;               = px • px*
         ;; (repeat+1 px) -> (ast SEQUENCE (list px (repeat px)))
         
         (define repeat+1
           (lambda (px)
             (sequence px (repeat px))))

         ;; === Syntactic Predicates: Unlimited Lookahead ===

         ;; (is? px) = &px
         ;; (is? px) -> (ast IS px)
         
         (define is?
           (lambda (px)
             (encode-ast IS (check-ast px))))

         ;; (is-not? px) = !px
         ;; (is-not? px) -> (ast IS-NOT px)
         
         (define is-not?
           (lambda (px)
             (encode-ast IS-NOT (check-ast px))))

         ;; (one-of "abc") = [abc]
         ;; (one-of "")    = ∅
         ;;   where ∅ = the empty set
         ;;
         ;; (one-of "abc") -> (ast ONE-OF (charset "abc"))
         ;; (one-of "")    -> fail
         
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
         ;; (none-of "abc") -> (ast NONE-OF (charset "abc"))
         ;; (none-of "")    -> any
         ;;
         ;; Operation returns the universal set minus the provided characters.
         ;; In this context, the universal set contains all characters
         ;; as provided by R6RS — particularly Chez Scheme.
         
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
         ;; (call x)  -> (ast OPEN-CALL x)
         
         (define-syntax call
           (syntax-rules ()
             [(_ x)
              (let ([id (quote x)])
                (if (symbol? id)
                    (encode-ast OPEN-CALL id)
                    (raise (make-peg-error "(call _)" id ERROR-TYPE-SYMBOL))))]))

         ;; (grammar [id pattern] ...) = id <- pattern
         ;;                              ...
         ;; (grammar [id pattern] ...) -> (ast GRAMMAR (vector (ast RULE id pattern) ...))
         
         (define-syntax grammar
           (syntax-rules ()
             [(_ [rule-x body-x]
                 [rule-y body-y]
                 ...)
              (let ([rule-x (encode-ast RULE (quote rule-x) (check-ast body-x))]
                    [rule-y (encode-ast RULE (quote rule-y) (check-ast body-y))]
                    ...)
                (let* ([symbols    (quote (rule-x rule-y ...))]
                       [offsets    (zip symbols (iota (length symbols)))]
                       ;; Collect rules.
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
                                                        [(EMPTY FAIL ANY CHARACTER CALL ONE-OF NONE-OF) node]
                                                        ;; sequences
                                                        [(SEQUENCE CHOICE)
                                                         (encode-ast type (map recur (ast-node-x node)))]
                                                        ;; non-terminals
                                                        [(REPEAT IS IS-NOT)
                                                         (encode-ast type (recur (ast-node-x node)))]
                                                        [(CAPTURE)
                                                         (encode-ast type (ast-node-x node) (recur (ast-node-y node)))]
                                                        ;; skip
                                                        [(GRAMMAR) node]
                                                        ;; open call -> call
                                                        [(OPEN-CALL)
                                                         (let ([offset (assq (ast-node-x node) offsets)])
                                                           (if offset
                                                               (encode-ast CALL (car offset) (cdr offset))
                                                               (raise (make-peg-error "(grammar _)" (ast-node-x node) ERROR-UNDEFINED-RULE))))]
                                                        ;; wildcard
                                                        [else node])))))
                                    open-rules)])
                  ;; Check for possible left recursion.
                  (encode-ast GRAMMAR (check-grammar closed-rules))))]))

         ;; (capture fn px)
         ;;   where fn = function
         ;;         px = pattern
         ;;
         ;; (capture fn px) -> (ast CAPTURE fn px)
         
         (define capture
           (case-lambda
             [(px) (capture '() px)]
             [(fn px)
              (encode-ast CAPTURE (if (procedure? fn) fn '()) px)]))

         ;; (text "abc") = a • b • c
         ;; (text "")    = ε
         ;; (text "abc") -> (ast SEQUENCE (list (char #\a) (char #\b) (char #\c)))
         ;; (text "")    -> empty
         
         (define text
           (lambda (xs)
             (cond [(string? xs)
                    (let* ([characters (map (lambda (x) (encode-ast CHARACTER x)) (string->list xs))]
                           [size       (length characters)])
                      (cond [(< size 1) empty]
                            [(= size 1) (car characters)]
                            [else       (encode-ast SEQUENCE characters)]))]
                   [else (raise (make-peg-error "(text _)" xs ERROR-TYPE-STRING))])))

         ;; === Unit Tests ===

         (define unit-tests
           (let ([A (encode-ast CHARACTER #\a)]
                 [B (encode-ast CHARACTER #\b)]
                 [C (encode-ast CHARACTER #\c)])
             
             (test-chunk
              "Cursor Core"
              ;; === Literals ===
              (test-assert "character literal"
                           ast-equal?
                           (char #\a)
                           A)
              
              ;; === Concatenation ===
              (test-assert "text sequence abc"
                           ast-equal?
                           (text "abc")
                           (encode-ast SEQUENCE (list A B C)))

              (test-assert "text epsilon"
                           ast-equal?
                           (text "")
                           empty)

              (test-assert "sequence abc"
                           ast-equal?
                           (sequence (char #\a)
                                     (char #\b)
                                     (char #\c))
                           (encode-ast SEQUENCE (list A B C)))

              (test-assert "sequence nested"
                           ast-equal?
                           (sequence (sequence (char #\a)
                                               (char #\b))
                                     (char #\c)
                                     (text "ba"))
                           (encode-ast SEQUENCE (list A B C B A)))

              (test-assert "sequence identity"
                           ast-equal?
                           (sequence)
                           empty)

              ;; === Ordered Choice ===
              (test-assert "choice, a / b"
                           ast-equal?
                           (choice (char #\a) (char #\b))
                           (encode-ast CHOICE (list A B)))

              (test-assert "choice identity"
                           ast-equal?
                           (choice)
                           fail)

              ;; === Repetition ===
              (test-assert "repeat a*"
                           ast-equal?
                           (repeat (char #\a))
                           (encode-ast REPEAT A))

              (test-assert "repeat a+"
                           ast-equal?
                           (repeat+1 (char #\a))
                           (encode-ast SEQUENCE (list A (encode-ast REPEAT A))))
              
              ;; === Not Predicate ===
              (test-assert "predicate !a"
                           assert-equal?
                           (is-not? (char #\a))
                           (encode-ast IS-NOT A))

              ;; === Predicate ===
              (test-assert "predicate &a"
                           assert-equal?
                           (is? (char #\a))
                           (encode-ast IS A))

              ;; === Sets ===
              (test-assert "character set [abc]"
                           ast-equal?
                           (one-of "abc")
                           (encode-ast ONE-OF (make-charset "abc")))

              (test-assert "character set [^abc]"
                           ast-equal?
                           (none-of "abc")
                           (encode-ast NONE-OF (make-charset "abc")))

              (test-assert "empty set"
                           ast-equal?
                           (one-of "")
                           fail)

              (test-assert "universal set"
                           ast-equal?
                           (none-of "")
                           any)

              ;; === Captures ===
              (test-assert "capture, baseline"
                           ast-equal?
                           (capture (char #\a))
                           (encode-ast CAPTURE '() A))

              (test-assert "capture, true positive"
                           ast-equal?
                           (capture identity (char #\a))
                           (encode-ast CAPTURE identity A))

              (test-assert "capture, false positive, + ≠ identity"
                           ast-equal?
                           (capture + (char #\a))
                           (encode-ast CAPTURE identity A))

              ;; === Grammars ===
              (test-assert "grammar, baseline"
                           ast-equal?
                           (grammar [R1 (sequence (text "ab") (call R2))]
                                    [R2 (text "c")])
                           (encode-ast GRAMMAR
                                       (encode-ast RULE
                                                   (quote R1)
                                                   (encode-ast SEQUENCE
                                                               (list A
                                                                     B
                                                                     (encode-ast CALL
                                                                                 (quote R2)
                                                                                 1))))
                                       (encode-ast RULE
                                                   (quote R2)
                                                   C)))
              
              )))
         
         )

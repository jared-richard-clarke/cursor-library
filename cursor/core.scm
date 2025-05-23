(library (cursor core)
         (export empty
                 fail
                 any
                 char
                 and-then
                 or-else
                 maybe
                 repeat
                 repeat+1
                 is?
                 is-not?
                 one-of
                 none-of
                 rule
                 grammar
                 capture
                 transform
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
         (define ERROR-TYPE-FUNCTION  "not a function")
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
                 (peg-error "(check-ast _)" ERROR-TYPE-CODE (list x)))))

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
                        ;; Traverses sequence, checking the subsequent node
                        ;; only if the previous node is nullable. Every node
                        ;; in a sequence can be nullable but the last.
               (letrec ([check-sequence (lambda (xs grammar)
                                          (cond [(null? (cdr xs))
                                                 (recur (car xs) grammar)]
                                                [(recur (car xs) grammar)
                                                 (check-sequence (cdr xs) grammar)]
                                                [else #f]))]
                        ;; Traverses choice, checking the previous node
                        ;; only if the subsequent node is not nullable.
                        ;; Every node in a choice must NOT be nullable.
                        [check-choice (lambda (xs grammar)
                                        (cond [(null? (cdr xs))
                                               (recur (car xs) grammar)]
                                              [(check-choice (cdr xs) grammar)
                                               #t]
                                              [else
                                               (recur (car xs) grammar)]))])
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
                     [(RULE CAPTURE TRANSFORM)
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
             (let ([nodes xs]
                   [size (vector-length xs)]
                   [rule-count (make-eqv-hashtable)]
                   [call-count 0]
                   ;; If a set of rules has accumulatively been called more than a 1000 times,
                   ;; then it is probably left-recursive. This limit is arbitrary.
                   [max-count 1000]
                   [error-flag #f])
               (let ([check-rule (lambda (start)
                                   (letrec ([traverse-rules
                                             (lambda (index nullable-flag)
                                               (cond [(or error-flag (>= index size)) #f]
                                                     [else (traverse-node (vector-ref nodes index) nullable-flag)]))]
                                            ;; Traverses sequence, checking the subsequent node only if the
                                            ;; previous node is nullable and the error flag is set to false.
                                            [traverse-sequence
                                             (lambda (xs nullable-flag)
                                               (cond [error-flag #f]
                                                     [(null? (cdr xs))
                                                      (traverse-node (car xs) nullable-flag)]
                                                     [(traverse-node (car xs) nullable-flag)
                                                      (traverse-sequence (cdr xs) nullable-flag)]
                                                     [else #f]))]
                                            ;; Traverses choice. Each node must be checked unless
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
                                                          [(CAPTURE TRANSFORM)
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
                   (cond [error-flag
                          (peg-error "(grammar _)" ERROR-LEFT-RECURSION (list (find-rule)))]
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
                 (peg-error "(char _)" ERROR-TYPE-CHARACTER (list x)))))

         ;; === Concatenation ===

         ;; (and-then px py ...) = px • py • ...
         ;; (and-then px)        = px
         ;; (and-then)           = ε
         ;;
         ;; (and-then px py ...) -> (ast SEQUENCE (list px py ...))
         ;; (and-then px)        -> px
         ;; (and-then)           -> empty
         (define and-then
           (case-lambda
             [()  empty]
             [(x) (check-ast x)]
             [xs  (encode-ast SEQUENCE (flatten-ast SEQUENCE xs))]))

         ;; === Ordered Choice: Limited Backtracking ===

         ;; (or-else px py ...) = px / py / ...
         ;; (or-else px)        = px
         ;; (or-else)           = fail
         ;;
         ;; (or-else px py ...) -> (ast CHOICE (list px py ...))
         ;; (or-else px)        -> px
         ;; (or-else)           -> fail
         (define or-else
           (case-lambda
            [()  fail]
            [(x) (check-ast x)]
            [xs  (encode-ast CHOICE (flatten-ast CHOICE xs))]))

         ;; (maybe px) = px?
         ;;            = px / ε
         ;; (maybe px) -> (ast CHOICE (list px empty))
         (define maybe
           (lambda (px)
             (or-else px empty)))

         ;; === Repetition ===

         ;; (repeat px) = px*
         ;; (repeat px) -> (ast REPEAT px)
         (define repeat
           (lambda (px)
             (let ([pattern (check-ast px)])
               (if (not (nullable? pattern))
                   (encode-ast REPEAT pattern)
                   (peg-error "(repeat _)" ERROR-NULLABLE '())))))

         ;; (repeat+1 px) = px+
         ;;               = px • px*
         ;; (repeat+1 px) -> (ast SEQUENCE (list px (repeat px)))
         (define repeat+1
           (lambda (px)
             (and-then px (repeat px))))

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
                   [else
                    (peg-error "(one-of _)" ERROR-TYPE-STRING (list xs))])))

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
                   [else
                    (peg-error "(none-of _)" ERROR-TYPE-STRING (list xs))])))

         ;; === Grammar ===

         ;; (rule x)
         ;;   where x = symbol
         ;;
         ;; (rule x)  -> (ast OPEN-CALL x)
         (define-syntax rule
           (syntax-rules ()
             [(_ x)
              (let ([id (quote x)])
                (if (symbol? id)
                    (encode-ast OPEN-CALL id)
                    (peg-error "(rule _)" ERROR-TYPE-SYMBOL (list id))))]))

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
                                                        [(CAPTURE TRANSFORM)
                                                         (encode-ast type (ast-node-x node) (recur (ast-node-y node)))]
                                                        ;; skip
                                                        [(GRAMMAR) node]
                                                        ;; open call -> call
                                                        [(OPEN-CALL)
                                                         (let ([offset (assq (ast-node-x node) offsets)])
                                                           (if offset
                                                               (encode-ast CALL (car offset) (cdr offset))
                                                               (peg-error "(grammar _)" ERROR-UNDEFINED-RULE (list (ast-node-x node)))))]
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
         ;; (capture px)    -> (ast CAPTURE '() px)
         (define capture
           (case-lambda
             [(px) (capture '() px)]
             [(fn px)
              (encode-ast CAPTURE (if (procedure? fn) fn '()) (check-ast px))]))

         ;; (transform fn px)
         ;;   where fn = function
         ;;         px = pattern
         ;;
         ;; (transform fn px) -> (ast TRANSFORM fn px)
         (define transform
           (lambda (fn px)
             (if (procedure? fn)
                 (encode-ast TRANSFORM fn (check-ast px))
                 (peg-error "(transform _)" ERROR-TYPE-FUNCTION (list fn)))))

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
                   [else
                    (peg-error "(text _)" ERROR-TYPE-STRING (list xs))])))

         ;; === Unit Tests ===

         (define unit-tests
           (test-chunk
            "Cursor Core"
            ([A (encode-ast CHARACTER #\a)]
             [B (encode-ast CHARACTER #\b)]
             [C (encode-ast CHARACTER #\c)]
             [identity     (lambda (x) x)]
             [error-equal? (lambda (x y)
                             (and (peg-error? x) (peg-error? y)
                                  (equal? (list (condition-who x) (condition-message x) (condition-irritants x))
                                          (list (condition-who y) (condition-message y) (condition-irritants y)))))]
             [peg-error    (lambda (who message irritants)
                             (condition (make-peg-error)
                                        (make-who-condition who)
                                        (make-message-condition message)
                                        (make-irritants-condition irritants)))])
            ;; === Literals ===
            (test-assert "character literal"
                         ast-equal?
                         (char #\a)
                         A)

            (test-assert "character error"
                         error-equal?
                         (catch (char "a"))
                         (peg-error "(char _)" ERROR-TYPE-CHARACTER (list "a")))

            ;; === Concatenation ===
            (test-assert "text sequence abc"
                         ast-equal?
                         (text "abc")
                         (encode-ast SEQUENCE (list A B C)))

            (test-assert "text sequence error"
                         error-equal?
                         (catch (text #\a))
                         (peg-error "(text _)" ERROR-TYPE-STRING (list #\a)))

            (test-assert "text epsilon"
                         ast-equal?
                         (text "")
                         empty)

            (test-assert "and-then abc"
                         ast-equal?
                         (and-then (char #\a)
                                   (char #\b)
                                   (char #\c))
                         (encode-ast SEQUENCE (list A B C)))

            (test-assert "and-then nested"
                         ast-equal?
                         (and-then (and-then (char #\a)
                                             (char #\b))
                                   (char #\c)
                                   (text "ba"))
                         (encode-ast SEQUENCE (list A B C B A)))

            (test-assert "and-then identity"
                         ast-equal?
                         (and-then)
                         empty)

            ;; === Ordered Choice ===
            (test-assert "or-else, a / b"
                         ast-equal?
                         (or-else (char #\a) (char #\b))
                         (encode-ast CHOICE (list A B)))

            (test-assert "or-else nested"
                         ast-equal?
                         (or-else (char #\a)
                                  (or-else (char #\b)
                                           (char #\c)))
                         (encode-ast CHOICE (list A B C)))

            (test-assert "or-else identity"
                         ast-equal?
                         (or-else)
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

            (test-assert "repeat nullable"
                         error-equal?
                         (catch (repeat empty))
                         (peg-error "(repeat _)" ERROR-NULLABLE '()))

            ;; === Not Predicate ===
            (test-assert "predicate !a"
                         ast-equal?
                         (is-not? (char #\a))
                         (encode-ast IS-NOT A))

            ;; === Predicate ===
            (test-assert "predicate &a"
                         ast-equal?
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

            (test-assert "character set error"
                         error-equal?
                         (catch (one-of (list #\a #\b #\c)))
                         (peg-error "(one-of _)" ERROR-TYPE-STRING (list (list #\a #\b #\c))))

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
                         (grammar [R1 (and-then (text "ab") (rule R2))]
                                  [R2 (text "c")])
                         (encode-ast GRAMMAR
                                     (vector (encode-ast RULE
                                                         (quote R1)
                                                         (encode-ast SEQUENCE
                                                                     (list A
                                                                           B
                                                                           (encode-ast CALL
                                                                                       (quote R2)
                                                                                       1))))
                                             (encode-ast RULE
                                                         (quote R2)
                                                         C))))

            (test-assert "undefined rule"
                         error-equal?
                         (catch (grammar [A (and-then (char #\a) (rule C))]
                                         [B (char #\b)]))
                         (peg-error "(grammar _)" ERROR-UNDEFINED-RULE (list (quote C))))

            ;; Left Recursion: A → Bβ such that B ⇒ Aγ
            (test-assert "grammar, direct left recursion"
                         error-equal?
                         (catch (grammar [R (rule R)]))
                         (peg-error "(grammar _)" ERROR-LEFT-RECURSION (list (quote R))))

            (test-assert "grammar, indirect left recursion"
                         error-equal?
                         (catch (grammar [A (and-then (rule B) (char #\x))]
                                         [B (and-then (rule C) (char #\y))]
                                         [C (and-then (rule A) (char #\z))]))
                         (peg-error "(grammar _)" ERROR-LEFT-RECURSION (list (quote A))))
            ))
         
)

(library (cursor core)
         (export empty
                 fail
                 any
                 character
                 sequence
                 choice
                 maybe
                 repeat
                 repeat+1
                 is?
                 is-not?
                 one-of
                 none-of
                 grammar
                 call
                 capture
                 text
                 (rename (unit-tests core:unit-tests)))
         (import (rnrs)
                 (cursor tools)
                 (cursor collections charset))

         ;; === Constants ===

         (enum ERROR
               EMPTY
               ANY
               FAIL
               FAIL-TWICE
               CHARACTER
               SEQUENCE
               CHOICE
               COMMIT
               PARTIAL-COMMIT
               BACK-COMMIT
               GRAMMAR
               RULE
               CALL
               OPEN-CALL
               RETURN
               JUMP
               CAPTURE-START
               CAPTURE-STOP
               REPEAT
               IS
               IS-NOT
               ONE-OF
               NONE-OF
               MATCH)

         ;; === Error Messages ===

         (define ERROR-TYPE-CHARACTER "not a character")
         (define ERROR-TYPE-SYMBOL    "not a symbol")
         (define ERROR-TYPE-STRING    "not a string")
         (define ERROR-TYPE-FUNCTION  "not a function")
         (define ERROR-FUNCTION-ARITY "mismatched arity")
         (define ERROR-MALFORMED-CODE "malformed instruction")
         (define ERROR-UNDEFINED-RULE "undefined rule in grammar")
         (define ERROR-NULLABLE       "empty pattern within may cause infinite loop")

         ;; === Data ===

         ;; Instruction Set: (list code code ...)
         ;;   where code = (encode type op-x op-y)

         ;; record-type: code
         ;;
         ;; An instruction containing a type identifier followed by two operands.
         (define-record-type (code encode code?)
           (fields type op-x op-y)
           (nongenerative)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
                [(type)           (new type '() '())]
                [(type op-x)      (new type op-x '())]
                [(type op-x op-y) (new type op-x op-y)]))))

         ;; === Helper Functions ===

         (define check-length
           (lambda (x)
             (if (list? x)
                 (length x)
                 1)))

         (define check-code
           (lambda (x)
             (cond [(code? x) (list x)]
                   [(not (pair? x))
                    (list (encode ERROR x ERROR-MALFORMED-CODE))]
                   [(and (pair? x) (not (code? (car x))))
                    (list (encode ERROR x ERROR-MALFORMED-CODE))]
                   [else x])))

         (define nullable?
           (lambda (xs)
             (let recur ([xs xs]
                         [offset 0])
                     ;; End of list.
               (cond [(null? xs) #f]
                     ;; Check instructions at offsets.
                     [(> offset 0) (recur (cdr xs) (- offset 1))]
                     ;; If not malformed instruction, check for nullability.
                     [(and (code? (car xs))
                           (not (eq? ERROR (code-type (car xs)))))
                      (let* ([x    (car xs)]
                             [xs   (cdr xs)]
                             [type (code-type x)]
                             [op-x (code-op-x x)]
                             [op-y (code-op-y x)])
                              ;; Terminating cases.
                        (cond [(or (eq? type CHARACTER)
                                   (eq? type ANY)
                                   (eq? type FAIL)
                                   (eq? type OPEN-CALL)) #f]
                              [(or (eq? type EMPTY)
                                   (eq? op-y REPEAT)
                                   (eq? op-y IS)
                                   (eq? op-y IS-NOT)) #t]
                              ;; === choices ===
                              ;; Check first if second is nullable.
                              [(eq? type CHOICE) (if (recur xs (code-op-x x))
                                                     (recur (list x) offset)
                                                     #f]
                              ;; === sequences ===
                              ;; If first pattern is nullable, check subsequent.
                              [(recur (list x) offset) (recur xs offset)]
                              [else #f]))]
                     [else #f]))))

         ;; === Atoms ===

         ;; empty
         ;;
         ;; The empty pattern, which always succeeds and
         ;; consumes no input. Also known as epsilon.
         (define empty (list (encode EMPTY)))

         ;; fail
         ;;
         ;; Force failure.
         (define fail (list (encode FAIL)))

         ;; any
         ;;
         ;; Match and consume any character if there
         ;; is input to be consumed.
         (define any (list (encode ANY)))

         ;; (character x)
         ;;   where x = char
         ;;
         ;; Match and consume the given character.
         (define character
           (lambda (x)
             (if (char? x)
                 (list (encode CHARACTER x))
                 (list (encode ERROR x ERROR-TYPE-CHARACTER)))))

         ;; === Concatenation ===

         ;; (and-then px py)
         ;;
         ;; Match two patterns in sequence. Acts as flat-map or concat-map,
         ;; marking encoding errors while merging lists of instructions.
         (define and-then
           (lambda (px py)
             (append (check-code px) (check-code py))))

         ;; (sequence px py pz ...)
         ;;
         ;; Match a sequence of two or more patterns.
         (define sequence
           (lambda xs
             (if (>= (length xs) 2)
                 (reduce-right and-then xs)
                 (list (encode ERROR SEQUENCE ERROR-FUNCTION-ARITY)))))

         ;; === Ordered Choice: Limited Backtracking ===

         ;; (or-else px py)
         ;;
         ;; Match one of two patterns. Ordered choice.
         (define or-else
           (lambda (px py)
             (let ([offset-x (check-length px)]
                   [offset-y (check-length py)])
               (sequence (encode CHOICE (+ offset-x 2))
                         px
                         (encode COMMIT (+ offset-y 1))
                         py))))

         ;; (choice px py pz ...)
         ;;
         ;; Match one of two or more patterns.
         (define choice
           (lambda xs
             (if (>= (length xs) 2)
                 (reduce-right or-else xs)
                 (list (encode ERROR CHOICE ERROR-FUNCTION-ARITY)))))

         ;; (maybe px)
         ;;
         ;; Optional match. Always succeeds.
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; === Repetition ===

         ;; (repeat px)
         ;;
         ;; Match pattern zero or more times. Always succeeds.
         ;;
         ;; Side Note: Repetition on a pattern that succeeds
         ;; but consumes no input creates an infinite loop.
         (define repeat
           (lambda (px)
             (let ([pattern (check-code px)])
               (cond [(not (nullable? pattern))
                      (let ([offset (length pattern)])
                        (sequence (encode CHOICE (+ offset 2) REPEAT)
                                  pattern
                                  (encode PARTIAL-COMMIT (- offset))))]
                     [else
                      (list (encode ERROR REPEAT ERROR-NULLABLE))]))))

         ;; (repeat+1 px)
         ;;
         ;; Match pattern one or more times.
         (define repeat+1
           (lambda (px)
             (sequence px (repeat px))))

         ;; === Syntactic Predicates: Unlimited Lookahead ===

         ;; (is? px)
         ;;
         ;; Look ahead by pattern. Succeeds if pattern succeeds.
         ;; Fails if pattern fails. Consumes no input.
         (define is?
           (lambda (px)
             (let ([offset-x (check-length px)]
                   [offset-y 2])
               (sequence (encode CHOICE (+ offset-x 2) IS)
                         px
                         (encode BACK-COMMIT offset-y)
                         fail))))

         ;; (is-not? px)
         ;;
         ;; Look ahead by pattern. Fails if pattern succeeds.
         ;; Succeeds if pattern fails. Consumes no input.
         (define is-not?
           (lambda (px)
             (let ([offset (check-length px)])
               (sequence (encode CHOICE (+ offset 2) IS-NOT)
                         px
                         (encode FAIL-TWICE)))))

         ;; === Sets: Character Classes ===

         ;; (one-of xs)
         ;;   where xs = string
         ;;
         ;; Match character in a set of characters.
         ;; Constructs set from given string.
         (define one-of
           (lambda (xs)
             (if (string? xs)
                 (list (encode ONE-OF (make-charset xs)))
                 (list (encode ERROR ONE-OF ERROR-TYPE-STRING)))))

         ;; (none-of xs)
         ;;   where xs = string
         ;;
         ;; Match character not in a set of characters.
         ;; Constructs set from given string.
         (define none-of
           (lambda (xs)
             (if (string? xs)
                 (list (encode NONE-OF (make-charset xs)))
                 (list (encode ERROR NONE-OF ERROR-TYPE-STRING)))))

         ;; === Grammar ===

         ;; (call x)
         ;;   where x = symbol
         ;;
         ;; Calls rule within a closed grammar.
         (define-syntax call
           (syntax-rules ()
             [(_ x)
              (let ([id (quote x)])
                (if (symbol? id)
                    (list (encode OPEN-CALL id))
                    (list (encode ERROR OPEN-CALL ERROR-TYPE-SYMBOL))))]))

         ;; (grammar [rule pattern]
         ;;          [rule pattern] ...)
         ;;
         ;; Allows recursive patterns for grammar construction.
         ;; A sequence of one or more rules. The first rule is
         ;; the start state.
         (define-syntax grammar
           (lambda (stx)
             (syntax-case stx ()
               [(grammar [rule-x body-x] [rule-y body-y] ...)
                (with-syntax ([(size-x size-y ...)
                               (generate-temporaries (syntax (rule-x rule-y ...)))])
                  (syntax (let* ([rule-x  (sequence (encode RULE (quote rule-x)) body-x (encode RETURN))]
                                 [rule-y  (sequence (encode RULE (quote rule-y)) body-y (encode RETURN))]
                                 ...
                                 [size-x  (check-length rule-x)]
                                 [size-y  (check-length rule-y)]
                                 ...
                                 [symbols (quote (rule-x rule-y ...))]
                                 [offsets (zip-with cons symbols (scan-right + 2 (list 0 size-x size-y ...)))]
                                 [total   (apply + (list size-x size-y ...))]
                                 [rules   (sequence (encode GRAMMAR (+ total 3))
                                                    (encode CALL (quote rule-x) 2)
                                                    (encode JUMP (+ total 1))
                                                    rule-x
                                                    rule-y ...)])
                            (map (lambda (x)
                                   (cond [(and (code? x) (eq? OPEN-CALL (code-type x)))
                                          (let ([offset (assq (code-op-x x) offsets)])
                                            (if offset
                                                (encode CALL (car offset) (cdr offset))
                                                (encode ERROR (code-op-x x) ERROR-UNDEFINED-RULE)))]
                                         [else x]))
                                 rules))))])))

         ;; (capture px)
         ;; (capture fn px)
         ;;   where fn = function
         ;;         px = pattern
         ;;
         ;; Pattern returns list of character matches —
         ;; optionally transformed by an arbitrary function.
         ;; Ignores "fn" arguments that are not functions.
         (define capture
           (case-lambda
             [(px) (capture '() px)]
             [(fn px)
              (cond [(procedure? fn)
                     (sequence (encode CAPTURE-START fn)
                               px
                               (encode CAPTURE-STOP))]
                    [else
                     (sequence (encode CAPTURE-START)
                               px
                               (encode CAPTURE-STOP))])]))

         ;; (audit xs)
         ;;   where xs = pattern instructions
         ;;
         ;; Traverses instruction list, looking for invalid patterns.

         ;; (text xs)
         ;;
         ;; Transforms a string literal into a sequence
         ;; of character instructions. Returns the empty
         ;; instruction for an empty string.
         (define text
           (lambda (xs)
             (cond [(string? xs)
                    (let* ([characters (map character (string->list xs))]
                           [size       (length characters)])
                      (cond [(< size 1) empty]
                            [(= size 1) (car characters)]
                            [else (apply sequence characters)]))]
                   [else (list (encode ERROR SEQUENCE ERROR-TYPE-STRING))])))

         ;; === Unit Tests ===

         (define unit-tests
           (let* ([a (encode CHARACTER #\a)]
                  [b (encode CHARACTER #\b)]
                  [c (encode CHARACTER #\c)]
                  [u (encode CHARACTER #\⌘)]
                  [code-equal? (lambda (a b)
                                 (and (code? a)
                                      (code? b)
                                      (let ([a-type (code-type a)]
                                            [a-x    (code-op-x a)]
                                            [a-y    (code-op-y a)]
                                            [b-type (code-type b)]
                                            [b-x    (code-op-x b)]
                                            [b-y    (code-op-y b)])
                                        (or (and (eq? a-type b-type)
                                                 (charset? a-x)
                                                 (charset? b-x)
                                                 (charset-equal? a-x b-x))
                                            (equal? (list a-type a-x a-y)
                                                    (list b-type b-x b-y))))))]
                  [instructions-equal? (lambda (xs ys)
                                         (and (list? xs)
                                              (list? ys)
                                              (= (length xs) (length ys))
                                              (for-all code-equal? xs ys)))])
             (test-chunk
              "Cursor Core"
              ;; === Literals ===
              ;; Π(g, i, 'c') ≡ Char c
              (test-assert instructions-equal?
                           "character literal a"
                           (character #\a)
                           (list a))

              (test-assert instructions-equal?
                           "character literal ⌘"
                           (character #\⌘)
                           (list u))

              ;; === Concatenation ===
              ;; Π(g, i, p₁p₂) ≡ Π(g, i, p₁) Π(g, i + |Π(g, x, p₁)|, p₂)
              (test-assert instructions-equal?
                           "text sequence abc"
                           (text "abc")
                           (list a b c))

              (test-assert instructions-equal?
                           "text sequence ⌘b⌘"
                           (text "⌘b⌘")
                           (list u b u))

              (test-assert instructions-equal?
                           "sequence abc"
                           (sequence (character #\a)
                                     (character #\b)
                                     (character #\c))
                           (list a b c))

              (test-assert instructions-equal?
                           "text singular"
                           (text "a")
                           (character #\a))

              (test-assert instructions-equal?
                           "empty string"
                           (text "")
                           empty)

              ;; === Ordered Choice ===
              ;; Π(g, i, p₁/p₂) ≡ Choice |Π(g, x, p₁)| + 2
              ;;                  Π(g, i + 1, p₁)
              ;;                  Commit |Π(g, x, p₂)| + 1
              ;;                  Π(g, i + |Π(g, x, p₁)| + 1, p₂)
              (test-assert instructions-equal?
                           "choice a / b"
                           (choice (character #\a)
                                   (character #\b))
                           (list (encode CHOICE 3)
                                 a
                                 (encode COMMIT 2)
                                 b))

              (test-assert instructions-equal?
                           "right associativity"
                           (choice (character #\a)
                                   (character #\b)
                                   (character #\c))
                           (choice (character #\a)
                                   (choice (character #\b)
                                           (character #\c))))

              ;; === Repetition ===
              ;; Π(g, i, p*) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               PartialCommit − |Π(g, x, p)|
              (test-assert instructions-equal?
                           "repeat a*"
                           (repeat (character #\a))
                           (list (encode CHOICE 3)
                                 a
                                 (encode PARTIAL-COMMIT -1)))

              (test-assert instructions-equal?
                           "repeat a+"
                           (repeat+1 (character #\a))
                           (list a
                                 (encode CHOICE 3)
                                 a
                                 (encode PARTIAL-COMMIT -1)))

              ;; === Not Predicate ===
              ;; Π(g, i, !p) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               FailTwice
              (test-assert instructions-equal?
                           "predicate !a"
                           (is-not? (character #\a))
                           (list (encode CHOICE 3)
                                 a
                                 (encode FAIL-TWICE)))

              ;; === And Predicate ===
              ;; Π(g, i, &p) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               BackCommit 2
              ;;               Fail
              (test-assert instructions-equal?
                           "predicate &a"
                           (is? (character #\a))
                           (list (encode CHOICE 3)
                                 a
                                 (encode BACK-COMMIT 2)
                                 (encode FAIL))))))

         )

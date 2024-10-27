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

         (define MAX-RULES 1000)

         ;; === Error Messages ===

         (define ERROR-TYPE-CHARACTER "not a character")
         (define ERROR-TYPE-SYMBOL    "not a symbol")
         (define ERROR-TYPE-STRING    "not a string")
         (define ERROR-TYPE-FUNCTION  "not a function")
         (define ERROR-FUNCTION-ARITY "mismatched arity")
         (define ERROR-MALFORMED-CODE "malformed instruction")
         (define ERROR-UNDEFINED-RULE "undefined rule in grammar")
         (define ERROR-NULLABLE       "pattern within may cause infinite loop")
         (define ERROR-LEFT-RECURSION "rule may be left recursive")

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
               (cond [(null? xs) #f]
                     ;; Check instructions at offsets.
                     [(> offset 1) (recur (cdr xs) (- offset 1))]
                     ;; If not malformed instruction, check for nullability.
                     [(and (code? (car xs)) (not (eq? ERROR (code-type (car xs)))))
                      (let* ([x    (car xs)]
                             [xs   (cdr xs)]
                             [type (code-type x)]
                             [op-x (code-op-x x)]
                             [op-y (code-op-y x)])
                              ;; Terminating cases.
                        (cond [(or (eq? type CHARACTER)
                                   (eq? type ANY)
                                   (eq? type FAIL)
                                   (eq? type ONE-OF)
                                   (eq? type NONE-OF)
                                   (eq? type OPEN-CALL)) #f]
                              [(or (eq? type EMPTY)
                                   (eq? op-y REPEAT)
                                   (eq? op-y IS)
                                   (eq? op-y IS-NOT)) #t]
                              ;; === choice ===
                              ;; Check first pattern if second pattern is nullable.
                              [(eq? type CHOICE) (if (recur xs op-x)
                                                     (recur xs offset)
                                                     #f)]
                              ;; === sequence ===
                              [else (recur xs offset)]
                     [else #f]))))

         (define check-grammar
           (lambda (xs)
             (let* ([instructions (list->vector xs)]
                    [size         (vector-length instructions)]
                    [rule-count   (make-eqv-hashtable)]
                    [step-count   0]
                    [error-flag   #f]
                    [check-rule (lambda (start)
                                  (let recur ([index start])
                                    (unless (or error-flag (>= index size))
                                      (let* ([code (vector-ref instructions index)]
                                             [type (code-type code)]
                                             [op-x (code-op-x code)]
                                             [op-y (code-op-y code)])

                                        (cond [(or (eq? type CHARACTER)
                                                   (eq? type ANY)
                                                   (eq? type FAIL)
                                                   (eq? type ONE-OF)
                                                   (eq? type NONE-OF))]

                                              [(or (eq? type EMPTY)
                                                   (eq? op-y REPEAT)
                                                   (eq? op-y IS)
                                                   (eq? op-y IS-NOT))
                                               (recur (+ index 1))]

                                              [(eq? type CHOICE)
                                               (recur (+ index 1))
                                               (unless error-flag
                                                 (recur (+ index op-x)))]

                                              [(or (eq? type GRAMMAR)
                                                   (eq? type CALL))
                                               (recur op-y)]

                                              [(eq? type RULE)
                                               (set! step-count (+ step-count 1))
                                               (cond [error-flag]
                                                     [(>= step-count MAX-RULES) (set! error-flag #t)]
                                                     [else
                                                      (let ([total (hashtable-ref rule-count op-x #f)])
                                                        (if total
                                                            (begin (hashtable-set! rule-count op-x (+ total 1))
                                                                   (recur (+ index 1)))
                                                            (begin (hashtable-set! rule-count op-x 0)
                                                                   (recur (+ index 1)))))])]

                                              [else (recur (+ index 1))])))))]
                    [find-rule   (lambda ()
                                   (let ([rules (hashtable-keys rule-count)])
                                     (vector-fold (lambda (rule-x rule-y)
                                                    (let ([count-x (hashtable-ref rule-x 0)]
                                                          [count-y (hashtable-ref rule-y 0)])
                                                      (if (> count-x count-y)
                                                          rule-x
                                                          rule-y)))
                                                  rules)))]
                    [next-rule   (lambda (i)
                                   (let loop ([index i])
                                     (cond [(>= index size) index]
                                           [(eq? RULE (code-type (vector-ref instructions index))) index]
                                           [else (loop (+ index 1))])))]
                    [check-rules (lambda ()
                                   (let loop ([index (next-rule 0)])
                                     (cond [error-flag (list (encode ERROR (find-rule) ERROR-LEFT-RECURSION))]
                                           [(< index size)
                                            (check-rule index)
                                            (loop (next-rule (+ index 1)))]
                                           [else xs])))])
               (check-rules))))

         ;; === Terminals ===

         ;; empty -> ε
         ;;
         ;; Always succeeds and consumes no input.
         (define empty (list (encode EMPTY)))

         ;; fail
         ;;
         ;; Force failure.
         (define fail (list (encode FAIL)))

         ;; any -> .
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

         ;; (sequence px py ...)
         ;;
         ;; (sequence px py) -> px • py
         ;; (sequence px)    -> px
         ;; (sequence)       -> ε
         (define sequence
           (case-lambda
            [()  empty]
            [(x) (check-code x)]
            [xs  (fold-right (lambda (px py)
                               (append (check-code px) py))
                             '()
                             xs)]))

         ;; === Ordered Choice: Limited Backtracking ===

         (define fold-choices
           (let ([or-else (lambda (px py)
                            (let ([offset-x (check-length px)]
                                  [offset-y (car py)]
                                  [py       (cdr py)])
                              (cons (+ offset-x offset-y 2)
                                    (append (list (encode CHOICE (+ offset-x 2)))
                                            (check-code px)
                                            (list (encode COMMIT (+ offset-y 1)))
                                            py))))])
             (lambda (xs)
               (if (null? (cdr xs))
                   (cons (check-length (car xs)) (check-code (car xs)))
                   (or-else (car xs) (fold-choices (cdr xs)))))))

         ;; (choice px py ...)
         ;;
         ;; (choice px py) -> px / py
         ;; (choice px)    -> px
         ;; (choice)       -> fail
         (define choice
           (case-lambda
            [()  fail]
            [(x) (check-code x)]
            [xs  (cdr (fold-choices xs))]))

         ;; (maybe px)
         ;;
         ;; (maybe px) -> px?
         ;; (maybe px) -> px / ε
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; === Repetition ===

         ;; (repeat px)
         ;;
         ;; (repeat px) -> px*
         ;;
         ;; Side Note: Repetition on a pattern that succeeds
         ;; but consumes no input creates an infinite loop.
         (define repeat
           (lambda (px)
             (let ([pattern (check-code px)])
               (cond [(not (nullable? pattern))
                      (let ([offset (length pattern)])
                        (append (list (encode CHOICE (+ offset 2) REPEAT))
                                pattern
                                (list (encode PARTIAL-COMMIT (- offset)))))]
                     [else
                      (list (encode ERROR REPEAT ERROR-NULLABLE))]))))

         ;; (repeat+1 px)
         ;;
         ;; (repeat+1 px) -> px+
         ;; (repeat+1 px) -> px • px*
         (define repeat+1
           (lambda (px)
             (append (check-code px) (repeat px))))

         ;; === Syntactic Predicates: Unlimited Lookahead ===

         ;; (is? px)
         ;;
         ;; (is? px) -> &px
         ;;
         ;; Look ahead by pattern. Succeeds if pattern succeeds.
         ;; Fails if pattern fails. Consumes no input.
         (define is?
           (lambda (px)
             (let ([offset-x (check-length px)]
                   [offset-y 2])
               (append (list (encode CHOICE (+ offset-x 2) IS))
                       (check-code px)
                       (list (encode BACK-COMMIT offset-y))
                       fail))))

         ;; (is-not? px)
         ;;
         ;; (is-not? px) -> !px
         ;;
         ;; Look ahead by pattern. Fails if pattern succeeds.
         ;; Succeeds if pattern fails. Consumes no input.
         (define is-not?
           (lambda (px)
             (let ([offset (check-length px)])
               (append (list (encode CHOICE (+ offset 2) IS-NOT))
                       (check-code px)
                       (list (encode FAIL-TWICE))))))

         ;; === Sets: Character Classes ===

         ;; (one-of xs)
         ;;   where xs = string
         ;;
         ;; (one-of xs) -> [xy...]
         ;; (one-of "") -> fail
         ;;
         ;; Match character in a set of characters. Constructs set from given string.
         (define one-of
           (lambda (xs)
             (cond [(string? xs)
                    (if (string=? xs "")
                        fail
                        (list (encode ONE-OF (make-charset xs))))]
                   [else (list (encode ERROR ONE-OF ERROR-TYPE-STRING))])))

         ;; (none-of xs)
         ;;   where xs = string
         ;;
         ;; (none-of xs) -> [^xy...]
         ;; (none-of "") -> any
         ;;
         ;; Match character not in a set of characters. Constructs set from given string.
         (define none-of
           (lambda (xs)
             (cond [(string? xs)
                    (if (string=? xs "")
                        any
                        (list (encode NONE-OF (make-charset xs))))]
                   [else (list (encode ERROR NONE-OF ERROR-TYPE-STRING))])))

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
         ;; (grammar [rule pattern]) -> rule <- pattern
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
                  (syntax (let ([rule-x  (sequence (encode RULE (quote rule-x)) body-x (encode RETURN))]
                                [rule-y  (sequence (encode RULE (quote rule-y)) body-y (encode RETURN))]
                                ...)
                            (let* ([size-x  (length rule-x)]
                                   [size-y  (length rule-y)]
                                   ...
                                   [symbols (quote (rule-x rule-y ...))]
                                   [offsets (zip-with cons symbols (scan + (list 2 size-x size-y ...)))]
                                   [total   (apply + (list size-x size-y ...))]
                                   [rules   (append (list (encode GRAMMAR (+ total 2) 2))
                                                    (list (encode JUMP (+ total 1)))
                                                    rule-x
                                                    rule-y ...)])
                              (map (lambda (x)
                                     (cond [(and (code? x) (eq? OPEN-CALL (code-type x)))
                                            (let ([offset (assq (code-op-x x) offsets)])
                                              (if offset
                                                  (encode CALL (car offset) (cdr offset))
                                                  (encode ERROR (code-op-x x) ERROR-UNDEFINED-RULE)))]
                                           [else x]))
                                   rules))))]))))

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
                     (append (list (encode CAPTURE-START fn))
                             (check-code px)
                             (list (encode CAPTURE-STOP)))]
                    [else
                     (append (list (encode CAPTURE-START))
                             (check-code px)
                             (list (encode CAPTURE-STOP)))])]))

         ;; (audit xs)
         ;;   where xs = pattern instructions
         ;;
         ;; Traverses instruction list, looking for invalid patterns.

         ;; (text xs)
         ;;
         ;; (text xs) -> x • y ...
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
                            [else       (apply append characters)]))]
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
              (test-assert "character literal a"
                           instructions-equal?
                           (character #\a)
                           (list a))

              (test-assert "character literal ⌘"
                           instructions-equal?
                           (character #\⌘)
                           (list u))

              ;; === Concatenation ===
              ;; Π(g, i, p₁p₂) ≡ Π(g, i, p₁) Π(g, i + |Π(g, x, p₁)|, p₂)
              (test-assert "text sequence abc"
                           instructions-equal?
                           (text "abc")
                           (list a b c))

              (test-assert "text sequence ⌘b⌘"
                           instructions-equal?
                           (text "⌘b⌘")
                           (list u b u))

              (test-assert "sequence abc"
                           instructions-equal?
                           (sequence (character #\a)
                                     (character #\b)
                                     (character #\c))
                           (list a b c))

              (test-assert "text singular"
                           instructions-equal?
                           (text "a")
                           (character #\a))

              (test-assert "empty string"
                           instructions-equal?
                           (text "")
                           empty)

              ;; === Ordered Choice ===
              ;; Π(g, i, p₁/p₂) ≡ Choice |Π(g, x, p₁)| + 2
              ;;                  Π(g, i + 1, p₁)
              ;;                  Commit |Π(g, x, p₂)| + 1
              ;;                  Π(g, i + |Π(g, x, p₁)| + 1, p₂)
              (test-assert "choice a / b"
                           instructions-equal?
                           (choice (character #\a)
                                   (character #\b))
                           (list (encode CHOICE 3)
                                 a
                                 (encode COMMIT 2)
                                 b))

              (test-assert "right associativity"
                           instructions-equal?
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
              (test-assert "repeat a*"
                           instructions-equal?
                           (repeat (character #\a))
                           (list (encode CHOICE 3 REPEAT)
                                 a
                                 (encode PARTIAL-COMMIT -1)))

              (test-assert "repeat a+"
                           instructions-equal?
                           (repeat+1 (character #\a))
                           (list a
                                 (encode CHOICE 3 REPEAT)
                                 a
                                 (encode PARTIAL-COMMIT -1)))

              ;; === Not Predicate ===
              ;; Π(g, i, !p) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               FailTwice
              (test-assert "predicate !a"
                           instructions-equal?
                           (is-not? (character #\a))
                           (list (encode CHOICE 3 IS-NOT)
                                 a
                                 (encode FAIL-TWICE)))

              ;; === And Predicate ===
              ;; Π(g, i, &p) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               BackCommit 2
              ;;               Fail
              (test-assert "predicate &a"
                           instructions-equal?
                           (is? (character #\a))
                           (list (encode CHOICE 3 IS)
                                 a
                                 (encode BACK-COMMIT 2)
                                 (encode FAIL))))))

         )

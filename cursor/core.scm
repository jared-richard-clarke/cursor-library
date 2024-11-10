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
         (define ERROR-TYPE-FUNCTION  "not a function")
         (define ERROR-FUNCTION-ARITY "mismatched arity")
         (define ERROR-MALFORMED-CODE "malformed instruction")
         (define ERROR-UNDEFINED-RULE "undefined rule in grammar")
         (define ERROR-NULLABLE       "expression within may cause infinite loop")
         (define ERROR-LEFT-RECURSION "rule may be left recursive")

         ;; === Helper Functions ===

         (define check-length
           (lambda (x)
             (if (list? x)
                 (length x)
                 1)))

         (define check-code
           (lambda (x)
             (cond [(code? x) (list x)]
                   [(or (not (pair? x))
                        (and (pair? x) (not (code? (car x)))))
                    (list (encode ERROR x ERROR-MALFORMED-CODE))]
                   [else x])))

         (define encoding-error?
           (lambda (xs)
             (exists (lambda (x)
                       (or (not (code? x))
                           (eq? (code-type x) ERROR)))
                     xs)))

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
                              [else (recur xs offset)]))]
                     [else #f]))))

         (define check-grammar
           (lambda (xs)
             (let* ([instructions (list->vector xs)]
                    [size         (vector-length instructions)]
                    [rule-count   (make-eqv-hashtable)]
                    [step-count   0]
                    [error-flag   #f]
                    [check-rule   (lambda (start)
                                    (let recur ([index start])
                                      (unless (or error-flag (>= index size))
                                        (let* ([code (vector-ref instructions index)]
                                               [type (code-type code)]
                                               [op-x (code-op-x code)]
                                               [op-y (code-op-y code)])
                                          (cond [(eq? type RULE)
                                                 (set! step-count (+ step-count 1))
                                                 (cond [error-flag]
                                                       [(>= step-count MAX-RULES) (set! error-flag #t)]
                                                       [else
                                                        (let ([total (hashtable-ref rule-count op-x #f)])
                                                          (if total
                                                              (begin (hashtable-set! rule-count op-x (+ total 1))
                                                                     (recur (+ index 1)))
                                                              (begin (hashtable-set! rule-count op-x 1)
                                                                     (recur (+ index 1)))))])]
                                                [(or (eq? type GRAMMAR)
                                                     (eq? type CALL)
                                                     (eq? type JUMP))
                                                 (recur op-y)]
                                                [(eq? type CHOICE)
                                                 (recur (+ index 1))
                                                 (unless error-flag
                                                   (recur (+ index op-x)))]
                                                [(or (eq? type CHARACTER)
                                                     (eq? type ANY)
                                                     (eq? type FAIL)
                                                     (eq? type ONE-OF)
                                                     (eq? type NONE-OF))]
                                                [(or (eq? type EMPTY)
                                                     (eq? op-y REPEAT)
                                                     (eq? op-y IS)
                                                     (eq? op-y IS-NOT))
                                                 (recur (+ index 1))]
                                                [else (recur (+ index 1))])))))]
                    [find-rule   (lambda ()
                                   (let ([rules (hashtable-keys rule-count)])
                                     (vector-fold (lambda (rule-x rule-y)
                                                    (let ([count-x (hashtable-ref rule-count rule-x 0)]
                                                          [count-y (hashtable-ref rule-count rule-y 0)])
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

         ;; empty = ε
         (define empty (list (encode EMPTY)))

         ;; fail
         (define fail (list (encode FAIL)))

         ;; any = .
         (define any (list (encode ANY)))

         ;; (char #\a) = "a"
         (define char
           (lambda (x)
             (if (char? x)
                 (list (encode CHARACTER x))
                 (list (encode ERROR x ERROR-TYPE-CHARACTER)))))

         ;; === Concatenation ===

         ;; (sequence px py ...) = px • py • ...
         ;; (sequence px)        = px
         ;; (sequence)           = ε
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

         ;; (choice px py ...) = px / py / ...
         ;; (choice px)        = px
         ;; (choice)           = fail
         (define choice
           (case-lambda
            [()  fail]
            [(x) (check-code x)]
            [xs  (cdr (fold-choices xs))]))

         ;; (maybe px) = px? = px / ε
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; === Repetition ===

         ;; (repeat px) = px*
         (define repeat
           (lambda (px)
             (let ([pattern (check-code px)])
               (cond [(not (nullable? pattern))
                      (let ([offset (length pattern)])
                        (append (list (encode CHOICE (+ offset 2) REPEAT))
                                pattern
                                (list (encode PARTIAL-COMMIT (- offset)))))]
                     [else
                      (list (encode ERROR 'repeat ERROR-NULLABLE))]))))

         ;; (repeat+1 px) = px+
         ;; (repeat+1 px) = px • px*
         (define repeat+1
           (lambda (px)
             (append (check-code px) (repeat px))))

         ;; === Syntactic Predicates: Unlimited Lookahead ===

         ;; (is? px) = &px
         (define is?
           (lambda (px)
             (let ([offset-x (check-length px)]
                   [offset-y 2])
               (append (list (encode CHOICE (+ offset-x 2) IS))
                       (check-code px)
                       (list (encode BACK-COMMIT offset-y))
                       fail))))

         ;; (is-not? px) = !px
         (define is-not?
           (lambda (px)
             (let ([offset (check-length px)])
               (append (list (encode CHOICE (+ offset 2) IS-NOT))
                       (check-code px)
                       (list (encode FAIL-TWICE))))))

         ;; === Sets: Character Classes ===

         ;; (one-of "abc") = [abc]
         ;; (one-of "")    = ∅
         ;;   where ∅ = the empty set
         (define one-of
           (lambda (xs)
             (cond [(string? xs)
                    (if (string=? xs "")
                        fail
                        (list (encode ONE-OF (make-charset xs))))]
                   [else (list (encode ERROR 'one-of ERROR-TYPE-STRING))])))

         ;; (none-of "abc") = [^abc]
         ;; (none-of "")    = U
         ;;   where U = the universal set
         (define none-of
           (lambda (xs)
             (cond [(string? xs)
                    (if (string=? xs "")
                        any
                        (list (encode NONE-OF (make-charset xs))))]
                   [else (list (encode ERROR 'none-of ERROR-TYPE-STRING))])))

         ;; === Grammar ===

         ;; (call x)
         ;;   where x = symbol
         (define-syntax call
           (syntax-rules ()
             [(_ x)
              (let ([id (quote x)])
                (if (symbol? id)
                    (list (encode OPEN-CALL id))
                    (list (encode ERROR OPEN-CALL ERROR-TYPE-SYMBOL))))]))

         ;; (grammar [id pattern] ...) = id <- pattern
         ;;                              ...
         (define-syntax grammar
           (lambda (stx)
             (syntax-case stx ()
               [(grammar [rule-x body-x] [rule-y body-y] ...)
                (with-syntax ([(size-x size-y ...)
                               (generate-temporaries (syntax (rule-x rule-y ...)))])
                  (syntax (let ([rule-x (sequence (encode RULE (quote rule-x)) body-x (encode RETURN))]
                                [rule-y (sequence (encode RULE (quote rule-y)) body-y (encode RETURN))]
                                ...)
                            (let ([size-x (length rule-x)]
                                  [size-y (length rule-y)]
                                  ...)
                              (let* ([symbols    (quote (rule-x rule-y ...))]
                                     [offsets    (zip-with cons symbols (scan + (list 2 size-x size-y ...)))]
                                     [total      (+ size-x size-y ...)]
                                     ;; Combine rules into list of grammar instructions.
                                     [open-rules (append (list (encode GRAMMAR (+ total 2) 2))
                                                         (list (encode JUMP (+ total 1)))
                                                         rule-x
                                                         rule-y ...)]
                                     ;; Close open calls, adding the appropriate offsets.
                                     [closed-rules
                                      (peek-map (lambda (x xs peekable?)
                                                  (cond [(and (code? x) (eq? OPEN-CALL (code-type x)))
                                                         (let ([offset (assq (code-op-x x) offsets)])
                                                           (if offset
                                                               (if (and peekable?
                                                                        (let ([next (car xs)])
                                                                          (and (code? next) (eq? RETURN (code-type next)))))
                                                                   ;; tail call
                                                                   (encode JUMP (cdr offset) (car offset))
                                                                   ;; standard call
                                                                   (encode CALL (car offset) (cdr offset)))
                                                               (encode ERROR (code-op-x x) ERROR-UNDEFINED-RULE)))]
                                                        [else x]))
                                                open-rules)])
                                ;; If grammar otherwise free of errors, check for possible left recursion.
                                (if (encoding-error? closed-rules)
                                    closed-rules
                                    (check-grammar closed-rules)))))))])))

         ;; (capture fn px)
         ;;   where fn = function
         ;;         px = instruction-list
         (define capture
           (case-lambda
             [(px) (capture '() px)]
             [(fn px)
              (append (list (encode CAPTURE-START (if (procedure? fn) fn '())))
                      (check-code px)
                      (list (encode CAPTURE-STOP)))]))

         ;; (text "abc") = a • b • c
         ;; (text "")    = ε
         (define text
           (lambda (xs)
             (cond [(string? xs)
                    (let* ([characters (map char (string->list xs))]
                           [size       (length characters)])
                      (cond [(< size 1) empty]
                            [(= size 1) (car characters)]
                            [else       (apply append characters)]))]
                   [else (list (encode ERROR 'text ERROR-TYPE-STRING))])))

         ;; === Unit Tests ===

         (define unit-tests
           (let* ([a (encode CHARACTER #\a)]
                  [b (encode CHARACTER #\b)]
                  [c (encode CHARACTER #\c)]
                  [code-equal? (lambda (a b)
                                 (and (code? a)
                                      (code? b)
                                      (let ([a-type (code-type a)]
                                            [a-x    (code-op-x a)]
                                            [a-y    (code-op-y a)]
                                            [b-type (code-type b)]
                                            [b-x    (code-op-x b)]
                                            [b-y    (code-op-y b)])
                                            ;; Charset comparison.
                                        (or (and (eq? a-type b-type)
                                                 (charset? a-x)
                                                 (charset? b-x)
                                                 (charset-equal? a-x b-x))
                                            ;; Capture comparison.
                                            ;; Cannot meaningfully compare arbitrary functions.
                                            ;; Check only for their presence within captures.
                                            (and (eq? a-type CAPTURE-START)
                                                 (eq? a-type b-type)
                                                 (procedure? a-x)
                                                 (procedure? b-x))
                                            ;; Default comparison.
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
              (test-assert "character literal"
                           instructions-equal?
                           (char #\a)
                           (list a))

              (test-assert "string, not character"
                           instructions-equal?
                           (char "a")
                           (list (encode ERROR "a" ERROR-TYPE-CHARACTER)))

              ;; === Concatenation ===
              ;; Π(g, i, p₁p₂) ≡ Π(g, i, p₁) Π(g, i + |Π(g, x, p₁)|, p₂)
              (test-assert "text sequence abc"
                           instructions-equal?
                           (text "abc")
                           (list a b c))

              (test-assert "text epsilon"
                           instructions-equal?
                           (text "")
                           empty)

              (test-assert "sequence abc"
                           instructions-equal?
                           (sequence (char #\a)
                                     (char #\b)
                                     (char #\c))
                           (list a b c))

              (test-assert "sequence nested"
                           instructions-equal?
                           (sequence (sequence (char #\a)
                                               (char #\b))
                                     (char #\c)
                                     (text "ba"))
                           (list a b c b a))

              (test-assert "sequence identity"
                           instructions-equal?
                           (sequence)
                           empty)

              ;; === Ordered Choice ===
              ;; Π(g, i, p₁/p₂) ≡ Choice |Π(g, x, p₁)| + 2
              ;;                  Π(g, i + 1, p₁)
              ;;                  Commit |Π(g, x, p₂)| + 1
              ;;                  Π(g, i + |Π(g, x, p₁)| + 1, p₂)
              (test-assert "choice, a / b"
                           instructions-equal?
                           (choice (char #\a)
                                   (char #\b))
                           (list (encode CHOICE 3)
                                 a
                                 (encode COMMIT 2)
                                 b))

              (test-assert "choice, right associative"
                           instructions-equal?
                           (choice (char #\a)
                                   (char #\b)
                                   (char #\c))
                           (choice (char #\a)
                                   (choice (char #\b)
                                           (char #\c))))

              (test-assert "choice identity"
                           instructions-equal?
                           (choice)
                           fail)

              ;; === Repetition ===
              ;; Π(g, i, p*) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               PartialCommit − |Π(g, x, p)|
              (test-assert "repeat a*"
                           instructions-equal?
                           (repeat (char #\a))
                           (list (encode CHOICE 3 REPEAT)
                                 a
                                 (encode PARTIAL-COMMIT -1)))

              (test-assert "repeat a+"
                           instructions-equal?
                           (repeat+1 (char #\a))
                           (list a
                                 (encode CHOICE 3 REPEAT)
                                 a
                                 (encode PARTIAL-COMMIT -1)))

              (test-assert "repeat, empty nullable"
                           instructions-equal?
                           (repeat (text ""))
                           (list (encode ERROR 'repeat ERROR-NULLABLE)))

              (test-assert "repeat, predicate may be nullable"
                           instructions-equal?
                           (repeat (is? (char #\a)))
                           (list (encode ERROR 'repeat ERROR-NULLABLE)))

              (test-assert "repeat, nested repetitions may be nullable"
                           instructions-equal?
                           (repeat (repeat (char #\a)))
                           (list (encode ERROR 'repeat ERROR-NULLABLE)))

              ;; === Not Predicate ===
              ;; Π(g, i, !p) ≡ Choice |Π(g, x, p)| + 2
              ;;               Π(g, i + 1, p)
              ;;               FailTwice
              (test-assert "predicate !a"
                           instructions-equal?
                           (is-not? (char #\a))
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
                           (is? (char #\a))
                           (list (encode CHOICE 3 IS)
                                 a
                                 (encode BACK-COMMIT 2)
                                 (encode FAIL)))
              ;; === Sets ===
              (test-assert "character set [abc]"
                           instructions-equal?
                           (one-of "abc")
                           (list (encode ONE-OF (make-charset "abbbaac"))))

              (test-assert "character set [^abc]"
                           instructions-equal?
                           (none-of "abc")
                           (list (encode NONE-OF (make-charset "cba"))))

              (test-assert "empty set"
                           instructions-equal?
                           (one-of "")
                           fail)

              (test-assert "universal set"
                           instructions-equal?
                           (none-of "")
                           any)

              ;; === Captures ===
              (test-assert "capture, baseline"
                           instructions-equal?
                           (capture (char #\a))
                           (list (encode CAPTURE-START)
                                 a
                                 (encode CAPTURE-STOP)))

              (test-assert "capture, true positive"
                           instructions-equal?
                           (capture identity (char #\a))
                           (list (encode CAPTURE-START identity)
                                 a
                                 (encode CAPTURE-STOP)))

              (test-assert "capture, false positive, + = identity"
                           instructions-equal?
                           (capture + (char #\a))
                           (list (encode CAPTURE-START identity)
                                 a
                                 (encode CAPTURE-STOP)))

              ;; === Grammars ===
              (test-assert "grammar, baseline"
                           instructions-equal?
                           (grammar [R1 (sequence (text "ab") (call R2))]
                                    [R2 (text "c")])
                           (list (encode GRAMMAR 10 2)
                                 (encode JUMP 9)
                                 (encode RULE 'R1)
                                 a
                                 b
                                 (encode JUMP 7 'R2)
                                 (encode RETURN)
                                 (encode RULE 'R2)
                                 c
                                 (encode RETURN)))

              ;; Left Recursion: A → Bβ such that B ⇒ Aγ
              (test-assert "grammar, direct left recursion"
                           instructions-equal?
                           (grammar [R (call R)])
                           (list (encode ERROR 'R ERROR-LEFT-RECURSION)))

              (test-assert "grammar, indirect left recursion"
                           instructions-equal?
                           (grammar [A (sequence (call B) (char #\x))]
                                    [B (sequence (call C) (char #\y))]
                                    [C (sequence (call A) (char #\z))])
                           (list (encode ERROR 'A ERROR-LEFT-RECURSION))))))
         
         )

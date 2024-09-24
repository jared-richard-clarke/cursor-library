;; === Cursor: PEG Text Matching Library ===
(library (cursor)
         (export empty
                 any
                 character
                 sequence
                 choice
                 maybe
                 repeat
                 is?
                 is-not?
                 one-of
                 none-of
                 grammar
                 call
                 transform
                 text)
         (import (rnrs)
                 (collections charset))

         ;; === constants ===

         (define-syntax enum
           (syntax-rules ()
             [(_ x y ...)
              (begin (define x (quote x))
                     (define y (quote y)) ...)]))

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
               TRANSFORM-START
               TRANSFORM-END
               IS
               IS-NOT
               ONE-OF
               NONE-OF)

         ;; === data ===

         (define-record-type (code encode code?)
           (fields kind op-x op-y)
           (nongenerative)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
               [(kind)           (new kind '() '())]
               [(kind op-x)      (new kind op-x '())]
               [(kind op-x op-y) (new kind op-x op-y)]))))

         ;; === utilities ===

         (define zip-with
           (lambda (fn . xs)
             (apply map fn xs)))

         (define scan-right
           (lambda (fn base xs)
             (if (null? xs)
                 xs
                 (let ([x (fn base (car xs))])
                   (cons x (scan-right fn x (cdr xs)))))))

         (define reduce-right
           (lambda (fn xs)
             (fold-right (lambda (x accum)
                           (if (null? accum)
                               x
                               (fn x accum)))
                         '()
                         xs)))

         (define length-check
           (lambda (x)
             (length (if (list? x)
                         x
                         (list x)))))

         ;; === combinators ===

         ;; fail
         ;;
         ;; Force failure.
         (define fail       (encode FAIL))
         (define fail-twice (encode FAIL-TWICE))

         ;; empty
         ;;
         ;; Always succeed, consuming no input.
         (define empty (encode EMPTY))

         ;; any
         ;;
         ;; Match and consume any character if there is input to be consumed.
         (define any (encode ANY))

         ;; (character x)
         ;;   where x = char
         ;;
         ;; Match character.
         (define character
           (lambda (x)
             (if (char? x)
                 (encode CHARACTER x)
                 (encode ERROR x))))

         ;; (and-then px py)
         ;;
         ;; Match a sequence of two patterns. Acts as flat-map, simultaneously marking encoding errors
         ;; and flattening sequences into a single list of instructions.
         (define and-then
           (lambda (px py)
             (let ([check-code (lambda (x)
                                 (cond [(code? x) (list x)]
                                       [(not (pair? x)) (list (encode ERROR x))]
                                       [(and (pair? x) (not (code? (car x))))
                                        (list (encode ERROR x))]
                                       [else x]))])
               (append (check-code px) (check-code py)))))

         ;; (sequence px py pz ...)
         ;;
         ;; Match a sequence of two or more patterns.
         (define sequence
           (lambda xs
             (reduce-right and-then xs)))

         ;; (ordered-choice px py)
         ;;
         ;; Match one of two patterns. Ordered choice.
         (define ordered-choice
           (lambda (px py)
             (let ([offset-x (length-check px)]
                   [offset-y (length-check py)])
               (sequence (encode CHOICE (+ offset-x 2))
                         px
                         (encode COMMIT (+ offset-y 1))
                         py))))

         ;; (choice px py pz ...)
         ;;
         ;; Match one of two or more patterns.
         (define choice
           (lambda xs
             (reduce-right ordered-choice xs)))

         ;; (maybe px)
         ;;
         ;; Optionally match pattern.
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; (repeat px)
         ;;
         ;; Match pattern zero or more times.
         (define repeat
           (lambda (px)
             (let ([offset (length-check px)])
               (sequence (encode CHOICE (+ offset 2))
                         px
                         (encode PARTIAL-COMMIT (- offset))))))

         ;; (is? px)
         ;;
         ;; Queries match for pattern, consuming no input.
         (define is?
           (lambda (px)
             (let ([offset-x (+ (length-check px) 2)]
                   [offset-y 2])
               (sequence (encode CHOICE offset-x)
                         px
                         (encode BACK-COMMIT offset-y)
                         fail))))

         ;; (is-not? px)
         ;;
         ;; Queries no match for pattern, consuming no input.
         (define is-not?
           (lambda (px)
             (let ([offset (length-check px)])
               (sequence (encode CHOICE (+ offset 2))
                         px
                         fail-twice))))

         ;; (one-of xs)
         ;;   where xs = string
         ;;
         ;; Match a character in a string converted into a set of characters.
         (define one-of
           (lambda (xs)
             (if (string? xs)
                 (encode ONE-OF (make-charset xs))
                 (encode ERROR xs))))

         ;; (none-of xs)
         ;;   where xs = string
         ;;
         ;; Match a character not in a string converted into a set of characters.
         (define none-of
           (lambda (xs)
             (if (string? xs)
                 (encode NONE-OF (make-charset xs))
                 (encode ERROR xs))))

         (define-syntax call
           (syntax-rules ()
             [(_ x)
              (let ([id (quote x)])
                (if (symbol? id)
                    (encode OPEN-CALL id)
                    (encode ERROR id)))]))

         ;; (grammar [rule body]
         ;;          [rule body] ...)
         ;;
         ;; Allows recursive patterns for grammar construction.
         ;; A sequence of one or more rules. The first rule is the start pattern.
         (define-syntax grammar
           (lambda (stx)
             (let ([FIRST-OFFSET 2])
               (syntax-case stx ()
                 [(grammar [rule-x body-x] [rule-y body-y] ...)
                  (with-syntax ([(size-x size-y ...)
                                 (generate-temporaries (syntax (rule-x rule-y ...)))])
                    (syntax (let* ([rule-x  (sequence (encode RULE (quote rule-x)) body-x (encode RETURN))]
                                   [rule-y  (sequence (encode RULE (quote rule-y)) body-y (encode RETURN))]
                                   ...
                                   [size-x  (length-check rule-x)]
                                   [size-y  (length-check rule-y)]
                                   ...
                                   [symbols (quote (rule-x rule-y ...))]
                                   [offsets (zip-with cons symbols (scan-right + FIRST-OFFSET (list 0 size-x size-y ...)))]
                                   [total   (apply + (list size-x size-y ...))]
                                   [rules   (sequence (encode GRAMMAR total)
                                                      (encode CALL (quote rule-x) FIRST-OFFSET)
                                                      (encode JUMP (+ total 1))
                                                      rule-x
                                                      rule-y ...)])
                              (map (lambda (x)
                                     (cond [(and (code? x) (eq? OPEN-CALL (code-kind x)))
                                            (let ([offset (assq (code-op-x x) offsets)])
                                              (if offset
                                                  (encode CALL x (cdr offset))
                                                  x))]
                                           [else x]))
                                   rules))))]))))

         ;; (transform fn px)
         ;;   where fn = function
         ;;         px = pattern
         ;;
         ;; Pattern returns list of character matches to be transformed
         ;; by an arbitrary function.
         (define transform
           (lambda (fn px)
             (sequence (encode TRANSFORM-START fn)
                       px
                       (encode TRANSFORM-END))))

         ;; (audit xs)
         ;;   where xs = pattern instructions
         ;;
         ;; Traverses instruction list, looking for invalid patterns.

         ;; (text xs)
         ;;
         ;; Transforms a string literal into a sequence of character instructions.
         (define text
           (lambda (xs)
             (if (string? xs)
                 (let ([characters (map character (string->list xs))])
                   (apply sequence characters))
                 (encode ERROR xs))))
         )

;; === Cursor: A Parsing Expression Grammar Library ===
;;
;;  --------------------------------------------
;; | Cursor                   | PEG             |
;; |--------------------------+-----------------|
;; | empty                    | ε               |
;; |--------------------------+-----------------|
;; | any                      | .               |
;; |--------------------------+-----------------|
;; | (character #\x)          | "x"             |
;; |--------------------------+-----------------|
;; | (maybe p)                | p?              |
;; |--------------------------+-----------------|
;; | (repeat   p)             | p*              |
;; |--------------------------+-----------------|
;; | (repeat+1 p)             | p+              |
;; |--------------------------+-----------------|
;; | (is? p)                  | &p              |
;; |--------------------------+-----------------|
;; | (is-not? p)              | !p              |
;; |--------------------------+-----------------|
;; | (sequence px py)         | px py           |
;; |--------------------------+-----------------|
;; | (choice   px py)         | px / py         |
;; |--------------------------+-----------------|
;; | (one-of  "xyz")          | [xyz]           |
;; |--------------------------+-----------------|
;; | (none-of "xyz")          | [^xyz]          |
;; |--------------------------+-----------------|
;; | (grammar [rule pattern]) | rule <- pattern |
;;  --------------------------------------------

(library (cursor)
         (export empty
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
                 transform
                 text)
         (import (rnrs)
                 (collections charset))

         ;; === Constants ===

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

         ;; === Data ===

         ;; Instruction Set: (list code code ...)
         ;;   where code = (encode type op-x op-y)

         ;; record-type: code
         ;;
         ;; An instruction. A type symbol followed by two operands for
         ;; holding symbols, sets, offsets, functions, and error messages.
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

         ;; === Tools ===

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

         (define check-length
           (lambda (x)
             (if (list? x)
                 (length x)
                 1)))

         (define check-code
           (lambda (x)
             (cond [(code? x) (list x)]
                   [(not (pair? x))
                    (list (encode ERROR x))]
                   [(and (pair? x) (not (code? (car x))))
                    (list (encode ERROR x))]
                   [else x])))

         ;; === Atoms ===

         ;; fail
         ;;
         ;; Force failure.
         (define fail       (list (encode FAIL)))
         (define fail-twice (list (encode FAIL-TWICE)))

         ;; empty
         ;;
         ;; The empty pattern, which always succeeds and
         ;; consumes no input. Also known as epsilon.
         (define empty (list (encode EMPTY)))

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
                 (list (encode ERROR x)))))

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
                 (encode ERROR xs))))

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
                 (encode ERROR xs))))

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
             (let ([offset (check-length px)])
               (sequence (encode CHOICE (+ offset 2))
                         px
                         (encode PARTIAL-COMMIT (- offset))))))

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
               (sequence (encode CHOICE (+ offset-x 2))
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
               (sequence (encode CHOICE (+ offset 2))
                         px
                         fail-twice))))

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
                 (list (encode ERROR xs)))))

         ;; (none-of xs)
         ;;   where xs = string
         ;;
         ;; Match character not in a set of characters.
         ;; Constructs set from given string.
         (define none-of
           (lambda (xs)
             (if (string? xs)
                 (list (encode NONE-OF (make-charset xs)))
                 (list (encode ERROR xs)))))

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
                    (encode OPEN-CALL id)
                    (encode ERROR id)))]))

         ;; (grammar [rule pattern]
         ;;          [rule pattern] ...)
         ;;
         ;; Allows recursive patterns for grammar construction.
         ;; A sequence of one or more rules. The first rule is
         ;; the start state.
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
                                   [size-x  (check-length rule-x)]
                                   [size-y  (check-length rule-y)]
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
                                     (cond [(and (code? x) (eq? OPEN-CALL (code-type x)))
                                            (let ([offset (assq (code-op-x x) offsets)])
                                              (if offset
                                                  (encode CALL x (cdr offset))
                                                  (encode ERROR x)))]
                                           [else x]))
                                   rules))))]))))

         ;; (transform fn px)
         ;;   where fn = function
         ;;         px = pattern
         ;;
         ;; Pattern returns list of character matches to be
         ;; transformed by an arbitrary function.
         (define transform
           (lambda (fn px)
             (cond [(procedure? fn)
                    (sequence (encode TRANSFORM-START fn)
                              px
                              (encode TRANSFORM-END))]
                   [else
                    (sequence (encode ERROR fn)
                              px
                              (encode ERROR fn))])))

         ;; (audit xs)
         ;;   where xs = pattern instructions
         ;;
         ;; Traverses instruction list, looking for invalid patterns.

         ;; (text xs)
         ;;
         ;; Transforms a string literal into a sequence
         ;; of character instructions.
         (define text
           (lambda (xs)
             (cond [(string? xs)
                    (let ([characters (map character (string->list xs))])
                      (apply sequence characters))]
                   [else (encode ERROR xs)])))
         )

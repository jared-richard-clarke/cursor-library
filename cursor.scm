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
                 transform
                 text)
         (import (rnrs)
                 (collections charset))

         ;; === constants ===
         (define ERROR           'Error)
         (define EMPTY           'Empty)
         (define ANY             'Any)
         (define FAIL            'Fail)
         (define FAIL-TWICE      'Fail-Twice)
         (define CHARACTER       'Character)
         (define SEQUENCE        'Sequence)
         (define CHOICE          'Choice)
         (define COMMIT          'Commit)
         (define PARTIAL-COMMIT  'Partial-Commit)
         (define BACK-COMMIT     'Back-Commit)
         (define GRAMMAR         'Grammar)
         (define RULE            'Rule)
         (define CALL            'Call)
         (define RETURN          'Return)
         (define JUMP            'Jump)
         ;; Transform: Functors and Captures combined.
         (define TRANSFORM-START 'Transform-Start)
         (define TRANSFORM-END   'Transform-End)
         (define LABEL           'Label)
         (define IS              'Is)
         (define IS-NOT          'Is-Not)
         (define ONE-OF          'One-Of)
         (define NONE-OF         'None-Of)

         ;; === data ===
         (define-record-type (code encode code?)
           (fields kind operand)
           (nongenerative)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
               [(kind)         (new kind '())]
               [(kind operand) (new kind operand)]))))

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
         ;; Always succeeds, consuming no input.
         (define empty (encode EMPTY))

         ;; any
         ;;
         ;; Match and consume any character if there is input to be consumed.
         (define any (encode ANY))

         ;; (character x)
         ;;   where x = char
         ;;
         ;; Match character "x".
         (define character
           (lambda (x)
             (if (char? x)
                 (encode CHARACTER x)
                 (encode ERROR x))))

         ;; (and-then px py)
         ;;
         ;; Match two patterns in a sequence.
         (define and-then
           (lambda (px py)
             (let ([check-code (lambda (x)
                                 (cond [(code? x) (list x)]
                                       [(and (pair? x) (not (code? (car x))))
                                        (list (encode ERROR x))]
                                       [else x]))])
               (append (check-code px) (check-code py)))))

         ;; (sequence px py pz ...)
         ;;
         ;; Match two or more patterns in a sequence.
         (define sequence
           (lambda xs
             (reduce-right and-then xs)))

         ;; (ordered-choice px py)
         ;;
         ;; Ordered choice for two patterns.
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
         ;; Ordered choice for two or more patterns.
         (define choice
           (lambda xs
             (reduce-right ordered-choice xs)))

         ;; (maybe px)
         ;;
         ;; Match zero or one patterns.
         (define maybe
           (lambda (px)
             (choice px empty)))

         ;; (repeat px)
         ;;
         ;; Match pattern "px" zero or more times.
         (define repeat
           (lambda (px)
             (let ([offset (length-check px)])
               (sequence (encode CHOICE (+ offset 2))
                         px
                         (encode PARTIAL-COMMIT (- offset))))))

         ;; (is? px)
         ;;
         ;; Queries match for pattern "px", consuming no input.
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
         ;; Queries no match for pattern "px", consuming no input.
         (define is-not?
           (lambda (px)
             (let ([offset (length-check px)])
               (sequence (encode CHOICE (+ offset 2))
                         px
                         fail-twice))))

         ;; (one-of xs)
         ;;   where xs = string
         ;;
         ;; Match a character in string "xs" converted into a set of characters.
         (define one-of
           (lambda (xs)
             (if (string? xs)
                 (encode ONE-OF (make-charset xs))
                 (encode ERROR xs))))

         ;; (none-of xs)
         ;;   where xs = string
         ;;
         ;; Match a character not in string "xs" converted into a set of characters.
         (define none-of
           (lambda (xs)
             (if (string? xs)
                 (encode NONE-OF (make-charset xs))
                 (encode ERROR xs))))

         ;; (grammar [rule body]
         ;;          [rule body] ...)
         ;;
         ;; Allows recursive patterns for grammar construction.
         ;; The first rule is the start pattern.
         (define PLACE-HOLDER 'placeholder)

         (define-syntax grammar
           (lambda (stx)
             (syntax-case stx ()
               [(grammar [rule-x body-x] [rule-y body-y] ...)
                (with-syntax ([(size-x size-y ...)
                               (generate-temporaries (syntax (rule-x rule-y ...)))])
                  (syntax (let ([rule-x (sequence (encode RULE (quote rule-x)) body-x)]
                                [rule-y (sequence (encode RULE (quote rule-y)) body-y)]
                                ...
                                [rules (quote (rule-x rule-y ...))])
                            (let ([size-x (length-check rule-x)]
                                  [size-y (length-check rule-y)]
                                  ...)
                              (let ([offsets (zip-with cons rules (scan-right + 0 (list 0 size-x size-y ...)))])
                                PLACE-HOLDER)))))])))

         ;; (transform fn px)
         ;;   where fn = function
         ;;         px = pattern
         ;;
         ;; Patterns return a list containing sequences of character matches.
         ;; "transform" applies an arbitrary function over that list.
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


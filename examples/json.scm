(library (examples json)
         (export parse-json
                 ;; === record-type: json ===
                 json?           ;; predicate
                 json-elements   ;; field
                 ;; === record-type: object ===
                 object?         ;; predicate
                 object-members  ;; field
                 ;; === record-type: member ===
                 member?         ;; predicate
                 member-key      ;; field
                 member-value    ;; field
                 ;; === record-type: array ===
                 array?          ;; predicate
                 array-elements) ;; field
         (import (rnrs)
                 (cursor))

         ;; === JSON parser ===
         ;; grammar: https://www.json.org/json-en.html

         (define-record-type json
           (fields elements))

         (define-record-type object
           (fields members))

         (define-record-type member
           (fields key value))

         (define-record-type array
           (fields elements))

         (define capture-text
           (lambda (px)
             (capture (lambda (x)
                        (list->string x))
                      px)))

         (define capture-number
           (lambda (px)
             (capture (lambda (x)
                        (string->number (list->string x) 10))
                      px)))

         (define replace
           (lambda (px y)
             (capture (lambda (x) y)
                      px)))

         (define separate-by
           (lambda (px sep)
             (and-then px (repeat (and-then sep px)))))

         (define fullstop
           (lambda (px)
             (and-then px (is-not? any))))
         
         (define whitespace (repeat (one-of " \n\r\t")))

         (define digit  (one-of "0123456789"))
         (define digits (repeat+1 digit))

         (define hex (or-else digit (one-of "ABCDEFabcdef")))

         (define json-true  (text "true"))
         (define json-false (text "false"))
         (define json-null  (text "null"))

         (define json-character  (let* ([control "\"/\b\f\n\r\t\\"]
                                        [escape (or-else (one-of control)
                                                         (and-then (char #\u) hex hex hex hex))])
                                   (or-else (and-then (char #\\) escape)
                                            (none-of control))))
         
         (define json-characters (repeat json-character))

         (define sign
           (maybe (or-else (char #\+) (char #\-))))
         
         (define exponent
           (maybe (and-then (or-else (char #\e)
                                     (char #\E))
                            sign
                            digits)))
         
         (define fraction (maybe (and-then (char #\.) digits)))
         
         (define whole
           (or-else (char #\0)
                    (and-then (one-of "123456789")
                              (repeat digit))))
         
         (define integer (and-then sign whole))

         (define json-number (and-then integer fraction exponent))

         (define json-grammar
           (fullstop
            (grammar [Element (transform make-json
                                         (and-then whitespace
                                                   (rule Value)
                                                   whitespace))]
                    
                     [Value   (or-else (rule Object)
                                       (rule Array)
                                       (rule String)
                                       (rule Number)
                                       (rule True)
                                       (rule False)
                                       (rule Null))]

                     [Object   (transform (lambda (state)
                                            (list (make-object state)))
                                          (and-then (char #\{)
                                                    (or-else (rule Members) whitespace)
                                                    (char #\})))]

                     [Members  (separate-by (rule Member) (char #\,))]

                     [Member   (transform (lambda (state)
                                            (let ([key   (car state)]
                                                  [value (cadr state)])
                                              (cons (make-member key value) (cddr state))))
                                          (and-then whitespace
                                                    (rule String)
                                                    whitespace
                                                    (char #\:)
                                                    (rule Element)))]

                     [Array    (transform (lambda (state)
                                            (list (make-array state)))
                                          (and-then (char #\[)
                                                    (or-else (rule Elements) whitespace)
                                                    (char #\])))]

                     [Elements (separate-by (rule Element) (char #\,))]

                     [String   (and-then (char #\")
                                         (capture-text json-characters)
                                         (char #\"))]

                     [Number   (capture-number json-number)]

                     [True     (replace json-true #t)]

                     [False    (replace json-false #f)]

                     [Null     (replace json-null 'NULL)])))

         (define parse-json (compile json-grammar))

)

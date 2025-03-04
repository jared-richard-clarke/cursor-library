(library (examples json)
         (export parse-json
                 ;; === record-type: json ===
                 json?           ;; predicate
                 json-element    ;; field
                 ;; === record-type: object ===
                 object?         ;; predicate
                 object-members  ;; field
                 ;; === record-type: array ===
                 array?          ;; predicate
                 array-elements  ;; field
                 (rename (tests json:tests)))
         (import (rnrs)
                 (cursor))

         (define-record-type json
           (fields element))

         (define-record-type object
           (fields members))

         (define-record-type array
           (fields elements))

         (define fullstop
           (lambda (px)
             (and-then px (is-not? any))))

         (define separate-by
           (lambda (px sep)
             (and-then px (repeat (and-then sep px)))))

         (define ws (repeat (none-of " \r\n\t")))

         (define replace
           (lambda (px y)
             (capture (lambda (x) y) px)))

         (define capture-text
           (lambda (px)
             (capture (lambda (x) (list->string x))
                      px)))

         (define capture-number
           (lambda (px)
             (capture (lambda (x)
                        (string->number (list->string x)))
                      px)))

         (define control-characters "\"/\b\f\n\r\t\\")
         
         (define digit  (one-of "0123456789"))
         (define digits (repeat+1 digit))
         (define hex    (one-of "0123456789abcdefABCDEF"))

         (define sign        (maybe (char #\-)))
         (define whole       (or-else (char #\0) digits))
         (define integer     (and-then sign whole))
         (define fraction    (maybe (and-then (char #\.) digits)))
         (define exponent    (maybe (and-then (or-else (char #\e)
                                                       (char #\E))
                                              sign
                                              digits)))
         (define real-number (and-then integer fraction exponent))
         
         (define hex-character (and-then (char #\u) hex hex hex hex))

         (define escaped (and-then (char #\\)
                                   (or-else (one-of control-characters)
                                            hex-character)))

         (define build-member
           (lambda (px)
             (transform (lambda (stack)
                          (let ([value (car  stack)]
                                [key   (cadr stack)])
                            (cons (cons key value) (cddr stack))))
                        px)))

         (define collect-with
           (lambda (constructor)
             (lambda (px)
               (transform (lambda (stack)
                            (let loop ([stack      stack]
                                       [collection '()])
                            (cond [(null? stack)
                                   (cons (constructor collection) stack)]
                                  [else
                                   (loop (cdr stack)
                                         (cons (car stack) collection))])))
                          px))))

         (define build-object (collect-with make-object))
         (define build-array  (collect-with make-array))

         (define build-json
           (lambda (px)
             (transform (lambda (stack)
                          (let ([element (car stack)])
                            (make-json element)))
                        px)))

         (define json-grammar
           (build-json
            (grammar [Element (fullstop (and-then ws (rule Value) ws))]
                    
                     [Value (or-else (rule Object)
                                     (rule Array)
                                     (rule String)
                                     (rule Number)
                                     (rule True)
                                     (rule False)
                                     (rule Null))]

                     [Object (and-then (char #\{)
                                       (build-object (or-else (rule Members) ws))
                                       (char #\}))]

                     [Members (separate-by (rule Member) (char #\,))]

                     [Member (build-member
                              (and-then ws (rule String) ws (char #\:) (rule Element)))]

                     [Array (and-then (char #\[)
                                      (build-array (or-else (rule Elements) ws))
                                      (char #\]))]

                     [Elements (separate-by (rule Element) (char #\,))]

                     [String (and-then (char #\")
                                       (capture-text (rule Characters))
                                       (char #\"))]

                     [Characters (repeat (rule Character))]

                     [Character (or-else (none-of control-characters) escaped)]

                     [Number (capture-number real-number)]

                     [True  (replace (text "true") #t)]
                     [False (replace (text "false") #f)]
                     [Null  (replace (text "null") 'null)])))

         (define parse-json (compile json-grammar))

         (define tests
           (test-chunk
            
            "JSON"
            
            ([sample-text
              (call-with-input-file "examples/samples/sample.json"
                (lambda (port) (get-string-all port)))]
             
             [json-equal?
              (lambda (x y)
                (and (json? x) (json? y)
                     (letrec ([traverse-json
                               (lambda (x y)
                                 (cond [(and (object? x) (object? y)
                                             (traverse-lists (object-members x)
                                                             (object-members y)))]
                                       [(and (array? x) (array? y)
                                             (traverse-lists (array-elements x)
                                                             (array-elements y)))]
                                       [(and (pair? x) (pair? y))
                                        (let ([key-x (car x)]
                                              [key-y (car y)])
                                          (and (string? key-x) (string? key-y)
                                               (string=? key-x key-y)
                                               (traverse-json (cdr x) (cdr y))))]
                                       [(and (string? x) (string? y))
                                        (string=? x y)]
                                       [(and (number? x) (number? y))
                                        (= x y)]
                                       [(and (boolean? x) (boolean? y))
                                        (eq? x y)]
                                       [(and (null-symbol? x) (null-symbol? y))
                                        #t]
                                       [else #f]))]
                              [traverse-lists
                               (lambda (x y)
                                 (and (list? x) (list? y)
                                      (= (length x) (length y))
                                      (for-all traverse-json x y)))])
                       (traverse-json (json-element x)
                                      (json-element y)))))]

             [null-symbol?
              (lambda (x)
                (and (symbol x) (eq? x 'null)))])

            (test-assert "sample.json"
                         json-equal?
                         sample-text
                         (make-json
                          (make-object
                           (list (cons "comic books"
                                       (make-array
                                        (list (make-object
                                               (list (cons "title" "Watchmen")
                                                     (cons "authors" (make-array (list "Alan Moore"
                                                                                       "Dave Gibbons")))
                                                     (cons "complete" #t)
                                                     (cons "pages" 414)))
                                              (make-object
                                               (list (cons "title" "Maus")
                                                     (cons "authors" (make-array (list "Art Spiegelman")))
                                                     (cons "complete" #t)
                                                     (cons "pages" 296)))
                                              (make-object
                                               (list (cons "title" "Ghost World")
                                                     (cons "authors" (make-array (list "Daniel Clowes")))
                                                     (cons "complete" #t)
                                                     (cons "pages" 80)))
                                              (make-object
                                               (list (cons "title" "Berserk")
                                                     (cons "authors" (make-array (list "Kentaro Miura")))
                                                     (cons "complete" #f))))))))))
            ))
)

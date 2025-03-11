;; === Grammar: https://www.json.org/json-en.html ===
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
                 (cursor)
                 (cursor tools))

         (define-record-type json
           (fields element))

         (define-record-type object
           (fields members))

         (define-record-type array
           (fields elements))

         (define capture-list
           (lambda (px)
             (let ([base-transform
                    (lambda (px)
                      (transform (lambda (stack)
                                   (cons (list (car stack)) (cdr stack)))
                                 px))]
                   [repeat-transform
                    (lambda (px)
                      (transform (lambda (stack)
                                   (let ([x  (car  stack)]
                                         [xs (cadr stack)])
                                     (cons (cons x xs) (cddr stack))))
                                 px))]
                   [reverse-transform
                    (lambda (px)
                      (transform (lambda (stack)
                                   (cons (reverse (car stack)) (cdr stack)))
                                 px))])
               (reverse-transform
                (and-then (base-transform px)
                          (repeat (and-then (char #\,) (repeat-transform px))))))))

         (define ws (repeat (one-of " \r\n\t")))

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

         (define capture-member
           (lambda (px)
             (transform (lambda (stack)
                          (let ([value (car  stack)]
                                [key   (cadr stack)])
                            (cons (cons key value) (cddr stack))))
                        px)))

         (define build-with
           (lambda (constructor)
             (lambda (px)
               (transform (lambda (stack)
                            (cons (constructor (car stack)) (cdr stack)))
                          px))))

         (define capture-object (build-with make-object))
         (define capture-array  (build-with make-array))

         (define capture-json
           (lambda (px)
             (transform (lambda (stack)
                          (let ([element (car stack)])
                            (make-json element)))
                        px)))

         (define json-grammar
           (capture-json
            (grammar [Element    (and-then ws (rule Value) ws)]
                    
                     [Value      (or-else (rule Object)
                                          (rule Array)
                                          (rule String)
                                          (rule Number)
                                          (rule True)
                                          (rule False)
                                          (rule Null))]

                     [Object     (and-then (char #\{)
                                           (capture-object (or-else (rule Members) ws))
                                           (char #\}))]

                     [Members    (capture-list (rule Member))]

                     [Member     (capture-member
                                  (and-then ws (rule String) ws (char #\:) (rule Element)))]

                     [Array      (and-then (char #\[)
                                           (capture-array (or-else (rule Elements) ws))
                                           (char #\]))]

                     [Elements   (capture-list (rule Element))]

                     [String     (and-then (char #\")
                                           (capture-text (rule Characters))
                                           (char #\"))]

                     [Characters (repeat (rule Character))]

                     [Character  (or-else (none-of control-characters) escaped)]

                     [Number     (capture-number real-number)]

                     [True       (replace (text "true") #t)]
                     [False      (replace (text "false") #f)]
                     [Null       (replace (text "null") 'null)])))

         (define parse-json (compile json-grammar))

         (define tests
           (test-chunk
            
            "JSON"

            ([sample-base
              (call-with-input-file "examples/samples/json/base.json"
                (lambda (port) (get-string-all port)))]
            
             [sample-books
              (call-with-input-file "examples/samples/json/books.json"
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
                (and (symbol? x) (eq? x 'null)))]

             ;; Convenience functions make construction of test data
             ;; both simpler and more readable.
             [json   make-json]
             [object (lambda xs (make-object xs))]
             [field  cons]
             [array  (lambda xs (make-array xs))])

            (test-assert "base.json"
                         json-equal?
                         (parse-json sample-base)
                         (json
                          (object (field "key-1" (array #t #f 'null))
                                  (field "key-2" (object (field "key-3" (array 1 2 3 "3" 1e10 1e-3))))
                                  (field "key-4" 'null))))

            (test-assert "books.json"
                         json-equal?
                         (parse-json sample-books)
                         (json
                          (object (field "comic books"
                                         (array (object (field "title" "Watchmen")
                                                        (field "authors" (array "Alan Moore" "Dave Gibbons"))
                                                        (field "complete" #t)
                                                        (field "pages" 414))
                                                (object (field "title" "Maus")
                                                        (field "authors" (array "Art Spiegelman"))
                                                        (field "complete" #t)
                                                        (field "pages" 296))
                                                (object (field "title" "Ghost World")
                                                        (field "authors" (array "Daniel Clowes"))
                                                        (field "complete" #t)
                                                        (field "pages" 80))
                                                (object (field "title" "Berserk")
                                                        (field "authors" (array "Kentaro Miura"))
                                                        (field "complete" #f)))))))
            
            ))
)

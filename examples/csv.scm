(library (examples csv)
         (export parse-csv
                 ;; === record-type: csv ===
                 csv?        ;; predicate
                 csv-header  ;; field
                 csv-rows    ;; field
                 ;; === record-type: row ===
                 row?        ;; predicate
                 row-columns ;; field
                 (rename (tests csv:tests)))
         (import (rnrs)
                 (cursor)
                 (cursor tools))

         (define-record-type csv
           (fields header rows))

         ;; Side Note: "columns" should be called "fields", but name conflicts
         ;; with the "fields" form of "define-record-type".
         (define-record-type row
           (fields columns))

         (define capture-fields
           (lambda (px)
             (transform (lambda (stack)
                          (let loop ([stack stack]
                                     [row   '()])
                            (cond [(or (null? stack)
                                       (row? (car stack)))
                                   (cons (make-row row) stack)]
                                  [else
                                   (loop (cdr stack)
                                         (cons (car stack) row))])))
                        px)))

         (define capture-rows
           (lambda (px)
             (transform (lambda (stack)
                          (let loop ([stack stack]
                                     [rows  '()])
                            (cond [(null? stack)
                                   (let ([header (car rows)]
                                         [rows   (cdr rows)])
                                     (make-csv header rows))]
                                  [else
                                   (loop (cdr stack)
                                         (cons (car stack) rows))])))
                        px)))

         (define fullstop
           (lambda (px)
             (and-then px (is-not? any))))

         (define separate-by
           (lambda (px sep)
             (and-then px (repeat (and-then sep px)))))

         (define capture-text
           (lambda (px)
             (capture (lambda (x)
                        (list->string x))
                      px)))

         (define csv-grammar
           (fullstop
            (grammar [File   (capture-rows
                              (and-then (rule Header)
                                        (repeat+1 (rule Row))))]
                     
                     [Header (rule Row)]
                     
                     [Row    (capture-fields
                              (and-then (separate-by (rule Field) (char #\,))
                                        (maybe (char #\return))
                                        (char #\newline)))]
                     
                     [Field  (or-else (capture-text (repeat+1 (none-of "\",\n\r")))
                                      (and-then (char #\")
                                                (capture-text (repeat (none-of "\"")))
                                                (char #\"))
                                      empty)])))

         (define parse-csv (compile csv-grammar))

         (define tests
           (test-chunk
            
            "CSV"
            
            ([sample-books
              (call-with-input-file "examples/samples/csv/books.csv"
                (lambda (port) (get-string-all port)))]

             [sample-customers
              (call-with-input-file "examples/samples/csv/customers.csv"
                (lambda (port) (get-string-all port)))]
             
             [row-equal?
              (lambda (x y)
                (and (row? x) (row? y)
                     (equal? (row-columns x)
                             (row-columns y))))]
             
             [rows-equal?
              (lambda (xs ys)
                (and (list? xs) (list? ys)
                     (= (length xs) (length ys))
                     (for-all row-equal? xs ys)))]
             
             [csv-equal?
              (lambda (x y)
                (and (csv? x) (csv? y)
                     (let ([header-x (csv-header x)]
                           [header-y (csv-header y)]
                           [rows-x   (csv-rows x)]
                           [rows-y   (csv-rows y)])
                       (and (row-equal? header-x header-y)
                            (rows-equal? rows-x rows-y)))))]

             ;; Convenience functions make construction of test data
             ;; both simpler and more readable.
             [csv    (lambda (header . rows) (make-csv header rows))]
             [header (lambda xs (make-row xs))]
             [row    (lambda xs (make-row xs))])

            (test-assert "books.csv"
                         csv-equal?
                         (parse-csv sample-books)
                         (csv (header "Book" "Authors" "Complete?" "Pages")
                              (row "Watchmen" "Alan Moore and Dave Gibbons" "true" "414")
                              (row "Maus" "Art Spiegelman" "true" "296")
                              (row "Ghost World" "Daniel Clowes" "true" "80")
                              (row "Berserk" "Kentaro Miura" "false" "unknown")))

            (test-assert "customers.csv"
                         csv-equal?
                         (parse-csv sample-customers)
                         (csv (header "Customer Id" "First Name" "Last Name" "Email" "City" "State")
                              (row "1" "Jason" "Smith" "smith@example.com" "Phoenix" "AR")
                              (row "2" "Nathan" "Nguyen" "nguyen@example.com" "Seattle" "WA")
                              (row "3" "Charles" "Schulz" "schulz@example.com" "Pittsburgh" "PA")
                              (row "4" "Elaine" "Johnson" "johnson@example.com" "Los Angeles" "CA")))

            ))

)

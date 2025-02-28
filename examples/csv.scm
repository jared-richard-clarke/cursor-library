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
                 (cursor))

         (define-record-type csv
           (fields header rows))

         ;; Side Note: "columns" should be called "fields", but name conflicts
         ;; with the "fields" form of "define-record-type".
         (define-record-type row
           (fields columns))

         (define collect-fields
           (lambda (stack)
             (let loop ([stack stack]
                        [row   '()])
               (cond [(or (null? stack)
                          (row? (car stack)))
                      (cons (make-row row) stack)]
                     [else
                      (loop (cdr stack)
                            (cons (car stack) row))]))))

         (define collect-rows
           (lambda (stack)
             (let loop ([stack stack]
                        [rows  '()])
               (cond [(null? stack)
                      (let ([header (car rows)]
                            [rows   (cdr rows)])
                        (make-csv header rows))]
                     [else
                      (loop (cdr stack)
                            (cons (car stack) rows))]))))

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
            (grammar [File   (transform collect-rows
                                        (and-then (rule Header)
                                                  (repeat+1 (rule Row))))]
                     
                     [Header (rule Row)]
                     
                     [Row    (transform collect-fields
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
            
            ([sample-text
              (call-with-input-file "./samples/sample.csv" (lambda (port) (get-string-all port)))]
             
             [row-equal?
              (lambda (x y)
                (and (row? x) (row? y)
                     (equal? (row-columns x)
                             (row-columns y))))]
             
             [rows-equal?
              (lambda (xs ys)
                (for-all row-equal? xs ys))]
             
             [csv-equal?
              (lambda (x y)
                (and (csv? x) (csv? y)
                     (let ([header-x (csv-header x)]
                           [header-y (csv-header y)]
                           [rows-x   (csv-rows x)]
                           [rows-y   (csv-rows y)])
                       (and (row-equal? header-x header-y)
                            (rows-equal? rows-x rows-y)))))])

            (test-assert "sample.csv"
                         csv-equal?
                         (parse-csv sample-text)
                         (make-csv (make-row (list "Book" "Authors" "Complete?" "Pages"))
                                   (list (make-row (list "Watchmen" "Alan Moore and Dave Gibbons" "true" "414"))
                                         (make-row (list "Maus" "Art Spiegelman" "true" "296"))
                                         (make-row (list "Ghost World" "Daniel Clowes" "true" "80"))
                                         (make-row (list "Berserk" "Kentaro Miura" "false")))))

            ))

)

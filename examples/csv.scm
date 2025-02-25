(library (examples csv)
         (export parse-csv
                 ;; === record-type: csv ===
                 csv?         ;; predicate
                 csv-header   ;; field
                 csv-rows     ;; field
                 ;; === record-type: row ===
                 row?         ;; predicate
                 row-columns) ;; field
         (import (rnrs)
                 (cursor))

         (define-record-type csv
           (fields header rows))

         ;; Side Note: "columns" should be called "fields", but name conflicts
         ;; with the "fields" form of "define-record-type".
         (define-record-type row
           (fields columns))

         (define collect-fields
           (lambda (state)
             (let loop ([state state]
                        [row   '()])
               (cond [(or (null? state)
                          (row? (car state)))
                      (cons (make-row (reverse row)) state)]
                     [else
                      (loop (cdr state)
                            (cons (car state) row))]))))

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

         (define characters (repeat+1 (none-of "\",\n\r")))

         (define csv-grammar
           (fullstop
            (grammar [File   (transform (lambda (state)
                                          (make-csv (car state) (cdr state)))
                                        (and-then (rule Header)
                                                  (repeat+1 (rule Row))))]
                     [Header (rule Row)]
                     [Row    (transform collect-fields
                                        (and-then (separate-by (rule Field) (char #\,))
                                                  (maybe (char #\return))
                                                  (char #\newline)))]
                     [Field  (or-else (capture-text characters)
                                      (and-then (char #\")
                                                (capture-text characters)
                                                (char #\")))])))

         (define parse-csv (compile csv-grammar))

)
                                                      

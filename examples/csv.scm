(library (examples csv)
         (export (parse-csv))
         (import (rnrs)
                 (cursor))

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

         (define characters (capture-text (none-of "\",\n\r"))
         (define text       (and-then (char #\") characters (char #\")))

         (define csv
           (fullstop
            (grammar [File (and-then (rule Header)
                                     (repeat+1 (rule Row)))]
                     [Header (rule Row)]
                     [Row    (and-then (separate-by (rule Field) (char #\,))
                                       (maybe (char #\return))
                                       (char #\newline))]
                     [Field  (or-else characters text)])))

         (define parse-csv (compile csv))

)

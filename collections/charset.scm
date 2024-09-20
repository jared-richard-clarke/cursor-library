(library (collections charset)
         (export make-charset
                 charset?
                 charset-has?)
         (import (rnrs))

         ;; === constants ===
         (define TYPE 'charset)
         (define TYPE-ERROR "argument is not a string")

         ;; === record-type: charset ===
         (define-record-type charset
           (fields table)
           (nongenerative)
           (sealed #t)
           (protocol
            (lambda (new)
              (lambda (xs)
                (unless (string? xs)
                  (assertion-violation TYPE TYPE-ERROR xs))
                (let ([characters (string->list xs)]
                      [hashtable  (make-eqv-hashtable)])
                  (new (fold-left (lambda (accum x)
                                    (hashtable-set! accum x #t)
                                    accum)
                                  hashtable
                                  characters)))))))

         (define charset-has?
           (lambda (self key)
             (let ([table (charset-table self)])
               (hashtable-contains? table key))))

         (define charset-size
           (lambda (self)
             (let ([table (charset-table self)])
               (hashtable-size table))))
         )

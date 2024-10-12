(library (cursor collections charset)
         (export make-charset
                 charset?
                 charset-has?
                 charset-equal?
                 (rename (unit-tests charset:unit-tests)))
         (import (rnrs)
                 (cursor tools))

         ;; === constants ===
         (define TYPE 'charset)
         (define ERROR-TYPE-STRING "argument is not a string")

         ;; === record-type: charset ===
         (define-record-type charset
           (fields table)
           (nongenerative)
           (sealed #t)
           (protocol
            (lambda (new)
              (lambda (xs)
                (unless (string? xs)
                  (assertion-violation TYPE ERROR-TYPE-STRING xs))
                (let ([characters (string->list xs)]
                      [hashtable  (make-eqv-hashtable)])
                  (new (fold-left (lambda (accum x)
                                    (hashtable-set! accum x #t)
                                    accum)
                                  hashtable
                                  characters)))))))

         (define charset-size
           (lambda (self)
             (let ([table (charset-table self)])
               (hashtable-size table))))

         (define charset-has?
           (lambda (self key)
             (let ([table (charset-table self)])
               (hashtable-contains? table key))))

         (define character-set-equal?
           (lambda (set-x set-y)
             (let ([tx (charset-table set-x)]
                   [ty (charset-table set-y)])
               (and (= (hashtable-size tx) (hashtable-size ty))
                    (let ([keys (vector->list (hashtable-keys tx))])
                      (for-all (lambda (key) (hashtable-contains? ty key)) keys))))))

         (define unit-tests
           (let ([a #\a]
                 [b #\b]
                 [c #\c]
                 [d #\d]
                 [set-abcd (make-charset "abcd")]
                 [list-eq-set? (lambda (list-x set-y)
                                 (and (= (length list-x) (charset-size set-y))
                                      (for-all (lambda (x) (charset-has? set-y x)) list-x)))])
             (test-chunk
              "Cursor Character Set"
              (test-assert list-eq-set?
                           "ordered list, character set"
                           (list a b c d)
                           set-abcd)
              
              (test-assert list-eq-set?
                           "unordered list, character set"
                           (list c a d b)
                           set-abcd)
              
              (test-assert character-set-equal?
                           "different inputs, same set"
                           set-abcd
                           (make-charset "aabbccddccbbaa")))))

         )

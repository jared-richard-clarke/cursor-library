(library (cursor collections charset)
         (export make-charset
                 charset?
                 charset-union
                 charset-difference
                 charset-has?
                 charset-equal?
                 charset->string
                 (rename (unit-tests charset:unit-tests)))
         (import (rnrs)
                 (cursor tools))

         ;; === record-type: charset ===
         (define-record-type charset
           (fields table)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
               [()
                (new (make-eqv-hashtable))]
               [(xs)
                (unless (string? xs)
                  (assertion-violation 'charset "argument is not a string" xs))
                (let ([characters (string->list xs)]
                      [hashtable  (make-eqv-hashtable)])
                  (new (fold-left (lambda (accum x)
                                    (hashtable-set! accum x #t)
                                    accum)
                                  hashtable
                                  characters)))]))))

         (define charset-size
           (lambda (self)
             (let ([table (charset-table self)])
               (hashtable-size table))))

         ;; A ∪ B = { x ∣ x ∈ A or x ∈ B }
         (define charset-union
           (lambda (set-x set-y)
             (let* ([set-z (make-charset)]
                    [tx    (charset-table set-x)]
                    [ty    (charset-table set-y)]
                    [tz    (charset-table set-z)]
                    [xs    (hashtable-keys tx)]
                    [ys    (hashtable-keys ty)])
               (vector-for-each (lambda (x) (hashtable-set! tz x #t)) xs)
               (vector-for-each (lambda (y) (hashtable-set! tz y #t)) ys)
               set-z)))

         ;; A \ B = { x | x ∈ A and x ∉ B }
         (define charset-difference
           (lambda (set-x set-y)
             (let* ([set-z (make-charset)]
                    [tx    (charset-table set-x)]
                    [ty    (charset-table set-y)]
                    [tz    (charset-table set-z)]
                    [xs    (hashtable-keys tx)])
               (vector-for-each (lambda (x)
                                  (unless (hashtable-contains? ty x)
                                    (hashtable-set! tz x #t)))
                                xs)
               set-z)))

         (define charset-has?
           (lambda (self key)
             (let ([table (charset-table self)])
               (hashtable-contains? table key))))

         (define charset-equal?
           (lambda (set-x set-y)
             (let ([tx (charset-table set-x)]
                   [ty (charset-table set-y)])
               (and (= (hashtable-size tx) (hashtable-size ty))
                    (let ([keys (hashtable-keys tx)])
                      (vector-for-all (lambda (key)
                                        (hashtable-contains? ty key))
                                      keys))))))

         (define charset->string
           (lambda (self)
             (let ([elements (hashtable-keys (charset-table self))]
                   [header   "charset"]
                   [open     #\{]
                   [close    #\}]
                   [space    #\space])
               (let-values ([(port flush) (open-string-output-port)])
                 (begin (put-string port header)
                        (put-char port open)
                        (put-char port space)
                        (vector-for-each (lambda (element)
                                           (if (char-whitespace? element)
                                               (put-datum port element)
                                               (put-char port element))
                                           (put-char port space))
                                         elements)
                        (put-char port close)
                        (flush))))))

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
              (test-assert "ordered list, character set"
                           list-eq-set?
                           (list a b c d)
                           set-abcd)

              (test-assert "unordered list, character set"
                           list-eq-set?
                           (list c a d b)
                           set-abcd)

              (test-assert "different inputs, same set"
                           charset-equal?
                           set-abcd
                           (make-charset "aabbccddccbbaa"))

              (test-assert "set operation, union"
                           charset-equal?
                           set-abcd
                           (charset-union (make-charset "abc")
                                          (make-charset "adc")))

              (test-assert "set operation, difference"
                           charset-equal?
                           set-abcd
                           (charset-difference (make-charset "abcdefg")
                                               (make-charset "efghijk"))))))

         )

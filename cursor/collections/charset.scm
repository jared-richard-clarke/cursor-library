(library (cursor collections charset)
  (export make-charset
          charset?
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
         (let* ([characters (string->list xs)]
                [capacity   (length characters)]
                [hashtable  (make-eqv-hashtable capacity)])
           (new (fold-left (lambda (accum x)
                             (hashtable-set! accum x #t)
                             accum)
                           hashtable
                           characters)))]))))

  (define charset-size
    (lambda (self)
      (let ([table (charset-table self)])
        (hashtable-size table))))

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
        (let-values ([(buffer flush) (string-buffer)])
          (begin (put-string buffer header)
                 (put-char buffer space)
                 (put-char buffer open)
                 (put-char buffer space)
                 (vector-for-each (lambda (element)
                                    (if (char-whitespace? element)
                                        (put-datum buffer element)
                                        (put-char buffer element))
                                    (put-char buffer space))
                                  elements)
                 (put-char buffer close)
                 (flush))))))

  (define unit-tests
    (test-chunk
     "Cursor Character Set"
     ([a #\a]
      [b #\b]
      [c #\c]
      [d #\d]
      [set-abcd (make-charset "abcd")]
      [list-eq-set? (lambda (list-x set-y)
                      (and (= (length list-x) (charset-size set-y))
                           (for-all (lambda (x) (charset-has? set-y x)) list-x)))])

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
                  (make-charset "aabbccddccbbaa"))))
)

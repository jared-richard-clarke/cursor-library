(library (cursor tools)
         (export identity
                 test-assert
                 test-chunk
                 enum
                 zip-with
                 scan-left
                 scan
                 reduce-right
                 vector-fold)
         (import (rnrs))

         (define identity (lambda (x) x))

         (define-syntax thunk
           (syntax-rules ()
             [(_ x y ...)
              (lambda () x y ...)]))

         (define-syntax test-assert
           (syntax-rules ()
             [(_ label compare x y)
              (thunk (let ([computed-x x]
                           [computed-y y])
                       (unless (compare computed-x computed-y)
                         (begin (display (string-append "test " label " failed:"))
                                (newline)
                                (display "lhs: ") (write (quote x)) (display " -> ") (write computed-x) (display ", ")
                                (newline)
                                (display "rhs: ") (write (quote y)) (display " -> ") (write computed-y)
                                (newline)))))]))

         (define test-chunk
           (lambda (label . tests)
             (let ([start (string-append "Begin Test: " label)]
                   [stop  (string-append "End Test: "   label)]
                   [run   (lambda (f) (f))])
               (thunk (display start)
                      (newline)
                      (for-each run tests)
                      (display stop)
                      (newline)))))

         (define-syntax enum
           (syntax-rules ()
             [(_ x y ...)
              (begin (define x (quote x))
                     (define y (quote y)) ...)]))

         (define zip-with
           (lambda (fn xs ys)
             (if (or (null? xs) (null? ys))
                 '()
                 (cons (fn (car xs) (car ys))
                       (zip-with fn (cdr xs) (cdr ys))))))

         (define scan-left
           (lambda (fn base xs)
             (if (null? xs)
                 (cons base xs)
                 (cons base (scan-left fn (fn base (car xs)) (cdr xs))))))

         (define scan
           (lambda (fn xs)
             (scan-left fn (car xs) (cdr xs))))

         (define reduce-right
           (lambda (fn xs)
             (fold-right (lambda (x accum)
                           (if (null? accum)
                               x
                               (fn x accum)))
                         '()
                         xs)))

         (define vector-fold
           (lambda (fn xs)
             (let loop ([index 1]
                        [size  (vector-length xs)]
                        [accum (vector-ref xs 0)])
               (if (>= index size)
                   accum
                   (loop (+ index 1) size (fn accum (vector-ref xs index)))))))

         )

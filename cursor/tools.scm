(library (cursor tools)
         (export test-assert
                 test-chunk
                 enum
                 zip-with
                 scan-right
                 reduce-right)
         (import (rnrs))

         (define-syntax thunk
           (syntax-rules ()
             [(_ x y ...)
              (lambda () x y ...)]))

         (define-syntax test-assert
           (syntax-rules ()
             [(_ compare x y)
              (thunk (let ([computed-x x]
                           [computed-y y])
                       (unless (compare computed-x computed-y)
                         (begin (display "Test failed:")
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

         (define scan-right
           (lambda (fn base xs)
             (if (null? xs)
                 xs
                 (let ([x (fn base (car xs))])
                   (cons x (scan-right fn x (cdr xs)))))))

         (define reduce-right
           (lambda (fn xs)
             (fold-right (lambda (x accum)
                           (if (null? accum)
                               x
                               (fn x accum)))
                         '()
                         xs)))

         )

(library (cursor tools)
         (export assert-test
                 test-chunk
                 enum
                 zip-with
                 scan-right
                 reduce-right)
         (import (rnrs))

         (define-syntax assert-test
           (syntax-rules ()
             [(_ compare x y)
              (let ([computed-x x]
                    [computed-y y])
                (unless (compare computed-x computed-y)
                  (begin (display "Test failed:")
                         (newline)
                         (display "lhs: ") (write (quote x)) (display " -> ") (write computed-x) (display ", ")
                         (newline)
                         (display "rhs: ") (write (quote y)) (display " -> ") (write computed-y)
                         (newline))))]))

         (define-syntax test-chunk
           (syntax-rules ()
             [(_ label x y ...)
              (lambda ()
                (display (string-append "Begin Test: " label))
                (newline)
                x
                y
                ...
                (display (string-append "End Test: "   label))
                (newline))]))

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

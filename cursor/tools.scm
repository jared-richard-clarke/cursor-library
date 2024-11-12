(library (cursor tools)
         (export identity
                 test-assert
                 test-chunk
                 enum
                 zip-with
                 scan-left
                 scan
                 vector-fold
                 vector-for-all
                 peek-map)
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

         (define vector-fold
           (lambda (fn xs)
             (let ([size (vector-length xs)])
               (let loop ([index 1]
                          [accum (vector-ref xs 0)])
                 (if (>= index size)
                     accum
                     (loop (+ index 1) (fn accum (vector-ref xs index))))))))

         (define vector-for-all
           (lambda (fn xs)
             (let ([size (vector-length xs)])
               (let loop ([index 0])
                 (cond [(>= index size) #t]
                       [(fn (vector-ref xs index))
                        (loop (+ index 1))]
                       [else #f])))))

         (define peek-map
           (lambda (fn xs)
             (let ([peek? (lambda (x)
                            (and (pair? x) (pair? (cdr x))))])
               (let recur ([fn fn]
                           [xs xs]
                           [peekable? (peek? xs)])
                 (if (null? xs)
                     xs
                     (let ([x  (car xs)]
                           [xs (cdr xs)])
                       (cons (fn x xs peekable?)
                             (recur fn xs (peek? xs)))))))))

         )

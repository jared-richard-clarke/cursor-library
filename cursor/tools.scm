(library (cursor tools)
         (export identity
                 catch
                 test-assert
                 test-chunk
                 enum
                 iota
                 zip-with
                 zip
                 scan-left
                 scan
                 vector-fold
                 vector-for-all
                 peek-map
                 string-buffer
                 string->vector)
         (import (rnrs))

         ;; (identity x) -> x
         ;;   where x = any
         ;;
         ;; Returns its input unchanged.
         (define identity (lambda (x) x))

         ;; (thunk x y ...) -> (lambda () x y ...)
         ;;   where x = any
         ;;         y = any
         ;;
         ;; Macro wraps one or more expressions in a lambda expression.
         ;; Useful in delaying computations.
         (define-syntax thunk
           (syntax-rules ()
             [(_ x y ...)
              (lambda () x y ...)]))

         ;; (catch y z ...) -> (guard (x [else x]) y z ...)
         ;;   where x = exception object
         ;;         y = any
         ;;         z = any
         ;;
         ;; Macro wraps one or more expressions in a guard expression,
         ;; which catches raised exceptions and returns them as values.
         (define-syntax catch
           (syntax-rules ()
             [(_ y z ...)
              (guard (x [else x]) y z ...)]))

         ;; (test-assert label predicate x y)
         ;;   where label     = string
         ;;         predicate = function
         ;;         x         = any
         ;;         y         = any
         ;;
         ;; Macro compares outputs of expressions "x" and "y" using "predicate". If "predicate"
         ;; returns false, the macro prints the failed expressions to the current output port in both
         ;; their evaluated and unevaluated forms. "label" should be used to identify tests by name
         ;; in the the current output port. Macro wraps test in thunk to delay evaluation.
         ;;
         ;; (define test-thunk (test-assert "commutative" = (- 1 6) (- 6 1)))
         ;;
         ;; (test-thunk) -> test commutative failed:
         ;;                 lhs: (- 1 6) -> -5, rhs: (- 6 1) -> 5
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

         ;; (test-chunk label tests ...)
         ;;   where label = string
         ;;         tests = zero or more functions
         ;;
         ;; Macro calls a sequence of zero or more test thunks, printing failures
         ;; to the current output port. Designed explcitly to run "test-assert".
         ;; "label" should be used to identify test chunk in current output port.
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

         ;; (enum x y ...) -> (begin (define x (quote x))
         ;;                          (define y (quote y)) ...)
         ;;   where x = identifier
         ;;         y = identifier
         ;;
         ;; Macro binds one or more identifiers to their symbols.
         (define-syntax enum
           (syntax-rules ()
             [(_ x y ...)
              (begin (define x (quote x))
                     (define y (quote y)) ...)]))

         ;; (iota count)            -> (list 0 ... (+ 0 (* (- count 1) 1)))
         ;; (iota count start)      -> (list start ... (+ start (* (- count 1) 1)))
         ;; (iota count start step) -> (list start ... (+ start (* (- count 1) step)))
         ;;   where count = number
         ;;         start = number
         ;;         step  = number
         ;;
         ;; Returns a list of length "count" containing a sequence of numbers
         ;; ordered according to "start" and "step".
         ;; The "start" and "step" parameters default to 0 and 1 respectively.
         ;;
         ;; (iota 10)     -> '(0 1 2 3 4  5  6  7  8  9)
         ;; (iota 10 1)   -> '(1 2 3 4 5  6  7  8  9 10)
         ;; (iota 10 1 2) -> '(1 3 5 7 9 11 13 15 17 19)
         (define iota
           (case-lambda
            [(count)
             (iota count 0 1)]
            [(count start)
             (iota count start 1)]
            [(count start step)
             (let loop ([counter count]
                        [result  '()])
               (if (<= counter 0)
                   result
                   (loop (- counter 1)
                         (cons (+ start (* (- counter 1) step)) result))))]))

         ;; (zip-with fn xs ys) -> (list)
         ;;   where fn = function
         ;;         xs = list
         ;;         ys = list
         ;;
         ;; Combines two lists pairwise, using the provided function.
         ;; If one list is shorter than the other, excess elements of
         ;; the longer list are discarded.
         ;;
         ;; (zip-with + '(1 2 3) '(1 2 3)) -> '(2 4 6)
         (define zip-with
           (lambda (fn xs ys)
             (if (or (null? xs) (null? ys))
                 '()
                 (cons (fn (car xs) (car ys))
                       (zip-with fn (cdr xs) (cdr ys))))))

         ;; (zip xs ys) -> (list (x . y) ...)
         ;;   where xs = list
         ;;         ys = list
         ;;
         ;; Combines two lists pairwise. If one list is shorter than the other,
         ;; excess elements of the longer list are discarded.
         ;;
         ;; (zip '(a b c) '(1 2 3)) -> '((a. 1) (b . 2) (c . 3))
         (define zip
           (lambda (xs ys)
             (zip-with cons xs ys)))

         ;; (scan-left fn base xs) -> (list any ...)
         ;;   where fn   = function
         ;;         base = any
         ;;         xs   = list
         ;;
         ;; Combines list elements pairwise, left to right, into an accumulative value
         ;; using the provided binary function. Each combination is consed onto a list.
         ;; A base or starting accumulator must be provided.
         ;;
         ;; (scan-left + 0 '(1 2 3 4)) -> '(0 1 3 6 10)
         (define scan-left
           (lambda (fn base xs)
             (if (null? xs)
                 (cons base xs)
                 (cons base (scan-left fn (fn base (car xs)) (cdr xs))))))

         ;; (scan fn xs) -> (list any ...)
         ;;   where fn = function
         ;;         xs = list
         ;;
         ;; Like "scan-left" but the first element in the list is the base accumulator.
         ;; Consequently, the list must be non-empty.
         ;;
         ;; (scan '(1 2 3 4)) -> '(1 3 6 10)
         (define scan
           (lambda (fn xs)
             (scan-left fn (car xs) (cdr xs))))

         ;; (vector-fold fn xs) -> any
         ;;   where fn = function
         ;;         xs = list
         ;;
         ;; Combines vector elements pairwise, left to right, into an accumulative
         ;; value using the provided binary function. The first element in the
         ;; vector is the base or starting accumulator. Consequently, the list
         ;; must be non-empty.
         ;;
         ;; (vector-fold + '#(1 2 3 4)) -> 10
         (define vector-fold
           (lambda (fn xs)
             (let ([size (vector-length xs)])
               (let loop ([index 1]
                          [accum (vector-ref xs 0)])
                 (if (>= index size)
                     accum
                     (loop (+ index 1) (fn accum (vector-ref xs index))))))))

         ;; (vector-for-all fn xs) -> boolean
         ;;   where fn = function
         ;;         xs = list
         ;;
         ;; Checks if all elements satisfy the given predicate.
         ;;
         ;; (vector-for-all even? '(2 4 10)) -> #t
         (define vector-for-all
           (lambda (fn xs)
             (let ([size (vector-length xs)])
               (let loop ([index 0])
                 (cond [(>= index size) #t]
                       [(fn (vector-ref xs index))
                        (loop (+ index 1))]
                       [else #f])))))

         ;; (peek-map fn xs) -> (list any ...)
         ;;   where fn = (function x xs peekable?)
         ;;                where x         = any
         ;;                      xs        = list
         ;;                      peekable? = boolean
         ;;         xs = list
         ;;
         ;; Like "map" but provides the ability to peek ahead by one element.
         ;; The provided function must take three arguments: the current element,
         ;; the provided list, and the boolean flag "peekable?". Peeking is
         ;; a valid operation only if the flag is set to true.
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

         ;; (string-buffer) -> (values buffer fn)
         ;;                      where buffer = textual output port
         ;;                            fn     = function
         ;;
         ;; Alias for "open-string-output-port", which returns a new textual output port
         ;; and an extraction procedure, which, when called, flushes the port's contents
         ;; into a new string.
         ;;
         ;; (let-values ([(buffer flush) (string-buffer)])
         ;;   (put-string buffer "abc")
         ;;   (put-string buffer "def")
         ;;   (flush))
         ;;
         ;; ->  "abcdef"
         (define string-buffer open-string-output-port)

         ;; (string->vector xs) -> (vector char ...)
         ;;   where xs = string
         ;;
         ;; Transforms a string into a vector of unicode-encoded characters.
         ;;
         ;; (string->vector "abc") -> '#(#\a #\b #\c)
         (define string->vector
           (lambda (xs)
             (list->vector (string->list xs))))

         )

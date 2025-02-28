(library (examples arithmetic)
         (export parse-expression
                 (rename (tests arithmetic:tests)))
         (import (rnrs)
                 (cursor)
                 (cursor tools))

         (define replace
           (lambda (px y)
             (capture (lambda (x) y)
                      px)))

         (define whitespace (repeat (one-of " \n\r\t")))

         (define capture-number
           (lambda (px)
             (capture (lambda (x)
                        (string->number (list->string x)))
                      px)))

         (define binary
           (lambda (px op py)
             (and-then px
                       (repeat
                        (transform (lambda (stack)
                                     (let ([y  (car stack)]
                                           [fn (cadr stack)]
                                           [x  (caddr stack)])
                                       (cons (fn x y) (cdddr stack))))
                                   (and-then op py))))))

         (define digit  (one-of "0123456789"))
         (define digits (repeat+1 digit))
         
         (define sign        (maybe (or-else (char #\-) (char #\+))))
         (define whole       (or-else (char #\0) digits))
         (define integer     (and-then sign whole))
         (define fraction    (maybe (and-then (char #\.) digits)))
         (define exponent    (maybe (and-then (or-else (char #\e)
                                                       (char #\E))
                                              sign
                                              digits)))
         (define real-number (and-then integer fraction exponent))

         (define add      (replace (char #\+) +))
         (define subtract (replace (char #\-) -))
         (define multiply (replace (char #\*) *))
         (define divide   (replace (char #\/) /))
         (define power    (replace (char #\^) expt))

         (define add-sub (or-else add subtract))
         (define mul-div (or-else multiply divide))
         
         (define arithmetic-grammar
           (transform (lambda (stack) (car stack))
                      (grammar [Expression (binary (rule Term) add-sub (rule Term))]
                               [Term       (binary (rule Factor) mul-div (rule Factor))]
                               [Factor     (binary (rule Primary) power (rule Factor))]
                               [Primary    (and-then whitespace
                                                     (or-else (and-then (char #\()
                                                                        (rule Expression)
                                                                        (char #\)))
                                                              (rule Number))
                                                     whitespace)]
                               [Number (capture-number real-number)])))

         (define parse-expression (compile arithmetic-grammar))

         (define tests
           (test-chunk
            "Arithmetic"
            ()
            (test-assert "base"
                         =
                         (parse-expression "1 + 2 * 3 ^ 4")
                         163)

            (test-assert "float"
                         =
                         (parse-expression "0.1 + 0.2")
                         0.3)

            (test-assert "exponent"
                         =
                         (parse-expression "1.2e3 - 1")
                         1199)

            (test-assert "right-associative"
                         =
                         (parse-expression "4 ^ 3 ^ 2")
                         262144)))

)

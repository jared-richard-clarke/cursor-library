(library (examples arithmetic)
         (export parse-expression)
         (import (rnrs)
                 (cursor))

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

         (define chain-left
           (lambda (px op)
             (transform (lambda (state)
                          (letrec ([chain
                                    (lambda (x xs)
                                      (if (null? xs)
                                          x
                                          (let ([fn (car xs)]
                                                [y  (cadr xs)])
                                            (chain (fn x y) (cddr xs)))))])
                            (chain (car state) (cdr state))))
                        (and-then px (repeat (and-then op px))))))

         (define chain-right
           (lambda (px op)
             (transform (lambda (state)
                          (letrec ([chain
                                    (lambda (x xs)
                                      (if (null? xs)
                                          x
                                          (let ([fn (car xs)])
                                            (fn x (chain (cadr xs) (cddr xs))))))])
                            (chain (car state) (cdr state))))
                        (and-then px (repeat (and-then op px))))))

         (define digit    (one-of "0123456789"))
         (define digits   (and-then digit (repeat digit)))
         (define sign     (maybe (or-else (char #\-) (char #\+))))
         (define whole    (or-else (char #\0) (one-of "123456789")))
         (define integer  (and-then sign whole))
         (define fraction (maybe (and-then (char #\.) digits)))
         (define exponent (maybe (and-then (or-else (char #\e)
                                                    (char #\E))
                                            sign
                                            digits)))
         (define real     (capture-number (and-then integer fraction exponent)))

         (define add (replace (char #\+) +))
         (define sub (replace (char #\-) -))
         (define mul (replace (char #\*) *))
         (define div (replace (char #\/) /))
         (define pow (replace (char #\^) expt))

         (define add-sub (or-else add sub))
         (define mul-div (or-else mul div))
         
         (define arithmetic-grammar
           (and-then whitespace
                     (grammar [Expression (chain-left  (rule Term)    add-sub)]
                              [Term       (chain-left  (rule Factor)  mul-div)]
                              [Factor     (chain-right (rule Operand) pow)]
                              [Operand    (or-else (and-then (char #\()
                                                             (rule Expression)
                                                             (char #\)))
                                                   (and-then whitespace real whitespace))])))

         (define parse-expression (compile arithmetic-grammar))

)

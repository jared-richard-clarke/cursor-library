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

         (define separate-by
           (lambda (px op)
             (and-then px (repeat (and-then op px)))))

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

         (define add      (replace (char #\+) +))
         (define subtract (replace (char #\-) -))
         (define multiply (replace (char #\*) *))
         (define divide   (replace (char #\/) /))
         (define power    (replace (char #\^) expt))

         (define add-sub (or-else add subtract))
         (define mul-div (or-else multiply divide))
         
         (define arithmetic-grammar
           (and-then whitespace
                     (grammar [Expression (separate-by (rule Term) add-sub)]
                              [Term       (separate-by (rule Factor) mul-div)]
                              [Factor     (separate-by (rule Operand) power)]
                              [Operand    (or-else (and-then (char #\()
                                                             (rule Expression)
                                                             (char #\)))
                                                   (and-then whitespace real whitespace))])))

         (define parse-expression (compile arithmetic-grammar))

)

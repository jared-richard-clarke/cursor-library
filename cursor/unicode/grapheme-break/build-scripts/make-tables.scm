(import rnrs)

;; === parsers ===

(define matcher
  (lambda (pattern)
    (let ([PATTERN-RESET (string->list pattern)])
      (lambda (string)
       (let ([START 0]
             [STOP (string-length string)]
             [COMMENT-HASH #\#])
         (letrec ([consume
                   (lambda (pattern index)
                     (cond [(null? pattern)
                            #t]
                           [(= index STOP)
                            #f]
                           [else
                            (let ([x (car pattern)]
                                  [y (string-ref string index)])
                              (cond [(char=? y COMMENT-HASH)
                                     #f]
                                    [(char=? x y)
                                     (consume (cdr pattern) (+ index 1))]
                                    [else
                                     (continue (+ index 1))]))]))]
                  [continue
                   (lambda (index)
                     (cond [(= index STOP)
                            #f]
                           [else
                            (consume PATTERN-RESET index)]))])
           (consume PATTERN-RESET START)))))))

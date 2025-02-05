(library (cursor vm)
         (export run-vm)
         (import (rnrs)
                 (cursor data)
                 (cursor tools)
                 (cursor collections charset))

         (define ERROR-VM "unrecognized instruction")

         (define-record-type entry
           (fields ip (mutable sp) (mutable captures))
           (sealed #t))

         (define-record-type capture
           (fields type function offset)
           (sealed #t))

         (define run-vm
           (lambda (text program)
             (let ([size (length text)])
               (letrec ([state
                         (lambda (ip sp stack captures)
                           (let ([code (vector-ref program ip)])
                             (cond [(char? code)
                                    (if (and (< sp size)
                                             (char=? code (vector-ref text sp)))
                                        (state (+ ip 1)
                                               (+ sp 1)
                                               stack
                                               captures)
                                        (fail-state ip sp stack captures))]
                                   [(eq? code ANY)
                                    (if (< sp size)
                                        (state (+ ip 1)
                                               (+ sp 1)
                                               stack
                                               captures)
                                        (fail-state ip sp captures))]
                                   [(eq? code JUMP)
                                    (let ([offset (vector-ref program (+ ip 1))])
                                      (state (+ ip offset)
                                             sp
                                             stack
                                             captures))]
                                   [(eq? code CHOICE)
                                    (let ([offset (vector-ref program (+ ip 1))])
                                      (state (+ ip 2)
                                             sp
                                             (cons (make-entry (+ ip offset) sp captures) stack)
                                             captures))]
                                   [(eq? code COMMIT)
                                    (let ([offset (vector-ref program (+ ip 1))])
                                      (state (+ ip offset)
                                             sp
                                             (cdr stack)
                                             captures))]
                                   [(eq? code PARTIAL-COMMIT)
                                    (let ([offset (vector-ref program (+ ip 1))]
                                          [entry  (car stack)])
                                      (set-entry-sp! entry sp)
                                      (set-entry-captures! entry captures)
                                      (state (+ ip offset)
                                             sp
                                             stack
                                             captures))]
                                   [(eq? code BACK-COMMIT)
                                    (let ([offset (vector-ref program (+ ip 1))]
                                          [entry  (car stack)])
                                      (state (+ ip offset)
                                             (entry-sp entry)
                                             (cdr stack)
                                             (entry-captures entry)))]
                                   [(eq? code FAIL)
                                    (fail-state ip sp stack captures)]
                                   [(eq? code FAIL-TWICE)
                                    (fail-state ip sp (cdr stack) captures)]
                                   [(eq? code CALL)
                                    (let ([offset (vector-ref program (+ ip 1))])
                                      (state (+ ip offset)
                                             sp
                                             (cons (+ ip 2) stack)
                                             captures))]
                                   [(eq? code RETURN)
                                    (state (car stack)
                                           sp
                                           (cdr stack)
                                           captures)]
                                   [(eq? code ONE-OF)
                                    (let ([set (vector-ref program (+ ip 1))])
                                      (if (and (< sp size)
                                               (charset-has? set (vector-ref text sp)))
                                          (state (+ ip 2)
                                                 (+ sp 1)
                                                 stack
                                                 captures)
                                          (fail-state ip sp stack captures)))]
                                   [(eq? code NONE-OF)
                                    (let ([set (vector-ref program (+ ip 1))])
                                      (if (and (< sp size)
                                               (not (charset-has? set (vector-ref text sp))))
                                          (state (+ ip 2)
                                                 (+ sp 1)
                                                 stack
                                                 captures)
                                          (fail-state ip sp stack captures)))]
                                   [(eq? code CAPTURE-START)
                                    (let ([operation (vector-ref program (+ ip 1))])
                                      (state (+ ip 2)
                                             sp
                                             stack
                                             (cons (make-capture CAPTURE-START operation sp) captures)))]
                                   [(eq? code CAPTURE-STOP)
                                    (state (+ ip 1)
                                           sp
                                           stack
                                           (cons CAPTURE-STOP captures))]
                                   [(eq? code MATCH)
                                    (cons sp captures)]
                                   [else
                                    (raise (make-peg-error "virtual machine" code ERROR-VM))])))]
                        [fail-state
                         (lambda (ip sp stack captures)
                           (cond [(null? stack)
                                  FAIL]
                                 [else
                                  (let ([entry (car stack)])
                                    (if (number? entry)
                                        (fail-state ip sp (cdr stack) captures)
                                        (state (entry-ip entry)
                                               (entry-sp entry)
                                               (cdr stack)
                                               (entry-captures entry))))]))])
                 ;; === start virtual machine ===
                 (state 0 0 '() '())))))

         )

(library (cursor vm)
         (export run-vm)
         (import (rnrs)
                 (cursor data)
                 (cursor tools))

         (define-record-type entry
           (fields ip (mutable sp) (mutable captures))
           (sealed #t))

         (define-record-type capture
           (fields type function offset)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
                [(type offset)          (new type '() offset)]
                [(type function offset) (new type function offset)]))))

         (define run-vm
           (lambda (subject instructions)
             (let ([size (vector-length subject)])
               (letrec ([state
                         (lambda (ip sp stack captures)
                           (let ([instruction (vector-ref instructions ip)])
                             (let ([type (code-type instruction)]
                                   [op-x (code-op-x instruction)]
                                   [op-y (code-op-y instruction)])
                               (cond [(eq? type CHARACTER)
                                      (if (and (< sp size)
                                               (char=? op-x (vector-ref subject sp)))
                                          (state (+ ip 1)
                                                 (+ sp 1)
                                                 stack
                                                 captures)
                                          (fail-state ip sp stack captures))]
                                     [(eq? type ANY)
                                      (if (< sp size)
                                          (state (+ ip 1)
                                                 (+ sp 1)
                                                 stack
                                                 captures))]
                                     [(eq? type JUMP)
                                      (state (+ ip op-x)
                                             sp
                                             stack
                                             captures)]
                                     [(eq? type CHOICE)
                                      (state (+ ip 1)
                                             sp
                                             (cons (make-entry (+ ip op-x) sp captures) stack)
                                             captures)]
                                     [(eq? type COMMIT)
                                      (state (+ ip op-x)
                                             sp
                                             (cdr stack)
                                             captures)]
                                     [(eq? type PARTIAL-COMMIT)
                                      (let ([entry (car stack)])
                                        (begin (set-entry-sp! entry sp)
                                               (set-entry-captures! entry captures)
                                               (state (+ ip op-x)
                                                      sp
                                                      stack
                                                      captures)))]
                                     [(eq? type BACK-COMMIT)
                                      (let ([entry (car stack)])
                                        (state (+ ip op-x)
                                               (entry-sp entry)
                                               (cdr stack)
                                               (entry-captures entry)))]
                                     [(eq? type FAIL)
                                      (fail-state ip
                                                  sp
                                                  stack
                                                  captures)]
                                     [(eq? type FAIL-TWICE)
                                      (fail-state ip
                                                  sp
                                                  (cdr stack)
                                                  captures)]
                                     [(or (eq? type CALL)
                                          (eq? type GRAMMAR))
                                      (state op-y
                                             sp
                                             (cons (+ ip 1) stack)
                                             captures)]
                                     [(eq? type RETURN)
                                      (state (car stack)
                                             sp
                                             (cdr stack)
                                             captures)]
                                     [(eq? type ONE-OF)
                                      (if (and (< sp size)
                                               (hashtable-contains? op-x (vector-ref subject sp)))
                                          (state (+ ip 1)
                                                 (+ sp 1)
                                                 stack
                                                 captures)
                                          (fail-state ip sp stack captures))]
                                     [(eq? type NONE-OF)
                                      (if (and (< sp size)
                                               (not (hashtable-contains? op-x (vector-ref subject sp))))
                                          (state (+ ip 1)
                                                 (+ sp 1)
                                                 stack
                                                 captures)
                                          (fail-state ip sp stack captures))]
                                     [(eq? type CAPTURE-START)
                                      (state (+ ip 1)
                                             sp
                                             stack
                                             (cons (make-capture CAPTURE-START op-y sp) captures))]
                                     [(eq? type CAPTURE-STOP)
                                      (state (+ ip 1)
                                             sp
                                             stack
                                             (cons (make-capture CAPTURE-STOP sp) captures))]
                                     [(eq? type MATCH)
                                      (cons sp captures)]))))]
                        [fail-state
                         (lambda (ip sp stack captures)
                           (if (null? stack)
                               0
                               (let ([entry (car stack)])
                                 (if (number? entry)
                                     (fail-state ip
                                                 sp
                                                 (cdr stack)
                                                 captures)
                                     (state (entry-ip entry)
                                            (entry-sp entry)
                                            (cdr stack)
                                            (entry-captures entry))))))])
                 ;; === start virtual machine ===
                 (state 0 0 '() '())))))

         )

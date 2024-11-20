(library (cursor vm)
         (export run-vm)
         (import (rnrs)
                 (cursor data)
                 (cursor tools))

         (define-record-type entry
           (fields ip
                   (mutable sp)
                   stack
                   captures))

         (define-record-type capture
           (fields type
                   procedure
                   offset)
           (protocol
            (lambda (new)
              (case-lambda
               [(type offset)           (new type '() offset)]
               [(type procedure offset) (new type procedure offset)]))))

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
                                          ;; === todo ===
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
                                               (state (+ ip op-x)
                                                      sp
                                                      stack
                                                      captures)))]
                                     [(eq? type CALL)
                                      (state (+ ip op-y)
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
                                          ;; === todo ===
                                          (fail-state ip sp stack captures))]
                                     [(eq? type NONE-OF)
                                      (if (and (< sp size)
                                               (not (hashtable-contains? op-x (vector-ref subject sp))))
                                          (state (+ ip 1)
                                                 (+ sp 1)
                                                 stack
                                                 captures)
                                          ;; === todo ===
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
                                             (cons (make-capture CAPTURE-STOP sp) captures))]))))]
                        [fail-state
                         (lambda (ip sp stack captures)
                           'todo)])
                 ;; === start virtual machine ===
                 (state 0 0 '() '()))))))

         )

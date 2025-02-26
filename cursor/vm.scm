(library (cursor vm)
         (export run-vm)
         (import (rnrs)
                 (cursor data)
                 (cursor tools)
                 (cursor collections charset))

         (define ERROR-VM "unrecognized instruction")

         (define-record-type entry
           (fields ip (mutable sp) (mutable captures)))

         (define-record-type capture
           (fields type function offset))

         (define run-vm
           (lambda (text program)
             (let ([size (vector-length text)])
               (letrec ([state
                         (lambda (ip sp stack captures)
                           (let ([code (vector-ref program ip)])
                             (cond
                              ;; [(char? x), text[sp] = x]
                              ;; (ip, sp, stack, captures) -> (ip+1, sp+1, stack, captures)
                              ;;
                              ;; [(char? x), text[sp] ≠ x]
                              ;; (ip, sp, stack, captures) -> (fail, sp, stack, captures)
                              [(char? code)
                               (if (and (< sp size)
                                        (char=? code (vector-ref text sp)))
                                   (state (+ ip 1)
                                          (+ sp 1)
                                          stack
                                          captures)
                                   (fail-state ip sp stack captures))]
                              ;; [EMPTY]
                              ;; (ip, sp, stack, captures) -> (ip+1, sp, stack, captures)
                              [(eq? code EMPTY)
                               (state (+ ip 1)
                                      sp
                                      stack
                                      captures)]
                              ;; [ANY, sp ≤ |text|]
                              ;; (ip, sp, stack, captures) -> (ip+1, sp+1, stack, captures)
                              ;;
                              ;; [ANY, sp > |text|]
                              ;; (ip, sp, stack, captures) -> (fail, sp, stack, captures)
                              [(eq? code ANY)
                               (if (< sp size)
                                   (state (+ ip 1)
                                          (+ sp 1)
                                          stack
                                          captures)
                                   (fail-state ip sp stack captures))]
                              ;; [JUMP offset]
                              ;; (ip, sp, stack, captures) -> (ip+offset, sp, stack, captures)
                              [(eq? code JUMP)
                               (let ([offset (vector-ref program (+ ip 1))])
                                 (state (+ ip offset)
                                        sp
                                        stack
                                        captures))]
                              ;; [CHOICE offset]
                              ;; (ip, sp, stack, captures) -> (ip+2, sp, (ip+offset, sp, captures):stack, captures)
                              [(eq? code CHOICE)
                               (let ([offset (vector-ref program (+ ip 1))])
                                 (state (+ ip 2)
                                        sp
                                        (cons (make-entry (+ ip offset) sp captures) stack)
                                        captures))]
                              ;; [COMMIT offset]
                              ;; (ip, sp, entry:stack, captures) -> (ip+offset, sp, stack, captures)
                              [(eq? code COMMIT)
                               (let ([offset (vector-ref program (+ ip 1))])
                                 (state (+ ip offset)
                                        sp
                                        (cdr stack)
                                        captures))]
                              ;; [PARTIAL-COMMIT offset]
                              ;; (ip₂, sp₂, (ip₁, sp₁, captures₁):stack, captures₂)
                              ;; ->
                              ;; (ip₂+offset, sp₂, (ip₁, sp₂, captures₂):stack, captures₂)
                              [(eq? code PARTIAL-COMMIT)
                               (let ([offset (vector-ref program (+ ip 1))]
                                     [entry  (car stack)])
                                 (entry-sp-set! entry sp)
                                 (entry-captures-set! entry captures)
                                 (state (+ ip offset)
                                        sp
                                        stack
                                        captures))]
                              ;; [BACK-COMMIT offset]
                              ;; (ip₂, sp₂, (ip₁, sp₁, captures₁):stack, captures₂)
                              ;; ->
                              ;; (ip₂+offset, sp₁, stack, captures₁)
                              [(eq? code BACK-COMMIT)
                               (let ([offset (vector-ref program (+ ip 1))]
                                     [entry  (car stack)])
                                 (state (+ ip offset)
                                        (entry-sp entry)
                                        (cdr stack)
                                        (entry-captures entry)))]
                              ;; [FAIL]
                              ;; (fail, sp, number:stack, captures)                 -> (fail, sp, stack, captures)
                              ;; (fail, sp₂, (ip, sp₁, captures₁):stack, captures₂) -> (ip, sp₁, stack, captures₁)
                              [(eq? code FAIL)
                               (fail-state ip sp stack captures)]
                              ;; [FAIL-TWICE]
                              ;; (ip, sp, entry:stack, captures) -> (fail, sp, stack captures)
                              [(eq? code FAIL-TWICE)
                               (fail-state ip sp (cdr stack) captures)]
                              ;; [CALL offset]
                              ;; (ip, sp, stack, captures) -> (ip+offset, sp, (ip+2):stack, captures)
                              [(eq? code CALL)
                               (let ([offset (vector-ref program (+ ip 1))])
                                 (state (+ ip offset)
                                        sp
                                        (cons (+ ip 2) stack)
                                        captures))]
                              ;; [RETURN]
                              ;; (ip₂, sp, ip₁:stack, captures) -> (ip₁, sp, stack, captures)
                              [(eq? code RETURN)
                               (state (car stack)
                                      sp
                                      (cdr stack)
                                      captures)]
                              ;; [ONE-OF charset, text[sp] ∈ charset]
                              ;; (ip, sp, stack, captures) -> (ip+2, sp+1, stack, captures)
                              ;;
                              ;; [ONE-OF charset, text[sp] ∉ charset]
                              ;; (ip, sp, stack, captures) -> (fail, sp, stack, captures)
                              [(eq? code ONE-OF)
                               (let ([set (vector-ref program (+ ip 1))])
                                 (if (and (< sp size)
                                          (charset-has? set (vector-ref text sp)))
                                     (state (+ ip 2)
                                            (+ sp 1)
                                            stack
                                            captures)
                                     (fail-state ip sp stack captures)))]
                              ;; [NONE-OF charset, text[sp] ∈ charset]
                              ;; (ip, sp, stack, captures) -> (fail, sp, stack, captures)
                              ;;
                              ;; [NONE-OF charset, text[sp] ∉ charset]
                              ;; (ip, sp, stack, captures) -> (ip+2, sp+1, stack, captures)
                              [(eq? code NONE-OF)
                               (let ([set (vector-ref program (+ ip 1))])
                                 (if (and (< sp size)
                                          (not (charset-has? set (vector-ref text sp))))
                                     (state (+ ip 2)
                                            (+ sp 1)
                                            stack
                                            captures)
                                     (fail-state ip sp stack captures)))]
                              ;; [CAPTURE-START operation]
                              ;; (ip, sp, stack, captures) -> (ip+2, sp, stack, (CAPTURE-START, operation, sp):captures)
                              [(eq? code CAPTURE-START)
                               (let ([operation (vector-ref program (+ ip 1))])
                                 (state (+ ip 2)
                                        sp
                                        stack
                                        (cons (make-capture CAPTURE-START operation sp) captures)))]
                              ;; [CAPTURE-STOP]
                              ;; (ip, sp, stack, captures) -> (ip+1, sp, stack, (CAPTURE-STOP, (- sp 1)):captures)
                              [(eq? code CAPTURE-STOP)
                               (state (+ ip 1)
                                      sp
                                      stack
                                      (cons (make-capture CAPTURE-STOP '() (- sp 1)) captures))]
                              ;; [TRANSFORM operation]
                              ;; (ip, sp, stack, captures) -> (ip+2, sp, stack, (TRANSFORM, operation):captures)
                              [(eq? code TRANSFORM)
                               (let ([operation (vector-ref program (+ ip 1))])
                                 (state (+ ip 2)
                                        sp
                                        stack
                                        (cons (make-capture TRANSFORM operation 0) captures)))]
                              ;; [MATCH]
                              ;; (ip, sp, stack, captures) -> boolean | any
                              [(eq? code MATCH)
                               (if (null? captures)
                                   #t
                                   (collect-captures captures text))]
                              ;; undefined operation -> raise peg-error
                              [else
                               (raise (make-peg-error "virtual machine" code ERROR-VM))])))]
                        ;; Handles logic for when the virtual machine enters a failing state.
                        [fail-state
                         (lambda (ip sp stack captures)
                           (cond
                            ;; If the stack is empty, then the match has failed.
                            [(null? stack)
                             #f]
                            [else
                             (let ([entry (car stack)])
                               (if (number? entry)
                                   ;; Remove pending calls.
                                   ;; (fail, sp, number:stack, captures) -> (fail, sp, stack, captures)
                                   (fail-state ip sp (cdr stack) captures)
                                   ;; Set state to entry on top of stack.
                                   ;; (fail, sp₂, (ip, sp₁, captures₁):stack, captures₂) -> (ip, sp₁, stack, captures₁)
                                   (state (entry-ip entry)
                                          (entry-sp entry)
                                          (cdr stack)
                                          (entry-captures entry))))]))])
                 ;; === run-vm: start state ===
                 (state 0 0 '() '())))))

         (define collect-captures
           (lambda (captures text)
             (let ([captures (reverse captures)])
               (letrec ([state
                         (lambda (stack-1 stack-2 accumulator)
                           (cond [(null? stack-1)
                                  accumulator]
                                 [else
                                  (let ([capture (car stack-1)])
                                    (cond [(eq? (capture-type capture) CAPTURE-STOP)
                                           (let ([head (car stack-2)]
                                                 [tail capture])
                                             (let ([function (capture-function head)]
                                                   [start    (capture-offset head)]
                                                   [stop     (capture-offset tail)])
                                               (state (cdr stack-1)
                                                      (cdr stack-2)
                                                      (collect function start stop accumulator))))]
                                          [(eq? (capture-type capture) TRANSFORM)
                                           (let ([function (capture-function capture)])
                                             (state (cdr stack-1)
                                                    stack-2
                                                    (function accumulator)))]
                                          [else
                                           (state (cdr stack-1)
                                                  (cons (car stack-1) stack-2)
                                                  accumulator)]))]))]
                        [collect
                         (lambda (function start stop accumulator)
                           (let loop ([index      stop]
                                      [characters '()])
                             (cond [(>= index start)
                                    (loop (- index 1)
                                          (cons (vector-ref text index) characters))]
                                   [else
                                    (if (null? function)
                                        (cons characters accumulator)
                                        (cons (function characters) accumulator))])))])
                 ;; === collect-captures: start state ===
                 (state captures '() '())))))

)

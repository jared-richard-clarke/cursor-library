(library (cursor vm)
         (export run-vm)
         (import (rnrs)
                 (cursor data)
                 (cursor tools)
                 (cursor collections charset))

         ;; === Error Constants ===

         (define ERROR-VM         "unrecognized instruction in virtual machine")
         (define ERROR-VM-COLLECT "unrecognized instruction in virtual machine's collect captures phase")
         (define ERROR-CAPTURE    "provided function must operate over string")
         (define ERROR-TRANSFORM  "provided function pushes transformation into invalid state")

         ;; === VM Data Structures ===

         ;; record: (entry ip sp captures)
         ;;   where ip       = number: instruction index
         ;;         sp       = number: string index
         ;;         captures = (list capture): capture list state
         ;;
         ;; Stack entry used to save states wihin the virtual machine.
         ;; Allows backtracking.
         (define-record-type entry
           (fields ip
                   (mutable sp)
                   (mutable captures)))

         ;; record: (capture type function offset)
         ;;   where type     = symbol: CAPTURE-START | CAPTURE-STOP | TRANSFORM-START | TRANSFORM-STOP
         ;;         function = procedure: the function of the associated capture or transform.
         ;;         offset   = number: the string index of a successful capture.
         ;;
         ;; Element of captures list. Used to extract captures and process
         ;; transformations after a successful match.
         (define-record-type capture
           (fields type
                   function
                   offset))

         ;; record: (call-frame arguments stack)
         ;;  where arguments = (list any): the arguments list of the current call frame.
         ;;        stack     = call-frame
         ;;
         ;; Builds the arguments list for the current transform and tracks the call frame
         ;; that the current call frame must return to after it finishes executing.
         ;; The recursively-defined call-frame is similar to a cons cell. Whereas a series
         ;; of cons cells create a linked list, a series of call frames create a call stack.
         (define-record-type call-frame
           (fields (mutable arguments)
                   stack))

         ;; The terminus of a call stack. Similar to '() of a proper list.
         (define CALL-STACK-BASE (make-call-frame '() '()))

         ;; (call-stack-empty? call-frame) -> boolean
         ;;
         ;; Predicate function returns boolean true if the call-frame is the empty call frame.
         ;; Returns boolean false otherwise. Similar to the predicate function "null?".
         (define call-stack-empty?
           (lambda (call-frame)
             (eq? call-frame CALL-STACK-BASE)))

         ;; (run-vm text program) -> boolean | string | (list string) | any | (list any)
         ;;   where text    = string
         ;;         program = (vector instruction)
         ;;
         ;; Executes the given virtual machine program over the given text.
         ;; The semantics of this virtual machine are similar to the semantics of the LPeg
         ;; virtual machine by Sérgio Medeiros and Roberto Ierusalimschy. However,
         ;; whereas state  manipulation is implicit in the "for" loops and "goto"
         ;; statements of LPeg, state manipulation is explicit in the tail-call-based,
         ;; continuation-passing style of Cursor.
         (define run-vm
           (lambda (text program)
             (let ([size (string-length text)])
               ;; (state ip sp stack captures) -> (state ip sp stack captures) | (fail-state ip sp stack captures) | result
               ;;   where ip       = number: instruction index
               ;;         sp       = number: string index
               ;;         stack    = (list (entry | number)): saved states
               ;;         captures = (list capture): captures list
               ;;         result   = #t | string | (list string) | any | (list any): return value
               ;;
               ;; Moves the virtual machine forward, passing the VM state from one tail call to the next.
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
                                        (char=? code (string-ref text sp)))
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
                              ;; [ANY, sp < |text|]
                              ;; (ip, sp, stack, captures) -> (ip+1, sp+1, stack, captures)
                              ;;
                              ;; [ANY, sp ≥ |text|]
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
                                          (charset-has? set (string-ref text sp)))
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
                                          (not (charset-has? set (string-ref text sp))))
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
                              ;; (ip, sp, stack, captures) -> (ip+1, sp, stack, (CAPTURE-STOP, sp):captures)
                              [(eq? code CAPTURE-STOP)
                               (state (+ ip 1)
                                      sp
                                      stack
                                      (cons (make-capture CAPTURE-STOP '() sp) captures))]
                              ;; [TRANSFORM-START operation]
                              ;; (ip, sp, stack, captures) -> (ip+2, sp, stack, (TRANSFORM-START, operation):captures)
                              [(eq? code TRANSFORM-START)
                               (let ([operation (vector-ref program (+ ip 1))])
                                 (state (+ ip 2)
                                        sp
                                        stack
                                        (cons (make-capture TRANSFORM-START operation 0) captures)))]
                              ;; [TRANSFORM-STOP]
                              ;; (ip, sp, stack, captures) -> (ip+1, sp, stack, TRANSFORM-STOP:captures)
                              [(eq? code TRANSFORM-STOP)
                               (state (+ ip 1)
                                      sp
                                      stack
                                      (cons (make-capture TRANSFORM-STOP '() 0) captures))]
                              ;; [MATCH]
                              ;; (ip, sp, stack, captures) -> boolean | string | (list string) | any | (list any)
                              [(eq? code MATCH)
                               (if (null? captures)
                                   #t
                                   (collect-captures captures text))]
                              ;; undefined operation -> raise error
                              [else
                               (peg-error "virtual machine" ERROR-VM (list code))])))]
                        ;; (fail-state ip sp stack captures) -> (state ip sp stack captures) | (fail-state ip sp stack captures) | #f
                        ;;
                        ;; Handles logic for when the VM enters a failing state.
                        [fail-state
                         (lambda (ip sp stack captures)
                           (cond
                            ;; If the stack is empty, all options have been exhausted
                            ;; and the match has failed.
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
                 ;;
                 ;; (state ip sp stack captures)
                 (state 0 0 '() '())))))

         ;; (collect-captures captures text) -> boolean | string | (list string) | any | (list any)
         ;;   where captures = (list capture)
         ;;         text     = string
         ;;
         ;; Using the captures list and associated text, collect-captures extracts
         ;; and transforms the substrings of a successful match.
         (define collect-captures
           (lambda (captures text)
             ;; (state stack-x stack-y call-stack accumulator) -> (state stack-x stack-y call-stack accumulator) | result
             ;;   where stack-x     = (list capture)
             ;;         stack-y     = (list capture)
             ;;         call-stack  = call-frame
             ;;         accumulator = (list any)
             ;;         result      = string | (list string) | any | (list any)
             ;;
             ;; Moves "collect-captures" forward, passing state from one tail call to the next.
             ;;
             ;; === Internals ===
             ;;
             ;; - stack-x and stack-y: unpack the captures list for substring extraction.
             ;; - call-stack: tracks data for call frames, collecting captures, applying
             ;;   transformations, and maintaining scope.
             ;; - accumulator: collects all the transformations and captures that will
             ;;   eventually be returned as output for "collect-captures".
             (letrec ([state
                       (lambda (stack-x stack-y call-stack accumulator)
                         (cond [(null? stack-x)
                                (if (singleton? accumulator)
                                    (car accumulator)
                                    accumulator)]
                               [else
                                (let ([capture (car stack-x)]
                                      [type    (capture-type (car stack-x))])
                                  (cond [(eq? type CAPTURE-STOP)
                                         (state (cdr stack-x)
                                                (cons capture stack-y)
                                                call-stack
                                                accumulator)]
                                        [(eq? type CAPTURE-START)
                                         (let ([head capture]
                                               [tail (car stack-y)])
                                           (let ([function (capture-function head)]
                                                 [start    (capture-offset   head)]
                                                 [stop     (capture-offset   tail)])
                                             (let ([result (apply-capture function text start stop)])
                                               (cond [(call-stack-empty? call-stack)
                                                      (state (cdr stack-x)
                                                             (cdr stack-y)
                                                             call-stack
                                                             (cons result accumulator))]
                                                     [else
                                                      (let ([arguments (call-frame-arguments call-stack)])
                                                        (call-frame-arguments-set! call-stack (cons result arguments))
                                                        (state (cdr stack-x)
                                                               (cdr stack-y)
                                                               call-stack
                                                               accumulator))]))))]
                                        [(eq? type TRANSFORM-STOP)
                                         (state (cdr stack-x)
                                                stack-y
                                                (make-call-frame '() call-stack)
                                                accumulator)]
                                        [(eq? type TRANSFORM-START)
                                         (let ([function  (capture-function     capture)]
                                               [arguments (call-frame-arguments call-stack)]
                                               [previous  (call-frame-stack     call-stack)])
                                           (let ([result (apply-transform function arguments)])
                                             (cond [(call-stack-empty? previous)
                                                    (state (cdr stack-x)
                                                           stack-y
                                                           previous
                                                           (append result accumulator))]
                                                   [else
                                                    (let ([arguments (call-frame-arguments previous)])
                                                      (call-frame-arguments-set! previous (append result arguments))
                                                      (state (cdr stack-x)
                                                             stack-y
                                                             previous
                                                             accumulator))])))]
                                        [else
                                         (peg-error "virtual machine"
                                                    ERROR-VM-COLLECT
                                                    (list type))]))]))]
                      ;; (apply-capture function text start stop) -> string | any | raise exception
                      ;;   where function = (string) -> any | '()
                      ;;         text     = string
                      ;;         start    = number: start index
                      ;;         stop     = number: stop index
                      ;;
                      ;; Extracts substrings from text, and, if a function is supplied,
                      ;; applies that function to the extracted string. Will raise an
                      ;; exception if the provided function raises an exception.
                      [apply-capture
                       (lambda (function text start stop)
                         (let ([segment (substring text start stop)])
                           (if (null? function)
                               segment
                               (guard (context [(peg-violation? context)
                                                (raise context)]
                                               [else
                                                (peg-error (string-append "capture: " (datum->string function))
                                                           ERROR-CAPTURE
                                                           (list segment)
                                                           context)])
                                      (function segment)))))]
                      ;; (apply-transform function arguments) -> any | raise exception
                      ;;   where function  = (capture ...) -> (capture ...)
                      ;;         arguments = (list any)
                      ;;
                      ;; Applies function to list of arguments. Will raise an exception if function
                      ;; parameters do not match arguments list or an exception is raised within
                      ;; the function itself.
                      [apply-transform
                       (lambda (function arguments)
                         (call-with-values
                             (lambda ()
                               (guard (context [(peg-violation? context)
                                                (raise context)]
                                               [else
                                                (peg-error (string-append "transform: " (datum->string function))
                                                           ERROR-TRANSFORM
                                                           arguments
                                                           context)])
                                      (apply function arguments)))
                           list))])
               ;; === collect-captures: start state ===
               ;;
               ;; (state stack-x stack-y call-stack accumulator)
               (state captures '() CALL-STACK-BASE '()))))

)

(library (cursor data)
         ;; enumerations
         (export ERROR
                 EMPTY
                 ANY
                 FAIL
                 FAIL-TWICE
                 CHARACTER
                 SEQUENCE
                 CHOICE
                 COMMIT
                 PARTIAL-COMMIT
                 BACK-COMMIT
                 GRAMMAR
                 RULE
                 CALL
                 OPEN-CALL
                 TAIL-CALL
                 RETURN
                 JUMP
                 CAPTURE-START
                 CAPTURE-STOP
                 REPEAT
                 IS
                 IS-NOT
                 ONE-OF
                 NONE-OF
                 MATCH
         ;; constants
                 MAX-RULES
         ;; record-type: code
                 encode     ;; constructor
                 code?      ;; predicate
                 code-type  ;; field
                 code-op-x  ;; field
                 code-op-y  ;; field
         ;; record-type: &peg-error -> &condition
                 make-peg-error     ;; constructor
                 peg-error?         ;; predicate
                 peg-error-who      ;; field
                 peg-error-what     ;; field
                 peg-error-why)     ;; field
         (import (rnrs)
                 (cursor tools))

         (enum ERROR
               EMPTY
               ANY
               FAIL
               FAIL-TWICE
               CHARACTER
               SEQUENCE
               CHOICE
               COMMIT
               PARTIAL-COMMIT
               BACK-COMMIT
               GRAMMAR
               RULE
               CALL
               OPEN-CALL
               TAIL-CALL
               RETURN
               JUMP
               CAPTURE-START
               CAPTURE-STOP
               REPEAT
               IS
               IS-NOT
               ONE-OF
               NONE-OF
               MATCH)

         (define MAX-RULES 1000)

         ;; Instruction Set: (list code code ...)
         ;;   where code = (encode type op-x op-y)

         ;; record-type: code
         ;; An instruction containing a type identifier followed by two operands.
         (define-record-type (code encode code?)
           (fields type op-x op-y)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
                [(type)           (new type '() '())]
                [(type op-x)      (new type op-x '())]
                [(type op-x op-y) (new type op-x op-y)]))))

         ;; record-type: &peg-error -> &condition
         ;; Flags syntax errors during compilation of PEG parser.
         (define-record-type (&peg-error make-peg-error peg-error?)
           (parent &condition)
           (fields (immutable who  peg-error-who)
                   (immutable what peg-error-what)
                   (immutable why  peg-error-why))
           (sealed #t))

         )

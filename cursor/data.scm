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
                 CAPTURE
                 CAPTURE-START
                 CAPTURE-STOP
                 REPEAT
                 IS
                 IS-NOT
                 ONE-OF
                 NONE-OF
                 MATCH
          ;; record-type: ast        
                 encode-ast ;; constructor
                 ast?       ;; predicate
                 ast-type   ;; field
                 ast-node-x ;; field
                 ast-node-y ;; field
          ;; record-type: &peg-error -> &condition
                 make-peg-error ;; constructor
                 peg-error?     ;; predicate
                 peg-error-who  ;; field
                 peg-error-what ;; field
                 peg-error-why) ;; field
         (import (rnrs)
                 (cursor tools))

         ;; Symbols that identify nodes within abstract syntax trees
         ;; and instructions within instruction lists.
         
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
               CAPTURE
               CAPTURE-START
               CAPTURE-STOP
               REPEAT
               IS
               IS-NOT
               ONE-OF
               NONE-OF
               MATCH)

         ;; record-type: (ast type node node)
         ;;                where type = symbol
         ;;                      node = ast | (list ast ...) | (vector ast ...)
         ;;                      node = ast | (list ast ...) | (vector ast ...)
         ;;
         ;; The leaves and branches within an abstract syntax tree.
         ;; The type identifies the node. The child nodes are themselves
         ;; "ast"s or lists or vectors containing zero or more "ast"s.

         (define-record-type (ast encode-ast ast?)
           (fields type
                   node-x
                   node-y)
           (sealed #t)
           (protocol
            (lambda (new)
             (case-lambda
               [(type)               (new type '() '())]
               [(type node-x)        (new type node-x '())]
               [(type node-x node-y) (new type node-x node-y)]))))

         ;; record-type: &peg-error -> &condition
         ;; Flags syntax errors during compilation of PEG parser.
         
         (define-record-type (&peg-error make-peg-error peg-error?)
           (parent &condition)
           (fields (immutable who  peg-error-who)
                   (immutable what peg-error-what)
                   (immutable why  peg-error-why))
           (sealed #t))

         )

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
                 ast-equal?
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

         ;; (ast-equal? ast ast) -> boolean
         ;; Deep, structural comparison of asts.  
         (define ast-equal?
           (lambda (a b)
             (and (ast? a) (ast? b)
                  (let ([type-a (ast-type a)]
                        [type-b (ast-type b)])
                    (and (eq? type-a type-b)
                         (case type-a
                           [(EMPTY FAIL ANY) #t]
                           [(CHARACTER)
                            (char=? (ast-node-x a) (ast-node-x b))]
                           [(SEQUENCE CHOICE)
                            (let ([node-a (ast-node-x a)]
                                  [node-b (ast-node-x b)])
                              (and (= (length node-a) (length node-b))
                                   (for-all ast-equal? node-a node-b)))]
                           [(REPEAT IS IS-NOT)
                            (ast-equal? (ast-node-x a) (ast-node-x b))]
                           [(ONE-OF NONE-OF)
                            (charset-equal? (ast-node-x a) (ast-node-x b))]
                           [(OPEN-CALL)
                            (eq? (ast-node-x a) (ast-node-x b))]
                           [(CALL)
                            (and (eq? (ast-node-x a) (ast-node-x b))
                                 (= (ast-node-y a) (ast-node-y b)))]
                           [(GRAMMAR)
                            (for-all ast-equal?
                                     (vector->list (ast-node-x a))
                                     (vector->list (ast-node-x b)))]
                           [(RULE)
                            (and (eq? (ast-node-x a) (ast-node-x b))
                                 (ast-equal? (ast-node-y a) (ast-node-y b)))]
                           ;; Capture comparison.
                           ;; Cannot meaningfully compare arbitrary functions.
                           ;; Check only for their presence or absence within
                           ;; both captures.
                           [(CAPTURE)
                            (let ([a-f? (procedure? (ast-node-x a))]
                                  [a-px (ast-node-y a)]
                                  [b-f? (procedure? (ast-node-x b))]
                                  [b-px (ast-node-y b)])
                              (let ([present? (and a-f? b-f?)])
                                (and (or present? (not present?))
                                     (ast-equal? a-px b-px))))]
                           [else #f]))))))

         ;; record-type: &peg-error -> &condition
         ;; Flags syntax errors during compilation of PEG parser.
         (define-record-type (&peg-error make-peg-error peg-error?)
           (parent &condition)
           (fields (immutable who  peg-error-who)
                   (immutable what peg-error-what)
                   (immutable why  peg-error-why))
           (sealed #t))

         )

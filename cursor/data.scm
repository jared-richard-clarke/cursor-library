(library (cursor data)
         ;; enumerations
         (export EMPTY
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
                 RETURN
                 JUMP
                 CAPTURE
                 CAPTURE-START
                 CAPTURE-STOP
                 TRANSFORM
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
          ;; record-type: &peg -> &condition
                 make-peg-error ;; constructor
                 peg-error?     ;; predicate
                 peg-error)

         (import (rnrs)
                 (cursor tools)
                 (cursor collections charset))

         ;; Symbols that identify nodes within abstract syntax trees
         ;; and instructions within instruction lists.
         (enum EMPTY
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
               RETURN
               JUMP
               CAPTURE
               CAPTURE-START
               CAPTURE-STOP
               TRANSFORM
               REPEAT
               IS
               IS-NOT
               ONE-OF
               NONE-OF
               MATCH)

         ;; record-type: (ast type node-x node-y)
         ;;                where type = symbol
         ;;                      node-x = ast | (list ast ...) | (vector ast ...)
         ;;                      node-y = ast | (list ast ...) | (vector ast ...)
         ;;
         ;; The leaves and branches within an abstract syntax tree.
         ;; The type identifies the node. The child nodes are themselves
         ;; ASTs or lists or vectors containing zero or more ASTs.
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

         ;; empty, fail, any, character, sequence, choice, repeat,
         ;; is?, is-not?, one-of, none-of, call, grammar, rule, capture

         ;; (ast-equal? ast ast) -> boolean
         ;; Deep, structural comparison of ASTs.
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
                           ;; Capture and Transform comparison.
                           ;; Function equality is undecidable.
                           ;; We check only for their presence or absence
                           ;; within both captures or transforms.
                           [(CAPTURE TRANSFORM)
                            (let ([a-f? (procedure? (ast-node-x a))]
                                  [a-n? (null? (ast-node-x a))]
                                  [a-px (ast-node-y a)]
                                  [b-f? (procedure? (ast-node-x b))]
                                  [b-n? (null? (ast-node-x b))]
                                  [b-px (ast-node-y b)])
                              (let ([both-functions? (and a-f? b-f?)]
                                    [both-nulls?     (and a-n? b-n?)])
                                (and (or both-functions? both-nulls?)
                                     (ast-equal? a-px b-px))))]
                           [else #f]))))))

         ;; record-type: &peg -> &condition
         ;; Identifies errors that occur in PEG expressions and parsing functions.
         (define-condition-type &peg &condition make-peg-error peg-error?)

         (define peg-error
           (lambda (who message irritants)
             (raise (condition (make-peg-error)
                               (make-who-condition who)
                               (make-message-condition message)
                               (make-irritants-condition irritants)))))

)

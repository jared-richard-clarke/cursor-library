(library (cursor compiler)
         (export compile)
         (import (rnrs)
                 (cursor core)
                 (cursor data))

         (define ERROR-TYPE-AST   "not an abstract syntax tree")
         (define ERROR-UNKNOWN-AST "unknown AST type")

         ;; === Helper Functions ===
         ;;
         ;; Functions assume pairs are lists. This assumption is safe 
         ;; as long as ASTs compile to either terminals or lists.
         
         (define check-length
           (lambda (x)
             (if (pair? x)
                 (length x)
                 1)))

         (define fold-codes
           (lambda xs
             (let recur ([xs xs])
               (cond [(null? (cdr xs)) xs]
                     [(pair? (car xs)) (append (car xs) (recur (cdr xs)))]
                     [else (cons (car xs) (recur (cdr xs)))]))))

         ;; === Compiler ===

         (define compile
           (lambda (x)
             (unless (ast? x)
               (raise (make-peg-error "(compile _)" x ERROR-TYPE-AST)))
             (let ([type (ast-type x)])
               (case type
                 [(EMPTY ANY FAIL) (compile-symbol type)]
                 [(CHARACTER)      (compile-character x)]
                 [(SEQUENCE)       (compile-sequence x)]
                 [(CHOICE)         (compile-choice x)]
                 [(REPEAT)         (compile-repeat x)]
                 [(IS)             (compile-is x)]
                 [(IS-NOT)         (compile-is-not x)]
                 [(ONE-OF NONE-OF) (compile-set x)]
                 [(CAPTURE)        (compile-capture x)]
                 [else
                  (raise (make-peg-error "undefined" type ERROR-UNKNOWN-AST))]))))
         
         (define compile-symbol
           (lambda (x)
             (ast-type x)))

         ;; === Character Match ===
         ;;
         ;; Π(g, i, 'c') ≡ Char c
         (define compile-character
           (lambda (x)
             (ast-node-x x)))

         ;; === Concatenation ===
         ;;
         ;; Π(g, i, p₁p₂) ≡ Π(g, i, p₁) Π(g, i + |Π(g, x, p₁)|, p₂)
         (define compile-sequence
           (lambda (xs)
             (let ([nodes (ast-node-x xs)])
               (fold-right (lambda (node accum)
                             (let ([code (compile node)])
                               (if (pair? code)
                                   (append code accum)
                                   (cons code accum))))
                           '()
                           nodes))))

         ;; === Ordered Choice ===
         ;;
         ;; Π(g, i, p₁/p₂) ≡ Choice |Π(g, x, p₁)| + 2
         ;;                  Π(g, i + 1, p₁)
         ;;                  Commit |Π(g, x, p₂)| + 1
         ;;                  Π(g, i + |Π(g, x, p₁)| + 1, p₂)
         (define compile-choice
           (lambda (xs)
             (let ([nodes (ast-node-x xs)])
               (cdr (fold-choices nodes)))))

         (define fold-choices
           (lambda (xs)
             (let ([or-else (lambda (code-x code-y)
                              (let ([offset-x (check-length code-x)]
                                    [offset-y (car code-y)]
                                    [code-y   (cdr code-y)])
                                  (cons (+ offset-x offset-y 4)
                                        (fold-codes CHOICE (+ offset-x 2)
                                                    code-x
                                                    COMMIT (+ offset-y 1)
                                                    code-y))))])
               (let recur ([xs xs])
                 (if (null? (cdr xs))
                     (let ([x (compile (car xs))])
                       (cons (check-length x) x))
                     (or-else (compile (car xs)) (recur (cdr xs))))))))

         ;; === Repetition ===
         ;;
         ;; Π(g, i, p*) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               PartialCommit − |Π(g, x, p)|
         (define compile-repeat
           (lambda (x)
             (let ([code (compile (ast-node-x x))])
               (let ([offset (check-length code)])
                 (fold-codes CHOICE (+ offset 2)
                             code
                             PARTIAL-COMMIT (- offset))))))

         ;; === And Predicate ===
         ;;
         ;; Π(g, i, &p) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               BackCommit 2
         ;;               Fail
         (define compile-is
           (lambda (x)
             (let ([code (compile (ast-node-x x))])
               (let ([offset (check-length code)])
                 (fold-codes CHOICE (+ offset 2)
                             code
                             BACK-COMMIT 2
                             FAIL)))))

         ;; === Not Predicate ===
         ;;
         ;; Π(g, i, !p) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               FailTwice
         (define compile-is-not
           (lambda (x)
             (let ([code (compile (ast-node-x x))])
               (let ([offset (check-length code)])
                 (fold-codes CHOICE (+ offset 2)
                             code
                             FAIL-TWICE)))))

         ;; === Sets: "one-of" and "none-of" ===
         
         (define compile-set
           (lambda (x)
             (let ([type (ast-type x)]
                   [set  (ast-node-x x)])
               (fold-codes type set))))

         ;; === Captures ===
         
         (define compile-capture
           (lambda (x)
             (let ([fn   (ast-node-x x)]
                   [code (compile (ast-node-y x))])
               (fold-codes CAPTURE-START fn
                           code
                           CAPTURE-STOP))))

)

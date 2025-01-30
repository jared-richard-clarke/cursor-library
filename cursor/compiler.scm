(library (cursor compiler)
         (export compile
                 (rename unit-tests compiler:unit-tests))
         (import (rnrs)
                 (cursor core)
                 (cursor data)
                 (cursor collections charset))

         (define ERROR-TYPE-AST    "not an abstract syntax tree")
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
             (let recur ([codes xs])
               (cond [(null? (cdr codes))
                      (if (pair? (car codes))
                          (car codes)
                          codes)]
                     [(pair? (car codes))
                      (append (car codes) (recur (cdr codes)))]
                     [else
                      (cons (car codes) (recur (cdr codes)))]))))

         ;; === Compiler ===

         (define compile
           (lambda (x)
             (unless (ast? x)
               (raise (make-peg-error "(compile _)" x ERROR-TYPE-AST)))
             (let ([type (ast-type x)])
               (case type
                 [(EMPTY ANY FAIL) (compile-symbol x)]
                 [(CHARACTER)      (compile-character x)]
                 [(SEQUENCE)       (compile-sequence x)]
                 [(CHOICE)         (compile-choice x)]
                 [(REPEAT)         (compile-repeat x)]
                 [(IS)             (compile-is x)]
                 [(IS-NOT)         (compile-is-not x)]
                 [(ONE-OF NONE-OF) (compile-set x)]
                 [(CAPTURE)        (compile-capture x)]
                 [(CALL)           (compile-call x)]
                 [(GRAMMAR)        (compile-grammar x)]
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
           (lambda (x)
             (let recur ([nodes (ast-node-x x)])
               (cond [(null (cdr nodes))
                      (let ([code (compile (car nodes))])
                        (if (pair? code)
                            code
                            (list code)))]
                     [else
                      (let ([code (compile (car nodes))])
                        (if (pair? code)
                            (append code (recur (cdr nodes)))
                            (cons code (recur (cdr nodes)))))]))))

         ;; === Ordered Choice ===
         ;;
         ;; Π(g, i, p₁/p₂) ≡ Choice |Π(g, x, p₁)| + 2
         ;;                  Π(g, i + 1, p₁)
         ;;                  Commit |Π(g, x, p₂)| + 1
         ;;                  Π(g, i + |Π(g, x, p₁)| + 1, p₂)
         (define compile-choice
           (lambda (x)
             (let ([combine (lambda (code-x code-y)
                              (let ([offset-x (check-length code-x)]
                                    [offset-y (car code-y)]
                                    [code-y   (cdr code-y)])
                                  (cons (+ offset-x offset-y 4)
                                        (fold-codes CHOICE (+ offset-x 3)
                                                    code-x
                                                    COMMIT (+ offset-y 1)
                                                    code-y))))])
               (cdr (let recur ([nodes (ast-node-x x)])
                      (if (null? (cdr nodes))
                          (let ([code (compile (car nodes))])
                            (cons (check-length code) code))
                          (combine (compile (car nodes)) (recur (cdr nodes)))))))))

         ;; === Repetition ===
         ;;
         ;; Π(g, i, p*) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               PartialCommit − |Π(g, x, p)|
         (define compile-repeat
           (lambda (x)
             (let ([code (compile (ast-node-x x))])
               (let ([offset (check-length code)])
                 (fold-codes CHOICE (+ offset 3)
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
                 (fold-codes CHOICE (+ offset 3)
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

         ;; === Grammars ===

         (define compile-call
           (lambda (x)
             (fold-codes OPEN-CALL (ast-node-x x))))
         
         (define compile-grammar
           (lambda (x)
             (let ([rules   (ast-node-x x)]
                   [size    (vector-length (ast-node-x x))]
                   [offsets (make-eqv-hashtable)])
               (let loop ([index 0]
                          [codes '()]
                          [total 4])
                 (cond [(>= index size)
                        (let ([code (fold-codes CALL (- total 2)
                                                JUMP (- total 4)
                                                (apply fold-codes (reverse codes)))])
                          (adjust-offsets code offsets))]
                       [else
                        (let* ([rule (vector-ref rules index)]
                               [name (ast-node-x rule)]
                               [code (fold-codes (compile (ast-node-y rule)) RETURN)])
                          (hashtable-set! offsets name total)
                          (loop (+ index 1)
                                (cons code codes)
                                (+ total (check-length code))))])))))

         (define adjust-offsets
           (lambda (xs offsets)
             (let ([peekable? (lambda (x) (and (pair? x) (pair? (cdr x))))]
                   [first     car]
                   [second    cadr]
                   [third     caddr])
               (let recur ([index 0]
                           [codes xs])
                 (cond [(null? codes)
                        codes]
                       [(eq? (first codes) OPEN-CALL)
                        (let ([offset (hashtable-ref offsets (second codes) 0)])
                          (if (and (peekable? (second codes))
                                   (eq? (third codes) RETURN))
                              ;; open-call -> tail-call
                              (cons JUMP (cons (- offset index)
                                               (recur (+ index 2) (cddr codes))))
                              ;; open-call -> call
                              (cons CALL (cons (- offset index)
                                               (recur (+ index 2) (cddr codes))))))]
                       [else
                        (cons (car codes)
                              (recur (+ index 1) (cdr codes)))]))))))

         ;; === Unit Tests ===

         (define unit-tests
           (let ([A (char #\a)]
                 [B (char #\b)]
                 [C (char #\c)]
                 [set-ABC (make-charset "abc")]
                 [set-equal? (lambda (x y)
                               (and (list? x)
                                    (list? y)
                                    (eq? (car x) (car y))
                                    (charset-equal? (cadr x) (cadr y))))]
                 [capture-equal? (lambda (x y)
                                   (and (list? x)
                                        (list? y)
                                        (eq? (car x) CAPTURE-START)
                                        (eq? (car y) CAPTURE-START)
                                        (equal? (caddr x) (caddr y))))])
             (test-chunk
              "Cursor Compiler"
              (test-assert "character literal"
                           equal?
                           (compile A)
                           #\a)

              (test-assert "text sequence abc"
                           equal?
                           (compile (text "abc"))
                           '(#\a #\b #\c))
              
              (test-assert "text epsilon"
                           equal?
                           (compile (text ""))
                           EMPTY)

              (test-assert "sequence abc"
                           equal?
                           (compile (sequence A B C))
                           '(#\a #\b #\c))

              (test-assert "sequence nested"
                           equal?
                           (compile (sequence (sequence A B) (sequence C B (sequence A))))
                           '(#\a #\b #\c #\b #\a))

              (test-assert "sequence identity"
                           equal?
                           (compile (sequence))
                           EMPTY)

              (test-assert "choice, a / b"
                           equal?
                           (choice A B)
                           '(CHOICE 4 #\a COMMIT 2 #\b))

              (test-assert "choice, a / b / c"
                           equal?
                           (compile (choice A B C))
                           '(CHOICE 4 #\a COMMIT 7 CHOICE 4 #\b COMMIT 2 #\c))

              (test-assert "choice, a / (b / c)"
                           equal?
                           (compile (choice (choice A B) C))
                           '(CHOICE 4 #\a COMMIT 7 CHOICE 4 #\b COMMIT 2 #\c))

              (test-assert "choice identity"
                           equal?
                           (compile (choice))
                           FAIL)

              (test-assert "repeat a*"
                           equal?
                           (compile (repeat A))
                           '(CHOICE 4 #\a PARTIAL-COMMIT -2))

              (test-assert "repeat a+"
                           equal?
                           (compile (repeat+1 A))
                           '(#\a CHOICE 4 #\a PARTIAL-COMMIT -2))

              (test-assert "predicate &a"
                           equal?
                           (compile (is? A))
                           '(CHOICE 4 #\a BACK-COMMIT 2 FAIL))

              (test-assert "predicate !a"
                           equal?
                           (compile (is-not? A))
                           '(CHOICE 3 #\a FAIL-TWICE))

              (test-assert "character set [abc]"
                           set-equal?
                           (compile (one-of "abc"))
                           (list ONE-OF set-ABC))

              (test-assert "character set [^abc]"
                           set-equal?
                           (compile (none-of "abc"))
                           (list NONE-OF set-ABC))

              (test-assert "character set unique members"
                           set-equal?
                           (compile (one-of "abcbbc"))
                           (list ONE-OF set-ABC))

              (test-assert "capture, baseline"
                           capture-equal?
                           (compile (capture (sequence A B C)))
                           '(CAPTURE-START () #\a #\b #\c CAPTURE-STOP)))))

)

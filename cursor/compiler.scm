(library (cursor compiler)
         (export compile
                 (rename (unit-tests compiler:unit-tests)))
         (import (rnrs)
                 (cursor core)
                 (cursor data)
                 (cursor tools)
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

         (define fold-code
           (lambda xs
             (let recur ([xs xs])
               (cond [(null? (cdr xs))
                      (if (pair? (car xs))
                          (car xs)
                          xs)]
                     [(pair? (car xs))
                      (append (car xs) (recur (cdr xs)))]
                     [else
                      (cons (car xs) (recur (cdr xs)))]))))

         ;; === Compiler ===

         (define compile
           (lambda (x)
             (unless (ast? x)
               (raise (make-peg-error "(compile _)" x ERROR-TYPE-AST)))
             (let* ([code   (compile-ast x)]
                    [size   (length code)]
                    [buffer (make-vector (+ size 1))])
               (let ([program
                      (let loop ([index 0] [xs code])
                        (cond [(= index size)
                               (vector-set! buffer index MATCH)
                               buffer]
                              [else
                               (vector-set! buffer index (car xs))
                               (loop (+ index 1) (cdr xs))]))])
                 (lambda ()
                   program)))))

         (define compile-ast
           (lambda (x)
             (let ([type (ast-type x)])
               (case type
                 [(EMPTY ANY FAIL) (compile-symbol x)]
                 [(CHARACTER)      (compile-character x)]
                 [(SEQUENCE)       (compile-sequence x)]
                 [(CHOICE)         (compile-choice x)]
                 [(REPEAT)         (compile-repeat x)]
                 [(IS IS-NOT)      (compile-predicate x)]
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
               (cond [(null? (cdr nodes))
                      (let ([code (compile-ast (car nodes))])
                        (if (pair? code)
                            code
                            (list code)))]
                     [else
                      (let ([code (compile-ast (car nodes))])
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
                                        (fold-code CHOICE (+ offset-x 4)
                                                   code-x
                                                   COMMIT (+ offset-y 2)
                                                   code-y))))])
               (cdr (let recur ([nodes (ast-node-x x)])
                      (if (null? (cdr nodes))
                          (let ([code (compile-ast (car nodes))])
                            (cons (check-length code) code))
                          (combine (compile-ast (car nodes)) (recur (cdr nodes)))))))))

         ;; === Repetition ===
         ;;
         ;; Π(g, i, p*) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               PartialCommit − |Π(g, x, p)|
         (define compile-repeat
           (lambda (x)
             (let ([code (compile-ast (ast-node-x x))])
               (let ([offset (check-length code)])
                 (fold-code CHOICE (+ offset 4)
                            code
                            PARTIAL-COMMIT (- offset))))))

         ;; === And Predicate ===
         ;;
         ;; Π(g, i, &p) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               BackCommit 2
         ;;               Fail
         ;;
         ;; === Not Predicate ===
         ;;
         ;; Π(g, i, !p) ≡ Choice |Π(g, x, p)| + 2
         ;;               Π(g, i + 1, p)
         ;;               FailTwice
         (define compile-predicate
           (lambda (x)
             (let* ([type   (ast-type x)]
                    [code   (compile-ast (ast-node-x x))]
                    [offset (check-length code)])
               (cond [(eq? type IS)
                      (fold-code CHOICE (+ offset 4)
                                 code
                                 BACK-COMMIT 3
                                 FAIL)]
                     [else
                      (fold-code CHOICE (+ offset 3)
                                 code
                                 FAIL-TWICE)]))))

         ;; === Sets: "one-of" and "none-of" ===
         
         (define compile-set
           (lambda (x)
             (let ([type (ast-type x)]
                   [set  (ast-node-x x)])
               (fold-code type set))))

         ;; === Captures ===
         
         (define compile-capture
           (lambda (x)
             (let ([fn   (ast-node-x x)]
                   [code (compile-ast (ast-node-y x))])
               (fold-code CAPTURE-START fn
                          code
                          CAPTURE-STOP))))

         ;; === Grammars ===

         (define compile-call
           (lambda (x)
             (fold-code OPEN-CALL (ast-node-x x))))

         ;; Π(g', i, (g, Ak)) ≡ Call o(g, Ak)
         ;;                     Jump |Π'(g, x)| + 1
         ;;                     Π'(g, 2) <--------- Keeps the invariant that all positions are relative
         ;;                                         to the first rule of the closed grammar.
         ;;
         ;; where Π'(g, i) = Π(g, i, g(A1))
         ;;                  Return
         ;;                  ...
         ;;                          k-1
         ;;                  Π(g, i + Σ |Π(g, x, Aj)| + 1, g(Ak))
         ;;                          j=1
         ;;                  Return
         ;;                  ...
         ;;                          n-1
         ;;                  Π(g, i + Σ |Π(g, x, Aj)| + 1, g(An))
         ;;                          j=1
         ;;                  Return
         (define compile-grammar
           (lambda (x)
             (let ([rules   (ast-node-x x)]
                   [size    (vector-length (ast-node-x x))]
                   [offsets (make-eqv-hashtable)])
               (let loop ([index 0]
                          [codes '()]
                          [total 4])
                 (cond [(>= index size)
                        (let ([code (fold-code CALL 4
                                               JUMP (- total 2)
                                               (apply fold-code (reverse codes)))])
                          (adjust-offsets code offsets))]
                       [else
                        (let* ([rule (vector-ref rules index)]
                               [name (ast-node-x rule)]
                               [code (fold-code (compile-ast (ast-node-y rule)) RETURN)])
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
                          (if (and (peekable? (cdr codes))
                                   (eq? (third codes) RETURN))
                              ;; open-call -> tail-call
                              (cons JUMP (cons (- offset index)
                                               (recur (+ index 2) (cddr codes))))
                              ;; open-call -> call
                              (cons CALL (cons (- offset index)
                                               (recur (+ index 2) (cddr codes))))))]
                       [else
                        (cons (car codes)
                              (recur (+ index 1) (cdr codes)))])))))

         ;; === Unit Tests ===

         (define unit-tests
           (let ([A (char #\a)]
                 [B (char #\b)]
                 [C (char #\c)]
                 [set-ABC (make-charset "abc")]
                 [identity    (lambda (x) x)]
                 [code-equal? (lambda (xs ys)
                                (cond [(and (list? xs) (list? ys)
                                            (= (length xs) (length ys)))
                                       (for-all (lambda (x y)
                                                  (cond [(and (null? x) (null? y))
                                                         #t]
                                                        [(and (symbol? x) (symbol? y))
                                                         (eq? x y)]
                                                        [(and (char? x) (char? y))
                                                         (char=? x y)]
                                                        [(and (number? x) (number? y))
                                                         (= x y)]
                                                        [(and (charset? x) (charset? y))
                                                         (charset-equal? x y)]
                                                        ;; Function comparison is undecidable. Return
                                                        ;; true and move on to the next comparison.
                                                        [(and (procedure? x) (procedure? y))
                                                         #t]
                                                        [else #f]))
                                                xs
                                                ys)]
                                      [else
                                       (equal? xs ys)]))])
             (test-chunk
              "Cursor Compiler"
              (test-assert "character literal"
                           code-equal?
                           (compile-ast A)
                           #\a)

              (test-assert "text sequence abc"
                           code-equal?
                           (compile-ast (text "abc"))
                           '(#\a #\b #\c))
              
              (test-assert "text epsilon"
                           code-equal?
                           (compile-ast (text ""))
                           EMPTY)

              (test-assert "sequence abc"
                           code-equal?
                           (compile-ast (sequence A B C))
                           '(#\a #\b #\c))

              (test-assert "sequence nested"
                           code-equal?
                           (compile-ast (sequence (sequence A B) (sequence C B (sequence A))))
                           '(#\a #\b #\c #\b #\a))

              (test-assert "sequence identity"
                           code-equal?
                           (compile-ast (sequence))
                           EMPTY)

              (test-assert "choice, a / b"
                           code-equal?
                           (compile-ast (choice A B))
                           '(CHOICE 5 #\a COMMIT 3 #\b))

              (test-assert "choice, a / b / c"
                           code-equal?
                           (compile-ast (choice A B C))
                           '(CHOICE 5 #\a COMMIT 8 CHOICE 5 #\b COMMIT 3 #\c))

              (test-assert "choice, a / (b / c)"
                           code-equal?
                           (compile-ast (choice (choice A B) C))
                           '(CHOICE 5 #\a COMMIT 8 CHOICE 5 #\b COMMIT 3 #\c))

              (test-assert "choice identity"
                           code-equal?
                           (compile-ast (choice))
                           FAIL)

              (test-assert "repeat a*"
                           code-equal?
                           (compile-ast (repeat A))
                           '(CHOICE 5 #\a PARTIAL-COMMIT -1))

              (test-assert "repeat a+"
                           code-equal?
                           (compile-ast (repeat+1 A))
                           '(#\a CHOICE 5 #\a PARTIAL-COMMIT -1))

              (test-assert "predicate &a"
                           code-equal?
                           (compile-ast (is? A))
                           '(CHOICE 5 #\a BACK-COMMIT 3 FAIL))

              (test-assert "predicate !a"
                           code-equal?
                           (compile-ast (is-not? A))
                           '(CHOICE 4 #\a FAIL-TWICE))

              (test-assert "character set [abc]"
                           code-equal?
                           (compile-ast (one-of "abc"))
                           (list ONE-OF set-ABC))

              (test-assert "character set [^abc]"
                           code-equal?
                           (compile-ast (none-of "abc"))
                           (list NONE-OF set-ABC))

              (test-assert "character set unique members"
                           code-equal?
                           (compile-ast (one-of "abcbbc"))
                           (list ONE-OF set-ABC))

              (test-assert "capture, baseline"
                           code-equal?
                           (compile-ast (capture (sequence A B C)))
                           '(CAPTURE-START () #\a #\b #\c CAPTURE-STOP))

              (test-assert "capture, true positive"
                           code-equal?
                           (compile-ast (capture identity A))
                           (list CAPTURE-START identity #\a CAPTURE-STOP))

              (test-assert "capture, false positive, + ≠ identity"
                           code-equal?
                           (compile-ast (capture + A))
                           (list CAPTURE-START identity #\a CAPTURE-STOP))

              (test-assert "grammar, baseline"
                           code-equal?
                           (compile-ast (grammar [R1 (sequence A (call R2) C)]
                                                 [R2 B]))
                           '(CALL 4 JUMP 9 #\a CALL 4 #\c RETURN #\b RETURN))

              (test-assert "grammar, tail call"
                           code-equal?
                           (compile-ast (grammar [R1 (sequence A B (call R2))]
                                                 [R2 C]))
                           '(CALL 4 JUMP 9 #\a #\b JUMP 3 RETURN #\c RETURN)))))

)

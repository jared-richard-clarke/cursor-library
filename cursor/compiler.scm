(library (cursor compiler)
         (export compile
                 (rename (unit-tests compiler:unit-tests)))
         (import (rnrs)
                 (cursor core)
                 (cursor data)
                 (cursor tools)
                 (cursor vm)
                 (cursor collections charset))

         ;; === Error Constants ===
         
         (define ERROR-TYPE-AST    "not an abstract syntax tree")
         (define ERROR-TYPE-STRING "argument must be of type string")
         (define ERROR-UNKNOWN-AST "unknown AST type")
         
         ;; === Helper Functions ===
         ;;
         ;; These helper functions assume pairs are lists. This assumption
         ;; is safe as long as ASTs compile to either terminals or lists.
         ;; We make this assumption for efficiency. Checking for lists is
         ;; an O(n) operation, where n is the length of the list.

         ;; (check-length x) -> number
         ;;   where x = list | any
         ;;
         ;; Returns the length of a list or assumes length 1 for a non-list.
         ;; Assumes pairs are lists.
         (define check-length
           (lambda (x)
             (if (pair? x)
                 (length x)
                 1)))

         ;; (fold-code x ...) -> (list x ...)
         ;;   where x = list | any
         ;;
         ;; Combines one or more lists and elements into a list.
         ;; Assumes pairs are lists.
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

         ;; (compile (ast type node-x node-y)) -> (function string) -> boolean | captures
         ;;   where captures = (list (list char ...) ...) | any
         ;;
         ;; Transforms an AST into a parsing function, which runs a match
         ;; over a string and returns 1 of 4 results:
         ;;
         ;; 1. Boolean true for match.
         ;; 2. Boolean false for non-match.
         ;; 3. List of lists of captured character matches.
         ;; 4. Arbitrary values that have been captured as lists of
         ;;    characters and then transformed by associated functions.
         (define compile
           (lambda (x)
             (unless (ast? x)
               (peg-error "(compile _)" ERROR-TYPE-AST (list x)))
             (let* ([code      (compile-ast x)]
                    [code-list (if (pair? code) code (list code))]
                    [size      (length code-list)]
                    [buffer    (make-vector (+ size 1))])
               (let ([program
                      (let loop ([index 0] [xs code-list])
                        (cond [(= index size)
                               (vector-set! buffer index MATCH)
                               buffer]
                              [else
                               (vector-set! buffer index (car xs))
                               (loop (+ index 1) (cdr xs))]))])
                 ;; Returns parsing function, which runs a PEG virtual machine over a string.
                 ;; Contains virtual machine instructions within its closure.
                 (lambda (text)
                   (unless (string? text)
                     (peg-error "parsing function" ERROR-TYPE-STRING (list text)))
                   (run-vm (string->vector text) program))))))

         ;; (compile-ast (ast type node-x node-y)) -> code | (list code ...) | raise peg-error
         ;;   where code = symbol | char | number | function | charset
         ;;
         ;; According to type, delegates the recursive, depth-first transformation
         ;; of an AST into a list of virtual machine instructions. Raises an error
         ;; for ASTs whose type is undefined.
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
                 [(TRANSFORM)      (compile-transform x)]
                 [(CALL)           (compile-call x)]
                 [(GRAMMAR)        (compile-grammar x)]
                 [else
                  (peg-error "(compile-ast _)" ERROR-UNKNOWN-AST (list type))]))))

         ;; (compile-symbol (ast type)) -> symbol
         (define compile-symbol
           (lambda (x)
             (ast-type x)))

         ;; === Character Match ===
         ;;
         ;; (compile-character (ast CHARACTER node-x)) -> char
         (define compile-character
           (lambda (x)
             (ast-node-x x)))

         ;; === Concatenation ===
         ;;
         ;; (compile-sequence (ast SEQUENCE node-x)) -> (list code ...)
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
         ;; (compile-choice (ast CHOICE node-x)) -> (list CHOICE number code COMMIT number code ...)
         (define compile-choice
           (lambda (x)
             ;; To avoid redundant calculation, pass the accumulative offset
             ;; alongside compiled code back up the call chain.
             (let ([combine (lambda (code-x code-y)
                              (let ([offset-x (check-length code-x)]
                                    [offset-y (car code-y)]
                                    [code-y   (cdr code-y)])
                                  (cons (+ offset-x offset-y 4)
                                        (fold-code CHOICE (+ offset-x 4)
                                                   code-x
                                                   COMMIT (+ offset-y 2)
                                                   code-y))))])
               ;; "cdr" discards the final offset, returning only the compiled code.
               (cdr (let recur ([nodes (ast-node-x x)])
                      (if (null? (cdr nodes))
                          (let ([code (compile-ast (car nodes))])
                            (cons (check-length code) code))
                          (combine (compile-ast (car nodes)) (recur (cdr nodes)))))))))

         ;; === Repetition ===
         ;;
         ;; (compile-repeat (ast REPEAT node-x)) -> (list CHOICE number code PARTIAL-COMMIT (- number))
         (define compile-repeat
           (lambda (x)
             (let ([code (compile-ast (ast-node-x x))])
               (let ([offset (check-length code)])
                 (fold-code CHOICE (+ offset 4)
                            code
                            PARTIAL-COMMIT (- offset))))))

         ;; === Predicates: "is" and "is-not" ===
         ;;
         ;; (compile-predicate (ast IS node-x))     -> (list CHOICE number code BACK-COMMIT number FAIL)
         ;; (compile-predicate (ast IS-NOT node-x)) -> (list CHOICE number code FAIL-TWICE)
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
         ;;
         ;; (compile-set (ast ONE-OF node-x))  -> (list ONE-OF charset)
         ;; (compile-set (ast NONE-OF node-x)) -> (list NONE-OF charset)
         (define compile-set
           (lambda (x)
             (let ([type (ast-type x)]
                   [set  (ast-node-x x)])
               (fold-code type set))))

         ;; === Captures ===
         ;;
         ;; (compile-capture (ast CAPTURE node-x node-y)) -> (list CAPTURE-START fn code CAPTURE-STOP)
         (define compile-capture
           (lambda (x)
             (let ([fn   (ast-node-x x)]
                   [code (compile-ast (ast-node-y x))])
               (fold-code CAPTURE-START fn
                          code
                          CAPTURE-STOP))))

         ;; === Transformations ===
         ;;
         ;; (compile-transform (ast TRANSFORM node-x node-y)) -> (list code TRANSFORM fn)
         (define compile-transform
           (lambda (x)
             (let ([fn   (ast-node-x x)]
                   [code (compile-ast (ast-node-y x))])
               (fold-code code TRANSFORM fn))))

         ;; === Grammars ===
         
         ;; (compile-call (ast CALL node-x node-y)) -> (list OPEN-CALL symbol)
         (define compile-call
           (lambda (x)
             (fold-code OPEN-CALL (ast-node-x x))))

         ;; (compile-grammar (ast GRAMMAR node-x)) -> (list code ...)
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
                          (if (> (hashtable-size offsets) 0)
                              (adjust-offsets code offsets)
                              code))]
                       [else
                        (let* ([rule (vector-ref rules index)]
                               [name (ast-node-x rule)]
                               [code (fold-code (compile-ast (ast-node-y rule)) RETURN)])
                          (hashtable-set! offsets name total)
                          (loop (+ index 1)
                                (cons code codes)
                                (+ total (check-length code))))])))))

         ;; (adjust-offsets xs offsets) -> (list code ...)
         ;;
         ;; Calculates the relative offsets of calls to their associated rules
         ;; by substracting the index of the current call from the absolute offset
         ;; of its associated rule.
         ;;
         ;; A call at the end of a rule is transformed into a tail call.
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
           (test-chunk
            "Cursor Compiler"
            ([A (char #\a)]
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

            (test-assert "and-then abc"
                         code-equal?
                         (compile-ast (and-then A B C))
                         '(#\a #\b #\c))

            (test-assert "and-then nested"
                         code-equal?
                         (compile-ast (and-then (and-then A B) (and-then C B (and-then A))))
                         '(#\a #\b #\c #\b #\a))

            (test-assert "and-then identity"
                         code-equal?
                         (compile-ast (and-then))
                         EMPTY)

            (test-assert "or-else, a / b"
                         code-equal?
                         (compile-ast (or-else A B))
                         '(CHOICE 5 #\a COMMIT 3 #\b))

            (test-assert "or-else, a / b / c"
                         code-equal?
                         (compile-ast (or-else A B C))
                         '(CHOICE 5 #\a COMMIT 8 CHOICE 5 #\b COMMIT 3 #\c))

            (test-assert "or-else, a / (b / c)"
                         code-equal?
                         (compile-ast (or-else (or-else A B) C))
                         '(CHOICE 5 #\a COMMIT 8 CHOICE 5 #\b COMMIT 3 #\c))

            (test-assert "or-else identity"
                         code-equal?
                         (compile-ast (or-else))
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
                         (compile-ast (capture (and-then A B C)))
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
                         (compile-ast (grammar [R1 (and-then A (rule R2) C)]
                                               [R2 B]))
                         '(CALL 4 JUMP 9 #\a CALL 4 #\c RETURN #\b RETURN))

            (test-assert "grammar, tail call"
                         code-equal?
                         (compile-ast (grammar [R1 (and-then A B (rule R2))]
                                               [R2 C]))
                         '(CALL 4 JUMP 9 #\a #\b JUMP 3 RETURN #\c RETURN))

            (test-assert "finite state machine"
                         code-equal?
                         (compile-ast (grammar [X (and-then (char #\x) (rule Y))]
                                               [Y (and-then (char #\y) (rule Z))]
                                               [Z (char #\z)]))
                         '(CALL 4 JUMP 12 #\x JUMP 3 RETURN #\y JUMP 3 RETURN #\z RETURN))))

)

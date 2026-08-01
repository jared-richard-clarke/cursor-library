(library (cursor unicode grapheme-break builders dfa)
         (export DFA
                 dfa-fail
                 dfa-start
                 dfa-accept
                 dfa-table
                 (rename (unit-tests dfa:unit-tests)))
         (import (rnrs)
                 (cursor unicode grapheme-break constants)
                 (cursor tools))

         ;; === Constants ===

         (enum EMPTY
               EPSILON
               CONCAT
               UNION
               REPEAT
               NEGATE)

         ;; === Data Types ===

         ;; record: (dfa fail start accept table)
         ;;   where fail   = number
         ;;         start  = number
         ;;         accept = (pair number number)
         ;;         table  = (vector number)
         ;;
         ;; fail:   Fail state offset. Always 0.
         ;; start:  Start state offset.
         ;; accept: Accept states. Exclusive range. May include the start state.
         ;; table:  All transitions packed in a vector. States are delineated by stride,
         ;;         the total number of transitions per state.
         ;;
         ;;         (vector-ref table (+ state-x symbol)) -> state-y
         ;;           where table = #( fail-state accept-states start-state other-states )
         ;;                            ^--------^
         ;;                              stride
         ;;
         ;;                 symbol = (grapheme-break-table codepoint) -> property
         (define-record-type dfa
           (fields fail
                   start
                   accept
                   table))

         ;; === Brzozowski Derivatives ===

         (define empty?
           (lambda (x)
             (eq? x EMPTY)))

         (define epsilon?
           (lambda (x)
             (eq? x EPSILON)))

         ;; literal ∈ Σ,
         ;;   where Σ = Any grapheme-break property as defined in Annex #29
         ;;             of the Unicode standard, version 17.
         ;;
         ;; In formal language theory, Σ usually denotes a finite alphabet
         ;; for a given language.
         (define literal?
           (lambda (x)
             (and (number? x)
                  (grapheme-break-property? x))))

         (define concat?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) CONCAT))))

         ;; x • y • ...
         ;;
         ;; (concat x y ...) -> '(CONCAT (x y ...))
         ;; (concat x)       -> x
         ;; (concat)         -> 'EPSILON
         (define concat
           (lambda xs
             (cond [(null? xs)
                    EPSILON]
                   [(singleton? xs)
                    (car xs)]
                   [else
                    (build-concat xs)])))

         ;; Constructor for concat expressions. Transforms specific expressions
         ;; into simpler but equivalent forms.
         (define build-concat
           (lambda (xs)

             (define append-reverse
               (lambda (xs ys)
                 (fold-left (lambda (y x) (cons x y)) ys xs)))

             ;; Helper function builds the body of a concat expression.
             ;; It also provides three optimizations:
             ;;
             ;; 1. Filters epsilon expressions.
             ;;    - ε • x ≈ x
             ;;    - x • ε ≈ x
             ;;
             ;; 2. Flattens nested concat expressions
             ;;    into a single concat expression.
             ;;
             ;; 3. Breaks or exits construction loop on encountering
             ;;    an empty expression. A concat expression
             ;;    that contains an empty expression is
             ;;    equivalent to the empty expression.
             ;;    - ∅ • x ≈ ∅
             (define filter-flatten-break
               (lambda (input output)
                 (if (null? input)
                     (cond [(null? output)
                            EPSILON]
                           [(singleton? output)
                            (car output)]
                           [else
                            (list CONCAT (reverse output))])
                     (let ([item (car input)]
                           [next (cdr input)])
                             ;; break
                       (cond [(empty? item)
                              EMPTY]
                             ;; filter
                             [(epsilon? item)
                              (filter-flatten-break next output)]
                             ;; flatten
                             [(concat? item)
                              (filter-flatten-break next (append-reverse (cadr item) output))]
                             [else
                              (filter-flatten-break next (cons item output))])))))

             (filter-flatten-break xs '())))

         (define union?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) UNION))))

         ;; x + y + ...
         ;;
         ;; (union x y ...) -> '(UNION (x y ...))
         ;; (union x)       -> x
         ;; (union)         -> 'EMPTY
         ;;
         ;; Side Note: Implements leftmost-first match semantics.
         ;;            If match on both x and y, select x.
         (define union
           (lambda xs
             (cond [(null? xs)
                    EMPTY]
                   [(singleton? xs)
                    (car xs)]
                   [else
                    (build-union xs)])))

         ;; Smart constructor for union expressions. Transforms specific expressions
         ;; into simpler but equivalent forms.
         (define build-union
           (lambda (xs)

             ;; Helper function builds the body of a union expression.
             ;; It also provides two optimizations:
             ;;
             ;; 1. Filters empty expressions.
             ;;    - ∅ + x ≈ x
             ;;
             ;; 2. Flattens nested union expressions into
             ;;    a single union expression.
             (define filter-flatten
               (lambda (xs)
                 (if (null? xs)
                     xs
                     (let ([item (car xs)]
                           [next (cdr xs)])
                             ;; filter
                       (cond [(empty? item)
                              (filter-flatten next)]
                             ;; flatten
                             [(union? item)
                              (append (cadr item) (filter-flatten next))]
                             [else
                              (cons item (filter-flatten next))])))))

             (define choices (filter-flatten xs))

             (cond [(null? choices)
                    EMPTY]
                   [(singleton? choices)
                    (car choices)]
                   [else
                    (list UNION choices)])))

         (define repeat?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) REPEAT))))

         ;; x*
         ;;
         ;; (repeat x)          -> '(REPEAT x)
         ;; (repeat EMPTY)      -> 'EPSILON
         ;; (repeat EPSILON)    -> 'EPSILON
         ;; (repeat (repeat x)) -> '(REPEAT x)
         (define repeat
           (lambda (x)
             (cond [(empty? x)
                    EPSILON]
                   [(epsilon? x)
                    x]
                   [(repeat? x)
                    x]
                   [else
                    (list REPEAT x)])))

         ;; x+ ≈ x • x*
         ;;
         ;; (repeat+1 x) -> '(CONCAT x (REPEAT x))
         (define repeat+1
           (lambda (x)
             (concat x (repeat x))))

         (define negate?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) NEGATE))))

         ;; ¬x
         ;;
         ;; (negate x)          -> '(NEGATE x)
         ;; (negate EMPTY)      -> 'EPSILON
         ;; (negate EPSILON)    -> 'EMPTY
         ;; (negate (negate x)) -> x
         (define negate
           (lambda (x)
             (cond [(empty? x)
                    EPSILON]
                   [(epsilon? x)
                    EMPTY]
                   [(negate? x)
                    x]
                   [else
                    (list NEGATE x)])))

         ;; A nullable expression is one that matches the empty string.
         (define nullable?
           (lambda (regex)
             (cond [(empty? regex)   #f]
                   [(epsilon? regex) #t]
                   [(literal? regex) #f]
                   [(concat? regex)
                    (for-all nullable? (cadr regex))]
                   [(union? regex)
                    (exists  nullable? (cadr regex))]
                   [(repeat? regex)  #t]
                   [(negate? regex)
                    (let ([x (cadr regex)])
                      (not (nullable? x)))]
                   [else
                    (assertion-violation 'nullable?
                                         "not a regular expression"
                                         regex)])))

         ;; Computes the derivative of a regular expression and a given prefix.
         (define derive
           (lambda (regex prefix)
             (cond [(empty? regex)
                    EMPTY]
                   [(epsilon? regex)
                    EMPTY]
                   [(literal? regex)
                    (if (= regex prefix)
                        EPSILON
                        EMPTY)]
                   [(concat? regex)
                    (derive-concat regex prefix)]
                   [(union? regex)
                    (derive-union regex prefix)]
                   [(repeat? regex)
                    (derive-repeat regex prefix)]
                   [(negate? regex)
                    (derive-negate regex prefix)]
                   [else
                    (assertion-violation 'derive
                                         "not a regular expression"
                                         regex)])))

         ;; ∂ₓ(x • y) -> ∂ₓx • y + ν(x) • ∂ₓy
         ;;   where ν(x) = nullable
         (define derive-concat
           (lambda (regex prefix)

             (define sequence (cadr regex))
             (define first    (car sequence))
             (define follow   (cdr sequence))

             (if (nullable? first)
                 (union (apply concat (derive first prefix) follow)
                        (apply concat (derive (car follow) prefix) (cdr follow)))
                 (apply concat (derive first prefix) follow))))

         ;; ∂ₓ(x + y) -> ∂ₓx + ∂ₓy
         (define derive-union
           (lambda (regex prefix)

             (define choices (cadr regex))

             ;; Implements leftmost-first semantics by ignoring derivations
             ;; to the right of an epsilon expression.
             (define derive-leftmost
               (lambda (xs)
                 (if (null? xs)
                     xs
                     (let ([derivation (derive (car xs) prefix)]
                           [next       (cdr xs)])
                       (if (epsilon? derivation)
                           (list derivation)
                           (cons derivation (derive-leftmost next)))))))

             (apply union (derive-leftmost choices))))

         ;; ∂ₓ(x*) -> ∂ₓx • x*
         (define derive-repeat
           (lambda (regex prefix)

             (define original       regex)
             (define sub-expression (cadr regex))
             (define derivation     (derive sub-expression prefix))

             (cond [(empty? derivation)
                    EMPTY]
                   [(epsilon? derivation)
                    original]
                   [else
                    (concat derivation original)])))

         ;; ∂ₓ(¬x) -> ¬(∂ₓx)
         (define derive-negate
           (lambda (regex prefix)

             (define sub-expression (cadr regex))
             (define derivation     (derive sub-expression prefix))

             (negate derivation)))

         ;; === Patterns ===
         ;;
         ;; Regular expression patterns as defined in Table 1c. of Annex #29
         ;; of the Unicode standard, version 17.0.0.
         ;;
         ;; Side Note: Adjacent expressions imply concatenation.
         ;;            CR LF = CR • LF

         ;; CR LF | CR | LF
         (define crlf
           (union (concat CR LF)
                  CR
                  LF))

         ;; L* (V+ | LV V* | LVT) T* | L+ | T+
         (define hangul-syllable
           (union (concat (repeat L)
                          (union (repeat+1 V)
                                 (concat LV (repeat V))
                                 LVT)
                          (repeat T))
                  (repeat+1 L)
                  (repeat+1 T)))

         ;; RI RI
         (define RI-sequence (concat REGIONAL-INDICATOR REGIONAL-INDICATOR))

         ;; Extended_Pictographic (Extend* ZWJ Extended_Pictographic)*
         (define xpicto-sequence
           (concat EXTENDED-PICTOGRAPHIC
                   (repeat (concat (repeat EXTEND)
                                   ZWJ
                                   EXTENDED-PICTOGRAPHIC))))

         ;; Consonant (Extend* Linker (Extend|Linker)* Consonant)+
         (define conjunct-cluster
           (concat INDIC-CONSONANT
                   (repeat+1
                    (concat (repeat INDIC-EXTEND)
                            INDIC-LINKER
                            (repeat
                             (union INDIC-EXTEND
                                    INDIC-LINKER))
                            INDIC-CONSONANT))))

         ;; [^Control CR LF] ≈ ¬(Control + CR + LF)
         (define not-control
           (negate (union CONTROL CR LF)))

         ;; - compose -

         (define precore PREPEND)

         (define core
           (union hangul-syllable
                  RI-sequence
                  xpicto-sequence
                  conjunct-cluster
                  not-control))

         (define postcore
           (union EXTEND
                  ZWJ
                  SPACING-MARK))

         ;; crlf | Control | precore* core postcore*
         (define extended-grapheme-cluster
           (union crlf
                  CONTROL
                  (concat (repeat precore)
                          core
                          (repeat postcore))))

         ;; === Compiler ===

         ;; (compile regex) -> dfa
         (define compile
           (lambda (regex)
             (define FAIL-STATE EMPTY)
             (define START-STATE regex)
             (define STRIDE TOTAL-PROPERTIES)

             ;; transitions: (hashtable state (hashtable symbol state))
             ;;            ≈ (state, symbol) -> state
             (define transitions
               (let ([table (make-equal-hashtable)])
                 (hashtable-set! table START-STATE (make-eq-hashtable))
                 table))
             (define transitions-has-state?
               (lambda (state)
                 (hashtable-contains? transitions state)))
             (define transitions-add-state!
               (lambda (state)
                 (hashtable-set! transitions state (make-eq-hashtable))))
             (define transitions-connect-states!
               (lambda (state-x symbol state-y)
                 (let ([transition (hashtable-ref transitions state-x #f)])
                   (hashtable-set! transition symbol state-y))))

             ;; (enumerate transitions) -> (values transitions states)
             ;;   where transitions = (hashtable state (hashtable symbol state))
             ;;                    -> (hashtable state (pair offset (hashtable symbol state)))
             ;;                         where offset = (* number STRIDE)
             ;;
             ;;         states      = (list fail-state accept-states start-state other-states)
             ;;
             ;; Sorts states into fail-state, accept-states, start-state, and other-states,
             ;; then maps a number to each state within transitions from 0 to total states.
             ;; Returns both the enumerated transitions and list of sorted states.
             (define enumerate
               (lambda (transitions)
                 ;; (sort-states (vector state)) -> (list fail-state accept-states start-state other-states)
                 (define sort-states
                   (lambda (states)
                     (let ([size (vector-length states)])
                       (let loop ([index  0]
                                  [accept '()]
                                  [other  '()])
                         (if (= index size)
                             (append (cons FAIL-STATE accept) (cons START-STATE other))
                             (let ([state (vector-ref states index)]
                                   [next  (+ index 1)])
                               (cond [(eq? state FAIL-STATE)
                                      (loop next accept other)]
                                     [(equal? state START-STATE)
                                      (loop next accept other)]
                                     [else
                                      (if (nullable? state)
                                          (loop next (cons state accept) other)
                                          (loop next accept (cons state other)))])))))))

                 ;; sorted-states = (list fail-state accept-states start-state other-states)
                 (define sorted-states (sort-states (hashtable-keys transitions)))

                 (let loop ([counter 0]
                            [states  sorted-states])
                   (if (null? states)
                       (values transitions sorted-states)
                       (let* ([state      (car states)]
                              [transition (hashtable-ref transitions state #f)])
                         ;; Multiplies a state's number by stride to find its offset.
                         (hashtable-set! transitions state (cons (* counter STRIDE) transition))
                         (loop (+ counter 1) (cdr states)))))))

             ;; --- goto and explore ---
             ;;
             ;; Mutually-recursive functions perform a depth-first traversal of the DFA's
             ;; state graph, ensuring all possible transitions are enumerated.

             ;; (goto state symbol) -> unspecified
             ;;
             ;; Maps a state and symbol to their derived state and adds it to transitions.
             ;; Mutually-recursive with explore. Calls explore on any derived state not
             ;; already in transitions.
             (define goto
               (lambda (state-x symbol)
                 (let ([state-y (derive state-x symbol)])
                   (cond [(transitions-has-state? state-y)
                          (transitions-connect-states! state-x symbol state-y)]
                         [else
                          (transitions-add-state! state-y)
                          (transitions-connect-states! state-x symbol state-y)
                          (explore state-y)]))))

             ;; (explore state) -> transitions
             ;;
             ;; Derives states for every state-symbol pairing in ALPHABET.
             ;; Mutually-recursive with goto. Calls (goto state symbol),
             ;; where symbol ∈ ALPHABET.
             (define explore
               (lambda (state)
                 (let loop ([alphabet ALPHABET])
                   (cond [(null? alphabet)
                          transitions]
                         [else
                          (let ([symbol (car alphabet)]
                                [next   (cdr alphabet)])
                            (goto state symbol)
                            (loop next))]))))

             ;; (build-table transitions sorted-states) -> (vector number)
             ;;
             ;; Encodes a two-dimensional transition table, (state, symbol) -> state,
             ;; onto a one-dimensional vector.
             (define build-table
               (lambda (transitions sorted-states)
                 (define table-size (* (length sorted-states) STRIDE))
                 (define table (make-vector table-size 0))
                 (define build-row!
                   (lambda (state)
                     (let* ([data   (hashtable-ref transitions state #f)]
                            [offset (car data)]
                            [bound  (+ offset STRIDE)]
                            [states (cdr data)])
                       (let loop ([index    offset]
                                  [alphabet ALPHABET])
                         (unless (= index bound)
                           (let* ([symbol      (car alphabet)]
                                  [next-state  (hashtable-ref states symbol #f)]
                                  [next-offset (car (hashtable-ref transitions next-state #f))])
                             (vector-set! table index next-offset)
                             (loop (+ index 1) (cdr alphabet))))))))

                 ;; Skip enumeration of fail-state. Every element in vector is already set to 0.
                 (let loop ([states (cdr sorted-states)])
                   (if (null? states)
                       table
                       (let ([state (car states)]
                             [next  (cdr states)])
                         (build-row! state)
                         (loop next))))))

             (let-values ([(transitions sorted-states)
                           (enumerate (explore START-STATE))])
               (let* ([fail   0]
                      [start  (car (hashtable-ref transitions START-STATE #f))]
                      [accept (cons (+ fail STRIDE)
                                    (if (nullable? START-STATE)
                                        (+ start STRIDE)
                                        start))]
                      [table  (build-table transitions sorted-states)])
                 (make-dfa fail start accept table)))))

         ;; === Deterministic Finite Automaton: Extended Grapheme Clusters ===

         (define DFA (compile extended-grapheme-cluster))

         (define unit-tests
           (test-chunk
            "Builders: DFA"
            ()
            ;; === tests: regex constructors ===
            (test-assert "sanity check"
                         equal?
                         (concat L V LV LVT T)
                         (list CONCAT (list L V LV LVT T)))

            (test-assert "concat: singleton"
                         equal?
                         (concat LF)
                         LF)

            (test-assert "concat: identity"
                         equal?
                         (concat)
                         EPSILON)

            (test-assert "concat: flatten"
                         equal?
                         (concat (concat CR LF) CR (concat CR LF))
                         (list CONCAT (list CR LF CR CR LF)))

            (test-assert "concat: filter"
                         equal?
                         (concat EPSILON CR EPSILON LF)
                         (list CONCAT (list CR LF)))

            (test-assert "concat: break"
                         equal?
                         (concat LF EMPTY CR)
                         EMPTY)

            (test-assert "union: singleton"
                         equal?
                         (union LF)
                         LF)

            (test-assert "union: identity"
                         equal?
                         (union)
                         EMPTY)

            (test-assert "union: flatten"
                         equal?
                         (union CR (union (union CR LF LF) LF))
                         (list UNION (list CR CR LF LF LF)))

            (test-assert "union: filter"
                         equal?
                         (union LF EPSILON CR EMPTY)
                         (list UNION (list LF EPSILON CR)))

            (test-assert "repeat: idempotent"
                         equal?
                         (repeat (repeat (repeat LF)))
                         (list REPEAT LF))

            (test-assert "(repeat EMPTY) = (repeat EPSILON) = EPSILON"
                         equal?
                         (repeat EMPTY)
                         (repeat EPSILON))

            (test-assert "(negate EMPTY) = EPSILON"
                         equal?
                         (negate EMPTY)
                         EPSILON)

            (test-assert "(negate EPSILON) = EMPTY"
                         equal?
                         (negate EPSILON)
                         EMPTY)))

)

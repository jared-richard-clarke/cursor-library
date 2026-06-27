(library (builders dfa)
         (export (rename (unit-tests dfa:unit-tests)))
         (import (rnrs)
                 (unicode grapheme-break constants)
                 (cursor tools))

         ;; === Constants ===

         (enum EMPTY
               EPSILON
               CONCAT
               UNION
               REPEAT
               NEGATE)

         ;; === Data Types ===

         (define-record-type DFA
           (fields start
                   stride
                   match
                   table))

         ;; === Helper Functions ===

         (define build-union
           (lambda (xs)

             (define filter-flatten
               (lambda (xs)
                 (if (null? xs)
                     xs
                     (let ([item (car xs)]
                           [next (cdr xs)])
                       (cond [(empty? item)
                              (filter-flatten next)]
                             [(union? xs)
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

         (define build-concat
           (lambda (xs)

             (define append-reverse
               (lambda (xs ys)
                 (fold-left (lambda (y x) (cons x y)) ys xs)))

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
                       (cond [(empty? item)
                              EMPTY]
                             [(epsilon? item)
                              (filter-flatten-break next output)]
                             [(concat? item)
                              (filter-flatten-break next (append-reverse (cadr item) output))]
                             [else
                              (filter-flatten-break next (cons item output))])))))

             (filter-flatten-break xs)))

         ;; === Brzozowski Derivatives ===

         (define empty?
           (lambda (x)
             (eq? x EMPTY)))

         (define epsilon?
           (lambda (x)
             (eq? x EPSILON)))

         (define literal?
           (lambda (x)
             (and (number? x)
                  (grapheme-break-property? x))))

         (define concat?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) CONCAT))))

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

         (define union?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) UNION))))

         ;; (union x y ...) -> '(UNION (x y ...))
         ;; (union x)       -> x
         ;; (union)         -> 'EMPTY
         (define union
           (lambda xs
             (cond [(null? xs)
                    EMPTY]
                   [(singleton? xs)
                    (car xs)]
                   [else
                    (build-union xs)])))

         (define repeat?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) REPEAT))))

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

         (define repeat+1
           (lambda (x)
             (concat x (repeat x))))

         (define negate?
           (lambda (x)
             (and (pair? x)
                  (eq? (car x) NEGATE))))

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

         (define nullable?
           (lambda (regex)
             (cond [(empty? regex)   #f]
                   [(epsilon regex)  #t]
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
                                         "argument not a regular expression"
                                         regex)])))

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
                    (derive-repeat regex)]
                   [(negate? regex)
                    (derive-negate regex)])))

         (define derive-concat
           (lambda (regex prefix)

             (define sequence (cadr regex))
             (define first    (car sequence))
             (define follow   (cdr sequence))

             (if (nullable? first)
                 (union (apply concat (derive first prefix) follow)
                        (apply concat (derive (car follow) prefix) (cdr follow)))
                 (apply concat (derive first prefix) follow))))

         (define derive-union
           (lambda (regex prefix)

             (define choices (cadr regex))

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

         (define derive-negate
           (lambda (regex prefix)

             (define sub-expression (cadr regex))
             (define derivation     (derive sub-expression prefix))

             (negate expression)))

         ;; === Patterns ===

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

         ;; [^Control CR LF]
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

         (define unit-tests
           (test-chunk
            "DFA Builder"
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

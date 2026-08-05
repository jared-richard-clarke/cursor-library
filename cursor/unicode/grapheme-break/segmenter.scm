(library (cursor unicode grapheme-break segmenter)
         (export segment)
         (import (rnrs)
                 (cursor unicode grapheme-break constants)
                 (cursor unicode grapheme-break builders dfa)
                 (cursor unicode grapheme-break builders code-point-trie))

         ;; === DFA Constants ===

         (define FAIL-START   (car (dfa-fail DFA)))
         (define FAIL-STOP    (cdr (dfa-fail DFA)))
         (define START-STATE  (dfa-start DFA))
         (define ACCEPT-START (car (dfa-accept DFA)))
         (define ACCEPT-STOP  (cdr (dfa-accept DFA)))
         (define TRANSITIONS  (dfa-table DFA))

         ;; === DFA Predicates and Functions ===

         (define fail-state?
           (lambda (x)
             (< x FAIL-STOP)))

         (define accept-state?
           (lambda (x)
             (and (>= x ACCEPT-START)
                  (<  x ACCEPT-STOP))))

         (define next-state
           (lambda (state symbol)
             (vector-ref TRANSITIONS (+ state symbol))))

         ;;            exact integer
         ;;           +--------------+
         ;; high-bits | EP | IB | GB | low-bits
         ;;           +--------------+
         ;; where EP = extended-pictographic
         ;;       IB = indic-break
         ;;       GB = grapheme-break
         ;;
         ;; === Bitwise Constants ===
         
         (define GB-BIT-START 0)
         (define GB-BIT-STOP  GB-TOTAL-BITS)
         
         (define IB-BIT-START GB-TOTAL-BITS)
         (define IB-BIT-STOP  (+ GB-TOTAL-BITS IB-TOTAL-BITS))
         
         (define EP-BIT-START (+ GB-TOTAL-BITS IB-TOTAL-BITS))
         (define EP-BIT-STOP  (+ GB-TOTAL-BITS IB-TOTAL-BITS EP-TOTAL-BITS))

         (define get-grapheme-property
           (lambda (x)
             (bitwise-bit-field x GB-BIT-START GB-BIT-STOP)))

         (define get-indic-property
           (lambda (x)
             (+ (bitwise-bit-field x IB-BIT-START IB-BIT-STOP)
                INDIC-BREAK-OFFSET)))

         (define get-emoji-property
           (lambda (x)
             (+ (bitwise-bit-field x EP-BIT-START EP-BIT-STOP)
                EXTENDED-PICTOGRAPHIC-OFFSET)))

)

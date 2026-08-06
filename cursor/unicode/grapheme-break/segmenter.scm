(library (cursor unicode grapheme-break segmenter)
         (export segment)
         (import (rnrs)
                 (cursor unicode grapheme-break constants)
                 (cursor unicode grapheme-break builders dfa)
                 (cursor unicode grapheme-break builders code-point-trie))

         ;; === DFA Constants ===

         (define FAIL-OFFSET-START   (car (dfa-fail DFA)))
         (define FAIL-OFFSET-STOP    (cdr (dfa-fail DFA)))
         (define START-STATE         (dfa-start DFA))
         (define ACCEPT-OFFSET-START (car (dfa-accept DFA)))
         (define ACCEPT-OFFSET-STOP  (cdr (dfa-accept DFA)))
         (define TRANSITION-TABLE    (dfa-table DFA))

         ;; === DFA Predicates and Functions ===

         (define fail-state?
           (lambda (x)
             (< x FAIL-OFFSET-STOP)))

         (define accept-state?
           (lambda (x)
             (and (>= x ACCEPT-OFFSET-START)
                  (<  x ACCEPT-OFFSET-STOP))))

         (define next-state
           (lambda (state symbol)
             (vector-ref TRANSITION-TABLE (+ state symbol))))

         ;;            exact integer
         ;;           +--------------+
         ;; high-bits | EP | IB | GB | low-bits
         ;;           +--------------+
         ;; where EP = extended-pictographic
         ;;       IB = indic-break
         ;;       GB = grapheme-break
         ;;
         ;; === Bitwise Constants ===
         
         (define GB-BIT-OFFSET-START 0)
         (define GB-BIT-OFFSET-STOP  GB-TOTAL-BITS)
         
         (define IB-BIT-OFFSET-START GB-TOTAL-BITS)
         (define IB-BIT-OFFSET-STOP  (+ GB-TOTAL-BITS IB-TOTAL-BITS))
         
         (define EP-BIT-OFFSET-START (+ GB-TOTAL-BITS IB-TOTAL-BITS))
         (define EP-BIT-OFFSET-STOP  (+ GB-TOTAL-BITS IB-TOTAL-BITS EP-TOTAL-BITS))

         (define IB-BIT-SHIFT GB-TOTAL-BITS)
         (define EP-BIT-SHIFT (+ GB-TOTAL-BITS IB-TOTAL-BITS))

         (define get-grapheme-property
           (lambda (x)
             (bitwise-bit-field x GB-BIT-OFFSET-START GB-BIT-OFFSET-STOP)))

         (define get-indic-property
           (lambda (x)
             (+ (bitwise-bit-field x IB-BIT-OFFSET-START IB-BIT-OFFSET-STOP)
                INDIC-BREAK-OFFSET)))

         (define get-emoji-property
           (lambda (x)
             (+ (bitwise-bit-field x EP-BIT-OFFSET-START EP-BIT-OFFSET-STOP)
                EXTENDED-PICTOGRAPHIC-OFFSET)))

)

(library (cursor unicode grapheme-break segmenter)
         (export segment)
         (import (rnrs)
                 (cursor unicode grapheme-break builders dfa)
                 (cursor unicode grapheme-break builders code-point-tree))

         ;; === Constants ===
         
         (define FAIL-START   (car (dfa-fail DFA)))
         (define FAIL-STOP    (cdr (dfa-fail DFA)))
         (define START-STATE  (dfa-start DFA))
         (define ACCEPT-START (car (dfa-accept DFA)))
         (define ACCEPT-STOP  (cdr (dfa-accept DFA)))
         (define TRANSITIONS  (dfa-table DFA))

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

)

(library (constants)
         (export OTHER
                 CR
                 LF
                 CONTROL
                 L
                 V
                 LV
                 LVT
                 T
                 REGIONAL-INDICATOR
                 PREPEND
                 EXTEND
                 ZWJ
                 SPACING-MARK
                 INDIC-CONSONANT
                 INDIC-EXTEND
                 INDIC-LINKER
                 EXTENDED-PICTOGRAPHIC
                 TOTAL-EXTENDED-GBP)
         (import (rnrs))

         ;; === Extended Grapheme Break Properties ===

         (define OTHER                  0)
         (define CR                     1)
         (define LF                     2)
         (define CONTROL                3)
         (define L                      4)
         (define V                      5)
         (define LV                     6)
         (define LVT                    7)
         (define T                      8)
         (define REGIONAL-INDICATOR     9)
         (define PREPEND               10)
         (define EXTEND                11)
         (define ZWJ                   12)
         (define SPACING-MARK          13)
         (define INDIC-CONSONANT       14)
         (define INDIC-EXTEND          15)
         (define INDIC-LINKER          16)
         (define EXTENDED-PICTOGRAPHIC 17)

         (define TOTAL-EXTENDED-GBP 18)
)

(library (cursor unicode grapheme-break constants)
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
                 TOTAL-GRAPHEME-BREAKS
                 TOTAL-INDIC-BREAKS
                 TOTAL-EXTENDED-PICTOGRAPHICS
                 TOTAL-PROPERTIES
                 ALPHABET
                 grapheme-break-property?
                 grapheme-break->string
                 grapheme-break->constant)
         (import (rnrs)
                 (cursor tools))

         ;; === Extended Grapheme Break Properties ===

         ;; - Grapheme Break Properties -
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
         ;; - Indic Break Properties -
         (define INDIC-CONSONANT       14)
         (define INDIC-EXTEND          15)
         (define INDIC-LINKER          16)
         ;; - Extended Pictographic Property -
         (define EXTENDED-PICTOGRAPHIC 17)

         (define TOTAL-GRAPHEME-BREAKS        14)
         (define TOTAL-INDIC-BREAKS            3)
         (define TOTAL-EXTENDED-PICTOGRAPHICS  1)
         (define TOTAL-PROPERTIES             18)

         (define ALPHABET
           (list OTHER
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
                 EXTENDED-PICTOGRAPHIC))

         (define grapheme-break-property?
           (lambda (x)
             (and (>= x 0) (< x TOTAL-PROPERTIES))))

         ;; Maps enumerated constants to their strings.
         (define grapheme-break-string-table
           '#("Other"
              "CR"
              "LF"
              "Control"
              "L"
              "V"
              "LV"
              "LVT"
              "T"
              "Regional_Indicator"
              "Prepend"
              "Extend"
              "ZWJ"
              "SpacingMark"
              "InCB; Consonant"
              "InCB; Extend"
              "InCB; Linker"
              "Extended_Pictographic"))

         (define grapheme-break->string
           (lambda (x)
             (unless (grapheme-break-property? x)
               (assertion-violation 'grapheme-break->string
                                     "not a grapheme-break"
                                     x))
               (vector-ref grapheme-break-string-table x)))

         ;; Maps strings to their enumerated constants.
         (define grapheme-break-constant-table
           (string-hashtable ("Other"                 OTHER)
                             ("CR"                    CR)
                             ("LF"                    LF)
                             ("Control"               CONTROL)
                             ("L"                     L)
                             ("V"                     V)
                             ("LV"                    LV)
                             ("LVT"                   LVT)
                             ("T"                     T)
                             ("Regional_Indicator"    REGIONAL-INDICATOR)
                             ("Prepend"               PREPEND)
                             ("Extend"                EXTEND)
                             ("ZWJ"                   ZWJ)
                             ("SpacingMark"           SPACING-MARK)
                             ("InCB; Consonant"       INDIC-CONSONANT)
                             ("InCB; Extend"          INDIC-EXTEND)
                             ("InCB; Linker"          INDIC-LINKER)
                             ("Extended_Pictographic" EXTENDED-PICTOGRAPHIC)))

         (define grapheme-break->constant
           (lambda (x)
             (let ([constant (hashtable-ref grapheme-break-constant-table x #f)])
               (unless constant
                 (assertion-violation 'grapheme-break-name->constant
                                      "not a grapheme-break"
                                      x))
               constant)))
)

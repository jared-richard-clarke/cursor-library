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
          INDIC-LINKER-AND-EXTEND
          INDIC-LINKER-SUB-EXTEND
          EXTENDED-PICTOGRAPHIC
          INDIC-LINKER
          OTHER-STRING
          CR-STRING
          LF-STRING
          CONTROL-STRING
          L-STRING
          V-STRING
          LV-STRING
          LVT-STRING
          T-STRING
          REGIONAL-INDICATOR-STRING
          PREPEND-STRING
          EXTEND-STRING
          ZWJ-STRING
          SPACING-MARK-STRING
          INDIC-CONSONANT-STRING
          INDIC-EXTEND-STRING
          INDIC-LINKER-STRING
          EXTENDED-PICTOGRAPHIC-STRING
          ALPHABET
          ALPHABET-LENGTH
          grapheme-break-property?
          string->grapheme-break)
  (import (rnrs)
          (cursor tools))

  ;; === Enumerated Constants ===

  ;; - Extended Grapheme Break Properties -
  (define OTHER                    0)
  (define CR                       1)
  (define LF                       2)
  (define CONTROL                  3)
  (define L                        4)
  (define V                        5)
  (define LV                       6)
  (define LVT                      7)
  (define T                        8)
  (define REGIONAL-INDICATOR       9)
  (define PREPEND                 10)
  (define EXTEND                  11)
  (define ZWJ                     12)
  (define SPACING-MARK            13)
  ;; - Indic Break Properties -
  (define INDIC-CONSONANT         14)
  (define INDIC-EXTEND            15)
  ;; Linker ∩ Extend
  (define INDIC-LINKER-AND-EXTEND 16)
  ;; Linker \ Extend
  (define INDIC-LINKER-SUB-EXTEND 17)
  ;; - Extended Pictographic Property -
  (define EXTENDED-PICTOGRAPHIC   18)

  ;; - Indic Linker Property -
  ;;
  ;; A distinct indic-conjunct break property that in a DFA
  ;; must either include or exclude the Extend property.
  (define INDIC-LINKER 19)

  ;; === Grapheme Break Identifier Strings ===

  (define OTHER-STRING                 "Other")
  (define CR-STRING                    "CR")
  (define LF-STRING                    "LF")
  (define CONTROL-STRING               "Control")
  (define L-STRING                     "L")
  (define V-STRING                     "V")
  (define LV-STRING                    "LV")
  (define LVT-STRING                   "LVT")
  (define T-STRING                     "T")
  (define REGIONAL-INDICATOR-STRING    "Regional_Indicator")
  (define PREPEND-STRING               "Prepend")
  (define EXTEND-STRING                "Extend")
  (define ZWJ-STRING                   "ZWJ")
  (define SPACING-MARK-STRING          "SpacingMark")
  (define INDIC-CONSONANT-STRING       "InCB; Consonant")
  (define INDIC-EXTEND-STRING          "InCB; Extend")
  (define INDIC-LINKER-STRING          "InCB; Linker")
  (define EXTENDED-PICTOGRAPHIC-STRING "Extended_Pictographic")

  ;; === DFA Alphabet ===

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
          INDIC-LINKER-AND-EXTEND
          INDIC-LINKER-SUB-EXTEND
          EXTENDED-PICTOGRAPHIC))

  (define ALPHABET-LENGTH (length ALPHABET))

  ;; === Grapheme Break Predicate ===
  
  (define grapheme-break-set
    (let ([table    (make-eqv-hashtable)]
          [set-list (list OTHER
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
                          EXTENDED-PICTOGRAPHIC)])
      (fold-left (lambda (accum property)
                   (hashtable-set! accum property #t)
                   accum)
                 table
                 set-list)))

  (define grapheme-break-property?
    (lambda (x)
      (and (number? x)
           (hashtable-contains? grapheme-break-set x))))

  ;; === Grapheme Break Identifier String Conversion ===

  ;; Maps grapheme-break identifier strings to their enumerated constants.
  (define grapheme-break-string-table
    (string-hashtable (OTHER-STRING                 OTHER)
                      (CR-STRING                    CR)
                      (LF-STRING                    LF)
                      (CONTROL-STRING               CONTROL)
                      (L-STRING                     L)
                      (V-STRING                     V)
                      (LV-STRING                    LV)
                      (LVT-STRING                   LVT)
                      (T-STRING                     T)
                      (REGIONAL-INDICATOR-STRING    REGIONAL-INDICATOR)
                      (PREPEND-STRING               PREPEND)
                      (EXTEND-STRING                EXTEND)
                      (ZWJ-STRING                   ZWJ)
                      (SPACING-MARK-STRING          SPACING-MARK)
                      (INDIC-CONSONANT-STRING       INDIC-CONSONANT)
                      (INDIC-EXTEND-STRING          INDIC-EXTEND)
                      (INDIC-LINKER-STRING          INDIC-LINKER)
                      (EXTENDED-PICTOGRAPHIC-STRING EXTENDED-PICTOGRAPHIC)))

  (define string->grapheme-break
    (lambda (x)
      (let ([constant (hashtable-ref grapheme-break-string-table x #f)])
        (unless constant
          (assertion-violation 'string->grapheme-break
                               "not a grapheme-break"
                               x))
        constant)))
)

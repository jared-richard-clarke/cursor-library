;; === Cursor ===
;;
;; A pattern-matching and parsing library based on Parsing Expression Grammars
;; by Bryan Ford and the LPeg parsing machine by Roberto Ierusalimschy
;; and Sérgio Medeiros.
;;
;; === Pattern Matchers ===
;; +-------------------------------------+
;; | PEG     | Cursor                    |
;; |---------+---------------------------|
;; | ε       | empty                     |
;; | .       | any                       |
;; | "x"     | #\x or "x"                |
;; | p?      | (maybe p)                 |
;; | p*      | (repeat p)                |
;; | p+      | (repeat+1 p)              |
;; | &p      | (is? p)                   |
;; | !p      | (is-not? p)               |
;; | px py   | (and-then px py)          |
;; | px / py | (or-else px py)           |
;; | [xyz]   | (one-of "xyz")            |
;; | [^xyz]  | (none-of "xyz")           |
;; | id <- p | (grammar [id p])          |
;; +-------------------------------------+
;;
;; === Captures and Transformations ===
;; +-------------------------------------+
;; | Cursor                              |
;; |-------------------------------------|
;; | (capture p) or (capture function p) |
;; | (transform function p)              |
;; +-------------------------------------+
(library (cursor)
  (export empty
          any
          and-then
          or-else
          maybe
          repeat
          repeat+1
          is?
          is-not?
          one-of
          none-of
          grammar
          rule
          capture
          transform
          compile)
  (import (cursor core)
          (cursor compiler)))

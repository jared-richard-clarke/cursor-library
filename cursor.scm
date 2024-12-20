;; === Cursor: A Parsing Expression Grammar Library ===
;;
;;  --------------------------------------------
;; | Cursor                   | PEG             |
;; |--------------------------+-----------------|
;; | empty                    | ε               |
;; |--------------------------+-----------------|
;; | any                      | .               |
;; |--------------------------+-----------------|
;; | (char #\x)               | "x"             |
;; |--------------------------+-----------------|
;; | (maybe p)                | p?              |
;; |--------------------------+-----------------|
;; | (repeat p)               | p*              |
;; |--------------------------+-----------------|
;; | (repeat+1 p)             | p+              |
;; |--------------------------+-----------------|
;; | (is? p)                  | &p              |
;; |--------------------------+-----------------|
;; | (is-not? p)              | !p              |
;; |--------------------------+-----------------|
;; | (sequence px py)         | px py           |
;; |--------------------------+-----------------|
;; | (choice px py)           | px / py         |
;; |--------------------------+-----------------|
;; | (one-of "xyz")           | [xyz]           |
;; |--------------------------+-----------------|
;; | (none-of "xyz")          | [^xyz]          |
;; |--------------------------+-----------------|
;; | (grammar [rule pattern]) | rule <- pattern |
;;  --------------------------------------------

(library (cursor)
         (export empty
                 fail
                 any
                 char
                 sequence
                 choice
                 maybe
                 repeat
                 repeat+1
                 is?
                 is-not?
                 one-of
                 none-of
                 call
                 grammar
                 capture
                 text)
         (import (cursor core)))

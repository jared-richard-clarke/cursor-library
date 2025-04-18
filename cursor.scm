;; === Cursor ===
;;
;; A pattern-matching and parsing library based on Parsing Expression Grammars
;; by Bryan Ford and the LPeg parsing machine by Roberto Ierusalimschy
;; and Sérgio Medeiros.
;;
;; === Pattern Matchers ===
;; +------------------------------------+
;; | PEG     | Cursor                   |
;; |---------+--------------------------|
;; | ε       | empty                    |
;; | .       | any                      |
;; | "x"     | (char #\x) or (text "x") |
;; | p?      | (maybe p)                |
;; | p*      | (repeat p)               |
;; | p+      | (repeat+1 p)             |
;; | &p      | (is? p)                  |
;; | !p      | (is-not? p)              |
;; | px py   | (and-then px py)         |
;; | px / py | (or-else px py)          |
;; | [xyz]   | (one-of "xyz")           |
;; | [^xyz]  | (none-of "xyz")          |
;; | id <- p | (grammar [id p])         |
;; +------------------------------------+
;;
;; === Captures and Transformations ===
;; +-------------------------------------+
;; | Cursor                              |
;; |-------------------------------------|
;; | (capture p) or (capture function p) |
;; | (transform function p)              |
;; +-------------------------------------+
(library (cursor)
          ;; === empty ===
          ;;
          ;; A parsing expression that always succeeds without
          ;; consuming any input.
  (export empty
          ;; === any ===
          ;;
          ;; A parsing expression that matches and consumes any
          ;; character. Fails only on empty inputs.
          any
          ;; === (char #\x) ===
          ;;
          ;; A parsing expression that matches and consumes
          ;; the provided character.
          char
          ;; === (and-then px py ...) ===
          ;;
          ;; Sequences zero or more parsing expressions. Each expression
          ;; must match part of the input. If any one expression fails,
          ;; the entire sequence fails, consuming no input.
          ;;
          ;; "and-then" with zero arguments produces the "empty" expression,
          ;; the identity element for sequences.
          and-then
          ;; === (or-else px py ...) ===
          ;;
          ;; Ordered choice with limited backtracking. Succeeds on the
          ;; first matching expression of zero or more parsing expressions.
          ;; For each failing expression, backtracks to the original input
          ;; position and tries the subsequent expression.
          ;;
          ;; "or-else" with zero arguments produces the "fail" expression,
          ;; the identity element for choices. A "fail" expression unconditionally
          ;; fails on all inputs.
          or-else
          ;; === (maybe px) ===
          ;;
          ;; Matches zero or one repetitions of its subexpression.
          ;; Consumes as much input as possible, never backtracking.
          maybe
          ;; === (repeat px) ===
          ;;
          ;; Matches zero or more repetitions of its subexpression.
          ;; Consumes as much input as possible, never backtracking.
          repeat
          ;; === (repeat+1 px) ===
          ;;
          ;; Matches one or more repetitions of its subexpression.
          ;; Consumes as much input as possible, never backtracking.
          repeat+1
          ;; === (is? px) ===
          ;;
          ;; The and-predicate provides unlimited lookahead. Success or failure
          ;; of this expression matches its subexpression. In either case,
          ;; consumes no input.
          is?
          ;; === (is-not? px) ===
          ;;
          ;; The not-predicate provides unlimited lookahead. Success or failure
          ;; is the inverse of its subexpression. In either case, consumes no input.
          is-not?
          ;; === (one-of "xyz") ===
          ;;
          ;; Transforms a non-empty string into a character set.
          ;; An empty string produces ∅, the empty set. This set
          ;; will fail on all inputs.
          one-of
          ;; === (none-of "xyz") ===
          ;;
          ;; Transforms a non-empty string into a character set that acts as
          ;; U, the universal set, minus the provided characters.
          ;; An empty string simply produces the universal set, which succeeds
          ;; on all inputs.
          ;;
          ;; In this context, the universal set is all characters as provided
          ;; by R6RS — particularly Chez Scheme.
          none-of
          ;; === (grammar [id px] ...) ===
          ;;
          ;; Allows the full expression of Parsing Expression Grammars.
          ;; Each grammar must contain one or more rules, where a rule
          ;; consists of an identifier and its associated parsing expression.
          ;; Each rule can contain both references to itself and other rules
          ;; within the grammar, allowing the construction of recursive patterns.
          grammar
          ;; === (rule id) ===
          ;;
          ;; Allows a parsing expression to refer to another parsing expression,
          ;; including itself, within its enclosing grammar. Its subexpression
          ;; must be a symbol that identifies a rule defined within the grammar.
          rule
          ;; === (capture px) or (capture fn px) ===
          ;; where fn = (list char ...) -> any
          ;;
          ;; Pushes a list of characters matched by the subexpression onto a stack.
          ;; This stack will later be returned to the caller. An optional function
          ;; is applied to the capture before it is pushed onto the stack.
          capture
          ;; === (transform fn px) ===
          ;; where fn = stack -> any
          ;;
          ;; Captures operate by pushing their values onto a stack implemented as a cons list.
          ;; Capturing expression "A" and then "B" places the stack in state "(list B A)".
          ;; Through the function "fn", "transform" provides direct access to the stack state
          ;; as computed by its subexpression "px".
          ;;
          ;; Function "fn" can transform said state arbitrarily, although transformations
          ;; compose best by maintaining a stack discipline. In other words,
          ;; fn = stack -> stack, where stack = (list x y ...).
          transform
          ;; === (text "xyz") ==
          ;;
          ;; Transforms a string into a sequence of character matches.
          text
          ;; === (compile px) ===
          ;;
          ;; Transforms a parsing expression into a parsing function, which runs a match
          ;; over a string and returns one of four results:
          ;;
          ;; 1. Boolean true for match.
          ;;
          ;; 2. Boolean false for non-match.
          ;;
          ;; 3. A list of captured character matches.
          ;;
          ;; 4. Arbitrary values that have been captured as character matches and then
          ;;    transformed by associated functions.
          compile)
  (import (cursor core)
          (cursor compiler)))

(import (rnrs)
        ;; Tests core components of Cursor library.
        (only (cursor core) core:unit-tests)
        (only (cursor collections charset) charset:unit-tests)
        (only (cursor compiler) compiler:unit-tests)
        ;; Examples double as integration tests.
        (only (examples arithmetic) arithmetic:tests)
        (only (examples csv) csv:tests)
        (only (examples json) json:tests)
        (only (examples match) match:tests)
        ;; Test grapheme-break components.
        (only (cursor unicode grapheme-break builders dfa)
              dfa:unit-tests))

(define run-tests
  (lambda xs
    (let ([run (lambda (fn) (fn))])
      (for-each run xs))))

(define help
  (lambda ()
    (display "usage: tests.scm [ --core | --unicode | --all ]")
    (newline)))

(define command-default "")
(define command-core    "--core")
(define command-unicode "--unicode")
(define command-all     "--all")

(define command (cadr (command-line)))

(cond [(or (string=? command command-default)
           (string=? command command-core))
       (run-tests core:unit-tests
                  charset:unit-tests
                  compiler:unit-tests
                  arithmetic:tests
                  csv:tests
                  json:tests
                  match:tests)]
      [(string=? command command-unicode)
       (run-tests dfa:unit-tests)]
      [(string=? command command-all)
       (run-tests core:unit-tests
                  charset:unit-tests
                  compiler:unit-tests
                  arithmetic:tests
                  csv:tests
                  json:tests
                  match:tests)]
      [else
       (help)])

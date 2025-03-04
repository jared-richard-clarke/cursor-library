(import (rnrs)
        (only (cursor core) core:unit-tests)
        (only (cursor collections charset) charset:unit-tests)
        (only (cursor compiler) compiler:unit-tests)
        (only (examples arithmetic) arithmetic:tests)
        (only (examples csv) csv:tests)
        (only (examples json) json:tests))

(define run-tests
  (lambda xs
    (let ([run (lambda (fn) (fn))])
      (for-each run xs))))

(run-tests core:unit-tests
           charset:unit-tests
           compiler:unit-tests
           arithmetic:tests
           csv:tests
           json:tests)

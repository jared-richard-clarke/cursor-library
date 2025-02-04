(import (rnrs)
        (only (cursor core) core:unit-tests)
        (only (cursor compiler) compiler:unit-tests))

(define run-tests
  (lambda xs
    (let ([run (lambda (fn) (fn))])
      (for-each run xs))))

(run-tests core:unit-tests
           compiler:unit-tests)

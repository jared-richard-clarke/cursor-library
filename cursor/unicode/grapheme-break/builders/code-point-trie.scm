(library (builders code-point-trie)
         (export)
         (import (rnrs)
                 (unicode grapheme-break-constants)
                 (cursor tools))

         ;; === Unicode Character Database: Side Notes ===
         ;;
         ;; - All data files are encoded in UTF-8. Unless otherwise noted,
         ;;   non-ASCII characters appear only in comments.
         ;;
         ;; - All data files use LF line termination.
         ;;
         ;; - Code points are expressed as hexadecimal numbers with four to six digits.
         ;;   They are written without the "U+" prefix in all data files except the
         ;;   Unihan data files.

         (define CODEC (utf-8-codec))
         (define EOL-STYLE (eol-style lf))
         ;; Raise exception on encoding errors. Lookup tables must not be corrupted.
         (define ERROR-HANDLING-MODE (error-handling-mode raise))
         (define TRANSCODER (make-transcoder CODEC EOL-STYLE ERROR-HANDLING-MODE))

         ;; === Data Types ===

         (define-record-type code-point-trie
           (fields table-1
                   table-2
                   table-3))

         ;; === Input/Output ===

         ;; (open-file string) -> textual-input-port
         ;;
         ;; Opens a textual input port.
         (define open-file
           (lambda (path)
             (open-file-input-port path (file-options) (buffer-mode block) TRANSCODER)))

         ;; === Parsers ===

         (define hex-string->number
           (lambda (text)
             (string->number text 16)))

         (define skip-whitespace
           (lambda (text anchor text-length)
             (let loop ([index anchor])
               (cond [(= index text-length)
                      index]
                     [(char=? #\space (string-ref text index))
                      (loop (+ index 1))]
                     [else
                      index]))))

         ;; (find pattern) -> function
         ;;   where pattern  = string
         ;;         function = string -> boolean
         ;;
         ;; Inputs a pattern and outputs an unanchored, non-backtracking,
         ;; pattern-matching function, tailored to finding a match within
         ;; a string derived from a Unicode data file.
         (define find
           (lambda (pattern)
             (let ([pattern-start (string->list pattern)])
               (lambda (string)
                 (let ([start 0]
                       [stop (string-length string)]
                       [hash #\#])
                   (let loop ([pattern pattern-start]
                              [index   start])
                     (cond [(null? pattern)
                            #t]
                           [(= index stop)
                            #f]
                           [else
                            (let ([x (car pattern)]
                                  [y (string-ref string index)])
                              (cond [(char=? y hash)
                                     #f]
                                    [(char=? x y)
                                     (loop (cdr pattern) (+ index 1))]
                                    [else
                                     (loop pattern-start (+ index 1))]))])))))))
)

(library (cursor unicode grapheme-break builders code-point-trie)
         (export (rename (unit-tests code-point-trie:unit-tests)))
         (import (rnrs)
                 (cursor unicode grapheme-break constants)
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

         ;; === Constants ===

         (define PERIOD         #\.)
         (define SEMICOLON      #\;)
         (define HASH           #\#)
         (define SPACE          #\space)
         (define HEX-DIGITS     "0123456789abcdefABCDEF")
         (define ASCII-LETTERS  "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (define CODE-POINT-MAX #x10FFFF)

         (define CODEC (utf-8-codec))
         (define EOL-STYLE (eol-style lf))
         ;; Raise exception on encoding errors. Lookup tables must not be corrupted.
         (define ERROR-HANDLING-MODE (error-handling-mode raise))
         (define TRANSCODER (make-transcoder CODEC EOL-STYLE ERROR-HANDLING-MODE))

         ;; === Data Types ===

         ;; record: (code-point-data start stop property)
         ;;           where start    = number
         ;;                 stop     = number
         ;;                 property = number, grapheme-break constant
         ;;
         ;; Data field from within the unicode database.
         (define-record-type code-point-data
           (fields start
                   stop
                   property))

         ;; record: (code-point-trie table-1 table-2 table-3)
         ;;           where table-1 = (vector number)
         ;;                 table-2 = (vector number)
         ;;                 table-3 = (vector number)
         ;;
         ;; A prefix trie, implented as a multi-stage lookup table.
         (define-record-type code-point-trie
           (fields table-1
                   table-2
                   table-3))

         ;; record: (charset table)
         ;;           where table = (hash-table char boolean)
         ;;
         ;; A character set with O(1) lookup.
         (define-record-type charset
           (fields table)
           (protocol
            (lambda (new)
              (lambda (text)
                (let* ([characters (string->list text)]
                       [capacity   (length characters)]
                       [hashtable  (make-eqv-hashtable capacity)])
                  (new (fold-left (lambda (accum x)
                                    (hashtable-set! accum x #t)
                                    accum)
                                  hashtable
                                  characters)))))))

         ;; (charset-has? char) -> boolean
         ;;
         ;; Membership testing of a given character within a given character set.
         (define charset-has?
           (lambda (self x)
             (let ([table (charset-table self)])
               (hashtable-contains? table x))))

         ;; === Input/Output ===

         ;; (open-file string) -> textual-input-port
         ;;
         ;; Opens a textual input port.
         (define open-file
           (lambda (path)
             (open-file-input-port path (file-options) (buffer-mode block) TRANSCODER)))

         ;; === Parser Combinators ===
         ;;
         ;; To enable composition, all combinators create parsers that share the same signature:
         ;;
         ;; parser = (procedure match? text index stop captures) -> (values match? text index stop captures)
         ;;   where match?   = boolean
         ;;         text     = string
         ;;         index    = integer
         ;;         stop     = integer
         ;;         captures = (list string)
         ;;
         ;; match?:   Flags either a match or non-match for the given input.
         ;; text:     The given input.
         ;; index:    The current index in the given input.
         ;; stop:     The length and terminus of the given input.
         ;; captures: A list of substrings copied from the given input.
         ;;
         ;; Side Note: (repeat px) where (nullable? px) = #t will create
         ;;            an infinitely-repeating parser. Nullable expressions
         ;;            are (repeat px), (maybe px), and (and-then) — expressions
         ;;            that can match an input without consuming it.

         ;; (match-with predicate) -> parser | raise exception
         ;;   where predicate = char | (procedure char) -> boolean
         (define match-with
           (lambda (x)
             
             (define predicate
               (cond [(char? x)
                      (lambda (y) (char=? x y))]
                     [(procedure? x)
                      x]
                     [else
                      (assertion-violation 'match-with "neither a procedure nor char" x)]))

             (lambda (match? text index stop captures)
               (cond [(= index stop)
                      (values #f text index stop captures)]
                     [(predicate (string-ref text index))
                      (values #t text (+ index 1) stop captures)]
                     [else
                      (values #f text index stop captures)]))))

         ;; (and-then parser ...) -> parser
         ;; (and-then) ≈ ε
         ;;
         ;; Sequences zero or more parsers. If any one parser fails,
         ;; the entire sequence fails, consuming no input.
         ;;
         ;; A sequence of zero parsers is equivalent to epsilon,
         ;; meaning the parser will always succeed, consuming no input.
         (define and-then
           (lambda parsers
             (lambda (match? text index stop captures)
               
               (define index-reset    index)
               (define captures-reset captures)
               
               (define loop
                 (lambda (parsers match? text index stop captures)
                   (if (null? parsers)
                       (values #t text index stop captures)
                       (let ([px (car parsers)])
                         (let-values ([(match? text next stop captures)
                                       (px match? text index stop captures)])
                           (if match?
                               (loop (cdr parsers) match? text next stop captures)
                               (values #f text index-reset stop captures-reset)))))))
               
               (loop parsers match? text index stop captures))))

         ;; (maybe parser) -> parser
         ;;
         ;; Matches zero or one repetitions of the given parser.
         (define maybe
           (lambda (px)
             (lambda (match? text index stop captures)
               (let-values ([(match? text next stop captures)
                             (px match? text index stop captures)])
                 (if match?
                     (values #t text next stop captures)
                     (values #t text index stop captures))))))

         ;; (repeat parser) -> parser
         ;;
         ;; Matches zero or more repetitions of the given parser.
         (define repeat
           (lambda (px)
             (define loop
               (lambda (match? text index stop captures)
                 (let-values ([(match? text next stop captures)
                               (px match? text index stop captures)])
                   (if match?
                       (loop match? text next stop captures)
                       (values #t text index stop captures)))))
             loop))

         ;; Matches one or more repetitions of the given parser.
         (define repeat+1
           (lambda (px)
             (and-then px (repeat px))))

         ;; (capture parser) -> parser
         ;;
         ;; Copies the matching substring and pushes it onto a captures list,
         ;; saving it for return after an overall match.
         (define capture
           (lambda (px)
             (lambda (match? text index stop captures)
               (let-values ([(match? text next stop captures)
                             (px match? text index stop captures)])
                 (if (and match? (< index next))
                     (let ([captures (cons (substring text index next) captures)])
                       (values #t text next stop captures))
                     (values #f text index stop captures))))))

         ;; (run parser text) -> captures | boolean
         ;;   where text     = string
         ;;         captures = (list string)
         ;;
         ;; Runs a parser over a string, returning either a list
         ;; of captured substrings or a boolean, reporting either
         ;; a match or non-match.
         (define run
           (lambda (parser text)
             (let ([stop (string-length text)])
               (let-values ([(match? text next stop captures)
                             (parser #f text 0 stop '())])
                 (if match?
                     (if (null? captures)
                         #t
                         (reverse captures))
                     #f)))))

         ;; === Parsers and Predicates ===
         
         (define string->hex
           (lambda (text)
             (string->number text 16)))         

         (define hex-digit?
           (let ([set (make-charset HEX-DIGITS)])
             (lambda (x)
               (charset-has? set x))))

         (define letter?
           (let ([set (make-charset ASCII-LETTERS)])
             (lambda (x)
               (charset-has? set x))))

         (define whitespace     (repeat (match-with SPACE)))

         (define period         (match-with PERIOD))
         
         (define semicolon      (match-with SEMICOLON))

         (define hex-digit      (match-with hex-digit?))

         (define letters        (repeat+1 (match-with letter?)))
         
         (define range-operator (and-then period period))

         (define code-point
           (capture
            (and-then hex-digit
                      hex-digit
                      hex-digit
                      hex-digit
                      (maybe
                       (and-then hex-digit
                                 (maybe hex-digit))))))

         (define code-points
           (and-then code-point (maybe (and-then range-operator code-point))))

         (define property
           (capture (and-then letters (maybe (and-then whitespace semicolon whitespace letters)))))

         (define field-parser
           (and-then whitespace code-points whitespace semicolon whitespace property))

         ;; (parse-field line) -> (code-point-data start stop property) | #f | raise exception
         ;;   where line = string
         ;;
         ;; Parses a given string, either returning a code-point-data record from a match,
         ;; or boolean false on a non-match. Will raise exceptions on unexpected inputs
         ;; and outputs.
         (define parse-field
           (lambda (line)
             
             (define output (run field-parser line))

             (define raise-assertion
               (lambda (message)
                 (assertion-violation 'parse-field message output)))
             
             (cond [(eq? output #f)
                    #f]
                   [(list? output)
                    (let ([size (length output)])
                      (cond [(= size 2)
                             (let ([start    (string->hex (list-ref output 0))]
                                   [property (grapheme-break->constant (list-ref output 1))])
                               (make-code-point-data start start property))]
                            [(= size 3)
                             (let ([start    (string->hex (list-ref output 0))]
                                   [stop     (string->hex (list-ref output 1))]
                                   [property (grapheme-break->constant (list-ref output 2))])
                               (make-code-point-data start stop property))]
                            [else
                             (raise-assertion "output must contain 2 to 3 fields")]))]
                   [else
                    (raise-assertion "unexpected output")])))
         
         ;; (find-string pattern) -> scanner | raise exception
         ;;   where pattern = string
         ;;         scanner = (procedure port) -> boolean
         ;;
         ;; Inputs a pattern and outputs an unanchored, non-backtracking,
         ;; pattern-matching function, tailored to finding a match within
         ;; a textual-input port.
         (define find-string
           (lambda (pattern)
             
             (define pattern-reset (string->list pattern))
             
             (lambda (port)
               (unless (and (textual-port? port) (input-port? port))
                 (assertion-violation 'find-string-procedure "not a textual input port" port))
               (let loop ([pattern pattern-reset])
                 (cond [(null? pattern)
                        #t]
                       [(port-eof? port)
                        #f]
                       [else
                        (let ([x (car pattern)]
                              [y (get-char port)])
                          (if (char=? x y)
                              (loop (cdr pattern))
                              (loop pattern-reset)))])))))

         ;; === Scanners ===
         ;;
         ;; The files DerivedCoreProperties.txt and emoji-data.txt contain
         ;; many properties not needed for grapheme cluster segmentation.
         ;; The following two procedures are designed to quickly scan for
         ;; the head of a desired section, reducing the workload
         ;; for subsequent parsers.
         ;;
         ;; If a scan returns boolean true, the port will have advanced
         ;; past the terminus of the matching string. If a scan returns
         ;; boolean false, the port will have exhausted its contents.

         ;; (indic-conjunct-header? port) -> boolean
         ;;   where port = textual-input port
         (define indic-conjunct-header?
           (find-string "# Derived Property: Indic_Conjunct_Break\n"))

         ;; (extended-pictographic-header? port) -> boolean
         ;;   where port = textual-input port
         (define extended-pictographic-header?
           (find-string "# All omitted code points have Extended_Pictographic=No\n"))

         (define unit-tests
           (test-chunk
            "Codepoint Trie"
            ([parser-state (lambda xs xs)]
             [catch-parser-state
              (lambda (parser text)
                (let ([stop (string-length text)])
                  (let-values ([(match? text index stop captures)
                                (parser #f text 0 stop '())])
                    (parser-state match? text index stop captures))))]
             [false?
              (lambda (x)
                (and (boolean? x) (eq? x #f)))]
             [code-point-data-equal?
              (lambda (x y)
                (and (code-point-data? x)
                     (code-point-data? y)
                     (let ([start-x    (code-point-data-start x)]
                           [start-y    (code-point-data-start y)]
                           [stop-x     (code-point-data-stop x)]
                           [stop-y     (code-point-data-stop y)]
                           [property-x (code-point-data-property x)]
                           [property-y (code-point-data-property y)])
                       (equal? (list start-x stop-x property-x)
                               (list start-y stop-y property-y)))))])
            
            (test-assert "parser combinator: code-points, single"
                         equal?
                         (catch-parser-state code-points "002B")
                         (parser-state #t "002B" 4 4 '("002B")))
            
            (test-assert "parser combinator: code-points, double"
                         equal?
                         (catch-parser-state code-points "003C..003E")
                         (parser-state #t "003C..003E" 10 10 '("003E" "003C")))
            
            (test-assert "parser combinator: property"
                         equal?
                         (catch-parser-state property "Prepend")
                         (parser-state #t "Prepend" 7 7 '("Prepend")))
            
            (test-assert "parser combinator: property, indic"
                         equal?
                         (catch-parser-state property "InCB; Consonant")
                         (parser-state #t "InCB; Consonant" 15 15 '("InCB; Consonant")))
            
            (test-assert "parser combinator: field-parser #1"
                         equal?
                         (catch-parser-state field-parser "11A84..11A89 ; Prepend")
                         (parser-state #t "11A84..11A89 ; Prepend" 22 22 '("Prepend" "11A89" "11A84")))
            
            (test-assert "parser combinator: field-parser #2"
                         equal?
                         (catch-parser-state field-parser "11082 ; SpacingMark")
                         (parser-state #t "11082 ; SpacingMark" 19 19 '("SpacingMark" "11082")))

            (test-assert "parser-combinator: field-parser #3"
                         equal?
                         (catch-parser-state field-parser "0D4D ; InCB; Linker")
                         (parser-state #t "0D4D ; InCB; Linker" 19 19 '("InCB; Linker" "0D4D")))

            (test-assert "parse-field #1"
                         code-point-data-equal?
                         (parse-field "094D ; InCB; Linker # Mn DEVANAGARI SIGN VIRAMA")
                         (make-code-point-data 2381 2381 INDIC-LINKER))

            (test-assert "parse-field #2"
                         code-point-data-equal?
                         (parse-field "0949..094C ; SpacingMark # Mc [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU")
                         (make-code-point-data 2377 2380 SPACING-MARK))))
)

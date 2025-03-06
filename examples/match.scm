(library (examples match)
         (export (prefix (rgb?
                          IPv4?
                          peg?)
                         match:))
         (import (rnrs)
                 (cursor)
                 (cursor tools))

         (define fullstop
           (lambda (px)
             (and-then px (is-not? any))))

         (define separate-by
           (lambda (px sep)
             (and-then px (repeat (and-then sep px)))))

         (define ws (repeat (one-of " \t\r\n")))
         
         (define trim
           (lambda (px)
             (and-then ws px ws)))

         ;; === RGB Tuple ===

         ;; +-----------+
         ;; | 2 | 5 | 5 |
         ;; +-----------+
         ;;   ^   ^---^
         ;; head  tail
         (define rgb-code (let* ([head (one-of "12")]
                                 [tail (one-of "012345")]
                                 [code (and-then (maybe head) (maybe tail) tail)])
                            (trim code)))
         
         (define rgb-list
           (let ([comma (char #\,)])
             (and-then rgb-code
                       comma
                       rgb-code
                       comma
                       rgb-code)))
         
         (define rgb (and-then (text "rgb") (char #\() rgb-list (char #\))))

         (define rgb? (compile (fullstop (trim rgb))))

         ;; === IPv4 ===

         (define byte
           (let ([one       (char #\1)]
                 [two       (char #\2)]
                 [five      (char #\5)]
                 [zero-four (one-of "01234")]
                 [zero-five (one-of "012345")]
                 [non-zero  (one-of "123456789")])
             (or-else (and-then two five zero-five)
                      (and-then two zero-four digit)
                      (and-then one digit digit)
                      (and-then non-zero digit)
                      digit)))

         (define IPv4
           (let ([dot (char #\.)])
             (and-then byte dot byte dot byte dot byte)))

         (define IPv4? (compile (fullstop (trim IPv4))))

         ;; === PEG ASCII Syntax ===
         ;;
         ;; As defined by Bryan Ford.

         ;; --- Lexical Syntax ---
         
         (define eol
           (let ([n (char #\newline)]
                 [r (char #\return)])
             (or-else (and-then r n)
                      n
                      r)))
         (define space
           (or-else (char #\space)
                    (char #\tab)
                    eol))
         
         (define comment
           (and-then (char #\#)
                     (repeat (and-then (is-not? eol) any))
                     eol))
         
         (define spacing
           (repeat (or-else space comment)))

         (define left-arrow    (and-then (text "<-") spacing))
         (define slash         (and-then (char #\/) spacing))
         (define predicate-and (and-then (char #\&) spacing))
         (define predicate-not (and-then (char #\!) spacing))
         (define question      (and-then (char #\?) spacing))
         (define star          (and-then (char #\.) spacing))
         (define plus          (and-then (char #\+) spacing))
         (define open          (and-then (char #\() spacing))
         (define close         (and-then (char #\)) spacing))
         (define dot           (and-then (char #\.) spacing))

         (define character
           (let ([escape     (and-then (char #\\) (char #\\))]
                 [zero-two   (one-of "012")]
                 [zero-seven (one-of "01234567")])
             (or-else (and-then escape
                                (or-else (one-of "nrt'\"[]\\")
                                         (and-then zero-two zero-seven zero-seven)
                                         (and-then zero-seven (maybe zero-seven))))
                      (and-then (is-not? escape) any))))

         (define range
           (or-else (and-then character (char #\-) character)
                    character))

         (define class
           (and-then (char #\[)
                     (repeat (and-then (is-not? (char #\]))
                                       range))
                     (char #\])
                     spacing))

         (define literal
           (let ([template
                  (lambda (px sep)
                    (and-then sep
                              (repeat (and-then (is-not? sep) px))
                              sep
                              spacing))])
             (or-else (template character (char #\'))
                      (template character (char #\")))))

         (define identifier
           (let* ([start  (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")]
                  [follow (or-else start (one-of "0123456789"))])
             (and-then start (repeat follow) spacing)))

         ;; --- Hierarchical Syntax ---

         (define peg-definition
           (grammar [Definition (and-then identifier left-arrow (rule Expression))]
                    [Expression (separate-by (rule Sequence) slash)]
                    [Sequence   (repeat (rule Prefix))]
                    [Prefix     (and-then (maybe (or-else predicate-and
                                                          predicate-not))
                                          (rule Suffix))]
                    [Suffix     (and-then (rule Primary)
                                          (maybe (or-else question
                                                          star
                                                          plus)))]
                    [Primary    (or-else (and-then identifier (is-not? left-arrow))
                                         (and-then open (rule Expression) close)
                                         literal
                                         class
                                         dot)]))

         (define peg-grammar
           (fullstop (and-then spacing (repeat+1 peg-definition))))

         (define peg? (compile peg-grammar))

)

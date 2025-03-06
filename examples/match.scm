(library (examples match)
         (export (prefix (real-number?
                          english-letter?
                          swedish-letter?
                          greek-letter?
                          rgb?
                          IPv4?)
                         match:))
         (import (rnrs)
                 (cursor))

         (define fullstop
           (lambda (px)
             (and-then px (is-not? any))))

         (define ws (repeat (one-of " \t\r\n")))
         
         (define trim
           (lambda (px)
             (and-then ws px ws)))

         ;; === Real Number ===

         (define digit    (one-of "0123456789"))
         (define digits   (repeat+1 digit))
         (define sign     (or-else (char #\-) (char #\+)))
         (define whole    (or-else (char #\0) digits))
         (define integer  (and-then (repeat sign) whole))
         (define fraction (and-then (char #\.) digits))
         (define exponent (and-then (or-else (char #\e)
                                             (char #\E))
                                    (maybe sign)
                                    digits))
         (define real     (and-then integer
                                    (maybe fraction)
                                    (maybe exponent)))

         (define real-number? (compile (fullstop (trim real))))

         ;; === Alphabets ===
         
         (define english-letter (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
         (define swedish-letter (or-else english-letter (one-of "åäöÅÄÖ")))
         (define greek-letter   (one-of "αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"))

         (define english-letter? (compile (fullstop (trim english-letter))))
         (define swedish-letter? (compile (fullstop (trim swedish-letter))))
         (define greek-letter?   (compile (fullstop (trim greek-letter))))

         ;; === RGB ===
         
         (define rgb-code (let* ([limit
                                  (one-of "12")]
                                 [code
                                  (or-else (char #\0)
                                           (and-then (maybe limit) (maybe digit) digit))])
                            (trim code)))
         (define rgb-codes
           (let ([comma (char #\,)]
                 [slash (char #\/)])
             (and-then rgb-code
                       comma
                       rgb-code
                       comma
                       rgb-code
                       (maybe (and-then slash rgb-code)))))
         
         (define rgb (and-then (text "rgb") (char #\() rgb-codes (char #\))))

         (define rgb? (compile (fullstop (trim rgb))))

         ;; === IPv4 ===
         
         (define zero-four (one-of "01234"))
         (define zero-five (one-of "012345"))
         (define non-zero  (one-of "123456789"))
         (define byte
           (let ([one  (char #\1)]
                 [two  (char #\2)]
                 [five (char #\5)])
             (or-else (and-then two five zero-five)
                      (and-then two zero-four digit)
                      (and-then one digit digit)
                      (and-then non-zero digit)
                      digit)))
         (define dot (char #\.))
         (define IPv4
           (and-then byte dot byte dot byte dot byte))

         (define IPv4? (compile (fullstop (trim IPv4))))

)

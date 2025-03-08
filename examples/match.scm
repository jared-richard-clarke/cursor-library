(library (examples match)
         (export rgb?
                 IPv4?
                 peg?
                 (rename (tests match:tests)))
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

         (define digit (one-of "0123456789"))

         (define byte
           (or-else (and-then (char #\2) (char #\5) (one-of "012345"))
                    (and-then (char #\2) (one-of "01234") digit)
                    (and-then (char #\1) digit digit)
                    (and-then (one-of "123456789") digit)
                    digit))

         ;; === RGB Tuple ===
         ;;
         ;; rgb([0-255], [0-255], [0-255])
         
         (define rgb-list
           (let ([comma    (char #\,)]
                 [rgb-code (trim byte)])
             (and-then rgb-code
                       comma
                       rgb-code
                       comma
                       rgb-code)))
         
         (define rgb (and-then (text "rgb") (char #\() rgb-list (char #\))))

         (define rgb? (compile (fullstop (trim rgb))))

         ;; === IPv4 ===
         ;;
         ;; 172.192.244.138
         ;;
         ;; +-----+-----+-----+-----+
         ;; | 172 | 192 | 244 | 138 |
         ;; +-----+-----+-----+-----+
         ;; ^-----------------------^
         ;;     32 bits / 4 bytes
         
         (define IPv4
           (let ([dot (char #\.)])
             (and-then byte dot byte dot byte dot byte)))

         (define IPv4? (compile (fullstop (trim IPv4))))

         ;; === PEG ASCII Syntax ===
         ;;
         ;; Source: "Parsing Expression Grammars: A Recognition-Based Syntactic Foundation"
         ;; Author: Bryan Ford

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

         (define tests
           (test-chunk
            
            "Match"
            
            ([samples-eq?
              (lambda (xs ys)
                (for-all eq? xs ys))]

             [all-true  '(#t #t #t #t)]
             
             [all-false '(#f #f #f #f)]
             
             [samples-rgb:true   '("rgb(0,0,0)"
                                   "rgb( 0, 100, 255 )"
                                   "rgb(255, 255, 255)"
                                   "rgb(45,  25 ,125  )")]

             [samples-rgb:false  '("rgb (0,0,0)"
                                   "(0, 0, 0)"
                                   "rgb(0, 0, 0"
                                   "rgb(2555, 255, 255)")]
             
             [samples-IPv4:true  '("234.201.51.223"
                                   "198.147.209.2"
                                   "203.222.97.55"
                                   "73.117.188.186")]

             [samples-IPv4:false '("300.88.0.1"
                                   "255.8a.3.4"
                                   "100.25.11"
                                   "23420151223")]

             [samples-PEG:true    '("S <- 'abc' / . S"
                                    "S <- [a-c]+"
                                    "A <- 'a' B\n B <- 'b' C\n C <- 'c'"
                                    "S <- 'a' !.")]

             [samples-PEG:false   '("S -> 'abc' / . S"
                                    "S <- [a-c]+\n 42"
                                    "S <- 'abc"
                                    "'abc'")])

            (test-assert "rgb true"
                         samples-eq?
                         all-true
                         (map rgb? samples-rgb:true))

            (test-assert "rgb false"
                         samples-eq?
                         all-false
                         (map rgb? samples-rgb:false))

            (test-assert "IPv4 true"
                         samples-eq?
                         all-true
                         (map IPv4? samples-IPv4:true))

            (test-assert "IPv4 false"
                         samples-eq?
                         all-false
                         (map IPv4? samples-IPv4:false))

            (test-assert "PEG true"
                         samples-eq?
                         all-true
                         (map peg? samples-PEG:true))

            (test-assert "PEG false"
                         samples-eq?
                         all-false
                         (map peg? samples-PEG:false))

            ))

)

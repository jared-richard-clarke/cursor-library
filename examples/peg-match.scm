(library (examples peg-match)
         (export (peg?))
         (import (rnrs)
                 (cursor))

         (define abc "abcdefghijklmnopqrstuvwxyz")

         (define whitespace (repeat (one-of " \t\n")))

         (define fullstop
           (lambda (px)
             (and-then px (is-not? any))))

         (define separate-by
           (lambda (px sep)
             (and-then px (repeat (and-then sep px)))))
         
         (define literal (and-then (char #\")
                                   (repeat (none-of "\""))
                                   (char #\")
                                   whitespace))

         (define character-class
           (and-then (char #\[)
                     (repeat (and-then (is-not? (char #\]))
                                       (or-else (and-then any (char #\-) any)
                                                any)))
                     (char #\])
                     whitespace))

         (define non-terminal
           (repeat+1 (one-of (string-append abc (string-upcase abc)))))

         (define peg-grammar
           (fullstop
            (grammar [PEG             (repeat+1
                                       (and-then (rule Non-Terminal)
                                                 (text "<-")
                                                 whitespace
                                                 (rule Pattern)))]
                     
                     [Pattern         (and-then (rule Choice)
                                                (repeat
                                                 (and-then (char #\/)
                                                           whitespace
                                                           (rule Choice))))]

                     [Choice          (repeat+1
                                       (and-then (maybe (one-of "!&"))
                                                 whitespace
                                                 (rule Suffix)))]

                     [Suffix          (and-then (rule Primary)
                                                (repeat
                                                 (and-then (one-of "*+?")
                                                           whitespace)))]

                     [Primary         (and-then
                                       (or-else (and-then (char #\()
                                                          whitespace
                                                          (rule Pattern)
                                                          (char #\))
                                                          whitespace)
                                                (and-then (char #\.)
                                                          whitespace)
                                                (rule Literal)
                                                (rule Character-Class)
                                                (rule Non-Terminal))
                                       (is-not? (text "<-")))]

                     [Literal         literal]
                     
                     [Character-Class character-class]
                     
                     [Non-Terminal    non-terminal])))

         (define peg? (compile peg-grammar))
)

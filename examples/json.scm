(library (examples json)
         (export parse-json)
         (import (rnrs)
                 (cursor))

         (define separate-by
           (lambda (x sep)
             (and-then x (repeat (and-then sep x)))))
         
         (define whitespace (repeat (one-of " \n\r\t")))

         (define digit (one-of "0123456789"))

         (define digits (repeat+1 digit))

         (define json-true  (text "true"))
         (define json-false (text "false"))
         (define json-null  (text "null"))

         (define json-character (and-then (is? (none-of "\"/\b\f\n\r\t")) any))

         (define json-characters (repeat+1 json-character))

         (define json-string (and-then (char #\") json-characters (char #\")))

         (define sign        (maybe (or-else (char #\+) (char #\-))))
         (define exponent    (maybe (and-then (or-else (char #\e)
                                                       (char #\E))
                                              sign
                                              digits)))
         (define fractional  (maybe (and-then (char #\.) digits)))
         (define whole       (or-else (char #\0)
                                      (and-then (one-of "123456789")
                                                (repeat digit))))
         (define integer     (and-then sign whole))

         (define json-number (and-then integer fractional exponent))

         (define json-grammar
           (grammar [Element (and-then whitespace (rule Value) whitespace)]
                    
                    [Value   (or-else (rule Object)
                                      (rule Array)
                                      (rule String)
                                      (rule Number)
                                      (rule True)
                                      (rule False)
                                      (rule Null))]
                    
                    [Object   (and-then (char #\{)
                                        (or-else (rule Members) whitespace)
                                        (char #\}))]
                    
                    [Members  (separate-by (rule Member) (char #\,))]
                    
                    [Member   (and-then whitespace
                                        (rule String)
                                        whitespace
                                        (char #\:)
                                        (rule Element))]
                    
                    [Array    (and-then (char #\[)
                                        (or-else (rule Elements) whitespace)
                                        (char #\]))]

                    [Elements (separate-by (rule Element) (char #\,)]
                    
                    [String   json-string]
                    
                    [Number   json-number]
                    
                    [True     json-true]
                    
                    [False    json-false]
                    
                    [Null     json-null]))

         (define parse-json (compile json-grammar))

)

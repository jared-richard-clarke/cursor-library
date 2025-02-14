(library (examples json)
         (export parse-json)
         (import (rnrs)
                 (cursor))

         (define NULL 'NULL)

         (define separate-by
           (lambda (x sep)
             (sequence x (repeat sep x))))
         
         (define whitespace (repeat (one-of " \n\r\t")))

         (define digit (one-of "0123456789"))

         (define digits (repeat+1 digit))

         (define hex (choice digit (one-of "ABCDEFabcdef")))

         (define escape (choice (one-of "\"\/\b\f\n\r\t")
                                (sequence (char #\u) hex hex hex hex)))

         (define json-true  (capture (lambda (xs state) #t)
                                     (text "true")))
         (define json-false (capture (lambda (xs state) #f)
                                     (text "false")))
         (define json-null  (capture (lambda (xs state) NULL)
                                     (text "null")))

         (define json-character (sequence (is-not? (choice (char #\")
                                                           (sequence (char #\\)
                                                                     escape)))
                                          any))

         (define json-characters (repeat+1 json-character))

         (define json-string (sequence (char #\")
                                       (capture (lambda (xs state) (list->string xs))
                                                json-characters)
                                       (char #\")))

         (define json-number
           (capture (lambda (xs state)
                      (string->number (list->string xs) 10))
                    
                    (grammar [Real       (sequence (rule Integer) (rule Fractional) (rule Exponent))]
                             
                             [Integer    (sequence (rule Sign) (rule Whole))]
                             
                             [Whole      (choice (char #\0)
                                                 (sequence (one-of "123456789") (repeat digit)))]
                             
                             [Fractional (maybe (sequence (char #\.) digits))]
                             
                             [Exponent   (maybe (sequence (choice (char #\e) (char #\E))
                                                          (rule Sign)
                                                          digits))]
                             
                             [Sign       (maybe (choice (char #\+) (char #\-)))])))

         (define json-grammar
           (grammar [Element (sequence whitespace (rule Value) whitespace)]
                    
                    [Value   (choice (rule Object)
                                     (rule Array)
                                     (rule String)
                                     (rule Number)
                                     (rule True)
                                     (rule False)
                                     (rule Null))]
                    
                    [Object   (sequence (char #\{)
                                        (choice (rule Members) whitespace)
                                        (char #\}))]
                    
                    [Members  (separate-by (rule Member) (char #\,))]
                    
                    [Member   (sequence whitespace
                                        (rule String)
                                        whitespace
                                        (char #\:)
                                        (rule Element))]
                    
                    [Array    (sequence (char #\[)
                                        (choice (rule Elements) whitespace)
                                        (char #\]))]

                    [Elements (separate-by (rule Element) (char #\,)]
                    
                    [String   json-string]
                    
                    [Number   json-number]
                    
                    [True     json-true]
                    
                    [False    json-false]
                    
                    [Null     json-null]))

         (define parse-json (compile json-grammar))

)

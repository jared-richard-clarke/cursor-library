(library (examples json)
         (export parse-json)
         (import (rnrs)
                 (cursor))

         (define NULL 'NULL)
         
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

         (define json-string (capture (lambda (xs state) (list->string xs))
                                     (sequence (char #\")
                                               json-characters
                                               (char #\"))))

         (define json-number
           (capture (lambda (xs state)
                      (string->number (list->string xs) 10))
                    
                    (grammar [Real       (sequence (call Integer)
                                                   (call Fractional)
                                                   (call Exponent))]
                             [Integer    (sequence (call Sign)
                                                   (call Whole))]
                             [Whole      (choice (char #\0)
                                                 (sequence (one-of "123456789")
                                                           (repeat digit)))]
                             [Fractional (maybe (sequence (char #\.)
                                                          digits))]
                             [Exponent   (maybe (sequence (choice (char #\e)
                                                                  (char #\E))
                                                          (call Sign)
                                                          digits))]
                             [Sign       (maybe (choice (char #\+)
                                                        (char #\-)))])))

         (define json-grammar
           (grammar [Element (sequence whitespace
                                       (call Value)
                                       whitespace)]
                    
                    [Value (choice (call Object)
                                   (call Array)
                                   (call String)
                                   (call Number)
                                   (call True)
                                   (call False)
                                   (call Null))]
                    
                    [Object (choice (sequence (char #\{)
                                              (choice (call Members)
                                                      whitespace)
                                              (char #\})))]
                    
                    [Members (sequence (call Member)
                                       (repeat (sequence (char #\,)
                                                         (call Member))))]
                    
                    [Member (sequence whitespace
                                      (call String)
                                      whitespace
                                      (char #\:)
                                      (call Element))]
                    
                    [Array (sequence (char #\[)
                                     (choice (call Elements)
                                             whitespace)
                                     (char #\]))]

                    [Elements (sequence (call Element)
                                        (repeat (sequence (char #\,)
                                                          (call Element))))]
                    
                    [String json-string]
                    
                    [Number json-number]
                    
                    [True json-true]
                    
                    [False json-false]
                    
                    [Null json-null]))

         (define parse-json (compile json-grammar))

)

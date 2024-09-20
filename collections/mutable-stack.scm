(library (collections mutable-stack)
         (export make-stack
                 stack?
                 stack-length
                 stack-capacity
                 stack-pop!
                 stack-drop!
                 stack-push!
                 stack-peek
                 stack-reset!)
         (import (rnrs))
         ;; === stack ===

         (define STACK-UNDERFLOW  "no more elements on stack")
         (define INVALID-SIZE     "capacity is a negative or inexact number")
         (define TYPE             'stack)
         (define DEFAULT-CAPACITY 8)
         (define STACK-BASE       0)
         (define GROWTH-FACTOR    2)
         (define INCREMENT        1)
         (define DECREMENT        (- INCREMENT))

         (define (increment x) (+ x INCREMENT))
         (define (decrement x) (+ x DECREMENT))

         (define (grow x) (* x GROWTH-FACTOR))

         (define copy-buffer
           (lambda (xs n)
             (let ([ys     (make-vector n)]
                   [length (vector-length xs)])
               (let loop ([index  0])
                 (cond [(>= index length) ys]
                       [else
                        (vector-set! ys index (vector-ref xs index))
                        (loop (+ index 1))])))))

         (define-record-type stack
           (fields (mutable buffer)   ;; vector
                   (mutable capacity) ;; non-negative integer
                   (mutable cursor))  ;; non-negative integer
           (nongenerative)
           (sealed #t)
           (protocol
            (lambda (new)
              (case-lambda
                [()  (new (make-vector DEFAULT-CAPACITY) ;; default buffer
                          DEFAULT-CAPACITY               ;; default capacity
                          STACK-BASE)]                   ;; cursor
                [(n) (if (or (<= n STACK-BASE) (inexact? n))
                         (assertion-violation TYPE INVALID-SIZE n)
                         (new (make-vector n)     ;; custom buffer
                              n                   ;; custom capacity
                              STACK-BASE))]))))   ;; cursor

         (define stack-length
           (lambda (self)
             (stack-cursor self)))

         (define stack-capacity
           (lambda (self)
             (stack-capacity self)))

         (define stack-pop!
           (lambda (self)
             (let ([buffer (stack-buffer self)]
                   [cursor (decrement (stack-cursor self))])
               (cond [(< cursor STACK-BASE)
                      ;; Report stack underflow.
                      (assertion-violation TYPE STACK-UNDERFLOW)]
                     [else
                      ;; Decrement stack by one place.
                      (stack-cursor-set! self cursor)
                      ;; Return popped element.
                      (vector-ref buffer cursor)]))))

         (define stack-drop!
           (lambda (self)
             (let ([buffer (stack-buffer self)]
                   [cursor (decrement (stack-cursor self))])
               (cond [(< cursor STACK-BASE)
                      ;; Return the stack without modification.
                      self]
                     [else
                      ;; Decrement stack by one place.
                      (stack-cursor-set! self cursor)
                      ;; Return the stack with modification.
                      self]))))

         (define stack-push!
           (lambda (self x)
             (let ([buffer   (stack-buffer   self)]
                   [capacity (stack-capacity self)]
                   [cursor   (stack-cursor   self)])
               (cond [(>= cursor capacity)
                      (let ([new-capacity (grow capacity)])
                        ;; Copy values over into larger buffer.
                        (stack-buffer-set! self (copy-buffer buffer new-capacity))
                        ;; Set new capacity.
                        (stack-capacity-set! self new-capacity)
                        ;; Increment the stack by one place.
                        (stack-cursor-set! self (increment cursor))
                        ;; Push new value onto the stack.
                        (vector-set! (stack-buffer self) cursor x)
                        ;; Return the stack.
                        self)]
                     [else
                      ;; Increment the stack by one place.
                      (stack-cursor-set! self (increment cursor))
                      ;; Push new value onto the stack.
                      (vector-set! (stack-buffer self) cursor x)
                      ;; Return the stack.
                      self]))))

         (define stack-peek
           (lambda (self)
             (let ([cursor (decrement (stack-cursor self))]
                   [buffer (stack-buffer self)])
               (if (< cursor STACK-BASE)
                   ;; Report stack underflow.
                   (assertion-violation TYPE STACK-UNDERFLOW)
                   ;; Return top element without popping stack.
                   (vector-ref buffer cursor)))))

         (define stack-reset!
           (lambda (self)
             (let ([cursor (stack-cursor self)])
               (cond [(= cursor STACK-BASE) self]
                     [else
                      (stack-cursor-set! self STACK-BASE)
                      self]))))
         )

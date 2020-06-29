#lang rosette/safe

(provide (all-defined-out))

(struct boolean-type () #:transparent)
(struct integer-type (signed bitwidth) #:transparent)
(struct fixed-bytes-type (width) #:transparent)
(struct address-type () #:transparent)
(struct function-type (arg-type-list ret-type-list) #:transparent)
(struct ref-type (type) #:transparent)
(struct struct-type (something) #:transparent)
(struct mapping-type (key value) #:transparent)
(struct type-parameter (symbol) #:transparent)

(define (get-symbolic-boolean)
  (define-symbolic* x boolean?)
  x)

(define (get-symbolic-integer bitwidth)
  (define-symbolic* x (bitvector bitwidth))
  x)

(define (get-symbolic-address)
  (define-symbolic* x (bitvector 160))
  x)



                 





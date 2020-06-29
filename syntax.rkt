#lang rosette/safe

(provide (all-defined-out))

(struct contract-decl (name body) #:transparent)

(struct contract-member-decl (type name initialval) #:transparent)

(struct arg-decl (type name) #:transparent)
(struct func-decl (name args retvals body) #:transparent)
(struct constructor-decl (args body) #:transparent)

(struct modifier-decl (name args body) #:transparent)

(struct local-decl-stmt (type name initialval) #:transparent)
(struct if-stmt (condition then else) #:transparent)
(struct func-call-stmt (addr name args) #:transparent)
(struct extract-var-stmt (name) #:transparent)
(struct extract-address-stmt (addr name) #:transparent)

(struct deref-pointer-stmt (ptr) #:transparent)
(struct extract-pointer-stmt (ptr name) #:transparent)

(struct assign-var-stmt (name val) #:transparent)
(struct assign-address-stmt (addr name val) #:transparent)
(struct assign-member-stmt (ptr name val) #:transparent)


(struct return-stmt (val) #:transparent)
(struct binary-builtin-expr (op lhs rhs) #:transparent)
(struct unary-builtin-expr (op operand) #:transparent)
(struct sequence-stmt (lst) #:transparent)

(struct new-slot-stmt (type) #:transparent)
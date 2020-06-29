#lang rosette/safe

(require "syntax.rkt")
(require "env.rkt")
(require "type.rkt")
(require rosette/lib/match)
(provide (all-defined-out))
                
(define (compile-func-call call)
  (match call
    [(func-call-stmt addr name args)
     (define compiled-addr (compile-body addr))
     (define compiled-args (map compile-body args))
     (define (iter e compiled-args)
       (if (null? compiled-args)
           (cons '() e)
           (let* ([cur (car compiled-args)]
                  [rest (cdr compiled-args)]
                  [e1 (cur e)]
                  [r (env-get-retval e1)]
                  [erest (iter e1 rest)]
                  [lst (car erest)]
                  [efin (cdr erest)])
             (if (eq? (env-get-status efin) 'rev)
                 (cons '() efin)
                 (cons (cons r lst) efin)))))
     (lambda (e)
       (define func (env-resolve-func e name))

       (let* ([e0 (compiled-addr e)]
              [addr0 (env-get-retval e0)]
              [val (iter e compiled-args)]
              [e1 (cdr val)]
              [arglist (car val)]
              [e2 (func (env-set-args (env-push-stack e1 addr0) arglist))]
              [status (env-get-status e2)]
              [val (env-get-retval e2)])

         (if (eq? status 'rev)
             (env-set-status (env-set-retval (env-pop-stack e2) val) 'rev)
             (env-set-status (env-set-retval (env-pop-stack e2) val) 'reg))))]))
  

(define (compile-local-decl ldecl)
  (define type (local-decl-stmt-type ldecl))
  (define name (local-decl-stmt-name ldecl))
  (define compiled-initialval (compile-body (local-decl-stmt-initialval ldecl)))
  (lambda (e)
    (let* ([e1 (compiled-initialval e)]
           [initialval (env-get-retval e1)])
      (env-add-local e1 type name initialval))))

(define (compile-extract-var ext)
  (define name (extract-var-stmt-name ext))
  (lambda (e)
    (env-set-retval e (env-resolve-local e name))))

(define (compile-extract-addr extaddr)
  (define compiled-addr (compile-body (extract-address-stmt-addr extaddr)))
  (define name (extract-address-stmt-name extaddr))
  (lambda (e)
    (let* ([e1 (compiled-addr e)]
           [addr (env-get-retval e1)])
      (env-set-retval e1 (env-resolve-address e1 addr name)))))

(define (compile-deref-pointer dptr)
  (define compiled-dptr (compile-body (deref-pointer-stmt-ptr dptr)))
  (lambda (e)
    (let* ([e1 (compiled-dptr e)]
           [ptr (env-get-retval e1)]
           [ptr-val (ref-ptr ptr)]
           [val (env-resolve-ptr e1 ptr-val)]
           [e2 (env-set-retval e1 val)])
      e2)))

(define (compile-extract-pointer exptr)
  (define compiled-exptr (compile-body (extract-pointer-stmt-ptr exptr)))
  (define name (extract-pointer-stmt-name exptr))
  (lambda (e)
    (let* ([e1 (compiled-exptr e)]
           [ptr (env-get-retval e1)]
           [ptr-val (ref-ptr ptr)]
           [val (env-resolve-ptr-member e1 ptr-val name)]
           [e2 (env-set-retval e1 val)])
      e2)))

(define (compile-assign-member asmem)
  (define compiled-ptr (compile-body (assign-member-stmt-ptr asmem)))
  (define name (assign-member-stmt-name asmem))
  (define compiled-val (compile-body (assign-member-stmt-val asmem)))
  (lambda (e)
    (let* ([e1 (compiled-ptr e)]
           [ptr (env-get-retval e1)]
           [ptr-val (ref-ptr ptr)]
           [e2 (compiled-val e1)]
           [val (env-get-retval e2)])
      (env-set-ptr-member e2 ptr-val name val))))
           
(define (compile-assign-var assign)
  (define name (assign-var-stmt-name assign))
  (define compiled-val (compile-body (assign-var-stmt-val assign)))

  (lambda (e)
    (let* (
           [e1 (compiled-val e)]
           [val (env-get-retval e1)])

      (env-assign-local e1 name val)
      )))

(define (compile-assign-addr assignaddr)
  (define compiled-addr (compile-body (assign-address-stmt-addr assignaddr)))
  (define name (assign-address-stmt-name assignaddr))
  (define compiled-val (compile-body (assign-address-stmt-val assignaddr)))
  (lambda (e)
    (let* ([e1 (compiled-addr e)]
           [addr (env-get-retval e1)]
           [e2 (compiled-val e1)]
           [val (env-get-retval e2)])
      (env-assign-address e2 addr name val))))

(define (compile-return ret)
  (define ret-list (return-stmt-val ret))
  (if (list? ret-list)
      (begin
        (define compiled-ret-list (map compile-body ret-list))
        (define (iter e compiled-ret-list)
          (if (null? compiled-ret-list)
              (cons '() e)
              (let* ([cur (car compiled-ret-list)]
                     [rest (cdr compiled-ret-list)]
                     [e1 (cur e)]
                     [r (env-get-retval e1)]
                     [erest (iter e1 rest)])
                (if (eq? (env-get-status e1) 'rev)
                    (cons '() e)
                    (cons (cons r (car erest)) (cdr erest))))))
        (lambda (e)
          (let ([val (iter e compiled-ret-list)])
            (if (eq? (env-get-status (cadr val)) 'rev)
                (cadr val)
                (env-set-status (env-set-retval (cadr val) (car val)) 'ret)))))
      (begin
        (define compiled-ret-list (compile-body ret-list))

        (lambda (e)
          (let ([newe (compiled-ret-list e)])
            (if (eq? (env-get-status newe) 'rev)
                newe
                (env-set-status newe 'ret)))))))


(define (compile-binary-builtin bin)
  (match bin
    [(binary-builtin-expr op lhs rhs)
     (define compiled-lhs (compile-body lhs))
     (define compiled-rhs (compile-body rhs))
     (cond [(eq? op '+)
            (lambda (e)
              (let* ([e1 (compiled-lhs e)]
                     [lhsval (env-get-retval e1)]
                     [e2 (compiled-rhs e)]
                     [rhsval (env-get-retval e2)])
                (env-set-retval e2 (bvadd lhsval rhsval))))]
           [(eq? op '-)
            (lambda (e)
              (let* ([e1 (compiled-lhs e)]
                     [lhsval (env-get-retval e1)]
                     [e2 (compiled-rhs e)]
                     [rhsval (env-get-retval e2)])
                (env-set-retval e2 (bvsub lhsval rhsval))))]
           [(eq? op '*)
            (lambda (e)
              (let* ([e1 (compiled-lhs e)]
                     [lhsval (env-get-retval e1)]
                     [e2 (compiled-rhs e)]
                     [rhsval (env-get-retval e2)])
                (env-set-retval e2 (bvmul lhsval rhsval))))]
           [(eq? op 'bvor)
            (lambda (e)
              (let* ([e1 (compiled-lhs e)]
                     [lhsval (env-get-retval e1)]
                     [e2 (compiled-rhs e)]
                     [rhsval (env-get-retval e2)])
                (env-set-retval e2 (bvor lhsval rhsval))))]
           [(eq? op '&)
            (lambda (e)
              (let* ([e1 (compiled-lhs e)]
                     [lhsval (env-get-retval e1)]
                     [e2 (compiled-rhs e)]
                     [rhsval (env-get-retval e2)])
                (env-set-retval e2 (bvand lhsval rhsval))))]
           [else (car '())])]))

(define (compile-unary-builtin un)
  (match un
    [(unary-builtin-expr op operand)
     (define compiled-operand (compile-body operand))
     (cond [(eq? op '-)
            (lambda (e)
              (let* ([e1 (compiled-operand e)]
                     [operandval (env-get-retval e1)])
                (env-set-retval e1 (bvneg operandval))))]
           [else (car '())])]))


(define (compile-if ifstmt)
  (define compiled-condition (compile-body (if-stmt-condition ifstmt)))
  (define compiled-then (compile-body (if-stmt-then ifstmt)))
  (define compiled-else (compile-body (if-stmt-else ifstmt)))
  (lambda (e)
    (let* ([e1 (compiled-condition e)]
           [condition (env-get-retval e1)])
      (if condition
          (compiled-then e1)
          (compiled-else e1)))))

(define (compile-new new)
  (define type (new-slot-stmt-type new))
  (lambda (e)
    (let ([cur-ptr-val (env-cur-ptr-val e)])
      (env-set-retval (env-new-heap-var e type (get-default-val type)) (ref cur-ptr-val)))))
          

(define (compile-sequence seq)
  (define compiled-sequence (map compile-body (sequence-stmt-lst seq)))
  (define (apply-seq e cseq)
    (if (null? cseq)
        e
        (let ([new-e ((car cseq) e)])
          (if (not (eq? (env-get-status new-e) 'reg))
              new-e
              (apply-seq new-e (cdr cseq))))))
  (lambda (e) (apply-seq e compiled-sequence)))
    

(define (compile-body body)
  (cond [(sequence-stmt? body) (compile-sequence body)]
        [(return-stmt? body) (compile-return body)]
        [(extract-var-stmt? body) (compile-extract-var body)]
        [(extract-address-stmt? body) (compile-extract-addr body)]
        [(deref-pointer-stmt? body) (compile-deref-pointer body)]
        [(extract-pointer-stmt? body) (compile-extract-pointer body)]
        [(assign-var-stmt? body) (compile-assign-var body)]
        [(assign-address-stmt? body) (compile-assign-addr body)]
        [(assign-member-stmt? body) (compile-assign-member body)]
        [(local-decl-stmt? body) (compile-local-decl body)]
        [(func-call-stmt? body) (compile-func-call body)]
        [(if-stmt? body) (compile-if body)]
        [(new-slot-stmt? body) (compile-new body)]
        [else (car '())]))

(define (compile-function args body)
  (define compiled-body (compile-body body))
  (lambda (e)
    (compiled-body
     (env-set-this (env-bind-args e args)))))

(define (compile-contract env-input contract)

  (define member-decls (filter contract-member-decl? (contract-decl-body contract)))
  (define initial-val
    (append
     (list
      (binding (integer-type #f 256) 'balance (bv 0 (bitvector 256)))
      )
     (map (lambda (x)
            (match x
              [(contract-member-decl type name initialval)
               (binding type name (if (void? initialval) (get-default-val type) initialval))]))
          member-decls)))

  (define contract-name (contract-decl-name contract))
  (define func-decls (filter func-decl? (contract-decl-body contract)))
  (define (compile-functions func-decls)
    (if (null? func-decls)
        '()
        (let ([cur (car func-decls)]
              [rest (cdr func-decls)])
          (match cur
            [(func-decl name args retvals body)
             (cons (binding (function-type (map arg-decl-type args) retvals)
                            (list contract-name name)
                            (compile-function args body))
                   (compile-functions rest))]))))
  (define (constructor)
    (define constructor-in-list (filter constructor-decl? (contract-decl-body contract)))
    (if (null? constructor-in-list)
        (begin
          (define compiled-function
            (compile-function
             '()
             (assign-address-stmt (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                                  'balance (extract-pointer-stmt (extract-var-stmt 'msg) 'val))))
          (lambda (e) (compiled-function (env-set-address-val e (env-get-curraddr e) initial-val))))
        (match (car constructor-in-list)
          [(constructor-decl args body)
           (define compiled-function
             (compile-function
              args
              (sequence-stmt
               (list
                (assign-address-stmt
                 (extract-pointer-stmt
                  (extract-var-stmt 'msg)
                  'sender)
                 'balance
                 (extract-pointer-stmt (extract-var-stmt 'msg) 'val))
                body))))
           (lambda (e)
             (compiled-function (env-set-address-val e (env-get-curraddr e) initial-val)))
           ])))
  (env-append-func-table
   env-input
   (cons (binding (function-type '() '()) (list contract-name 'internal 'constructor) (constructor))
         (compile-functions func-decls))))

(define (insert-builtin-functions env)
  (env-append-func-table
   env
   (list
    (binding (function-type '() '()) 'revert
             (lambda (e) (env-set-status e 'rev)))
    (binding (function-type (list (address-type) (integer-type #f 256)) '())
             'transfer
             (lambda (e)
               (let* ([e1 (env-set-this
                           (env-bind-args e (list (arg-decl (address-type) 'target)
                                                  (arg-decl (integer-type #f 256) 'amount))))]
                      [curraddr (env-get-curraddr e1)]
                      [target (env-resolve-local e1 'target)]
                      [amount (env-resolve-local e1 'amount)]
                      [mybalance (env-resolve-address e1 curraddr 'balance)]
                      [targetbalance (env-resolve-address e1 target 'balance)])
                 (if (bvult mybalance (env-resolve-local e1 'amount))
                     (env-set-status e1 'rev)
                     (env-assign-address 
                      (env-assign-address e1 curraddr 'balance (bvsub mybalance amount))
                      target 'balance (bvadd amount targetbalance))))))
    (binding (function-type (list (boolean-type)) '()) 'assert
             (lambda (e) (if (not (car (env-get-args e))) (env-set-status e 'rev) e)))
    (binding (function-type (list (ref-type (mapping-type (type-parameter 'k) (type-parameter 'v)))
                                  (type-parameter 'k))
                            (list (type-parameter 'v)))
             'mapping-get
             (lambda (e)
               (let ([e1 (env-set-this
                          (env-bind-args
                           e
                           (list (arg-decl
                                  (ref-type
                                   (mapping-type (type-parameter 'k)
                                                 (type-parameter 'v)))
                                  'm)
                                 (arg-decl
                                  (type-parameter 'k)
                                  'k))))])
                 (let* ([r (env-resolve-local e1 'm)]
                        [ptr (ref-ptr r)]
                        [val
                         (resolve-binding-list
                          (env-resolve-ptr e1 ptr)
                          (env-resolve-local e1 'k))])
                   (if (void? val)
                       (env-set-status e 'rev)
                       (env-set-retval e1 val))))))
    (binding (function-type (list (ref-type (mapping-type (type-parameter 'k) (type-parameter 'v)))
                                  (type-parameter 'k)
                                  (type-parameter 'v)) '())
             'mapping-set
             (lambda (e)
               (let ([e1 (env-set-this
                          (env-bind-args
                           e
                           (list (arg-decl
                                  (ref-type
                                   (mapping-type (type-parameter 'k)
                                                 (type-parameter 'v)))
                                  'm)
                                 (arg-decl
                                  (type-parameter 'k)
                                  'k)
                                 (arg-decl
                                  (type-parameter 'v)
                                  'v))))])
                 (let* ([r (env-resolve-local e1 'm)]
                        [ptr (ref-ptr r)])
                   (env-rebind-heap-var
                    e1
                    ptr
                    (rebind-binding-list
                     (env-resolve-ptr e1 ptr)
                     (type-parameter 'v)
                     (env-resolve-local e1 'k)
                     (env-resolve-local e1 'v)))))))
    (binding (function-type (list (ref-type (mapping-type (type-parameter 'k) (type-parameter 'v)))
                                  (type-parameter 'k)) (list (boolean-type)))
             'mapping-exists
             (lambda (e)
               (let ([e1 (env-set-this
                          (env-bind-args
                           e
                           (list (arg-decl
                                  (ref-type
                                   (mapping-type (type-parameter 'k)
                                                 (type-parameter 'v)))
                                  'm)
                                 (arg-decl
                                  (type-parameter 'k)
                                  'k))))])
                 (let* ([r (env-resolve-local e1 'm)])
                   (env-set-retval e1 (not (void? r)))))))
    )))


(define (get-initial-env)
  (insert-builtin-functions (get-empty-env)))
                         

(module+ test
  (displayln
   (compile-contract
    (get-initial-env)
    (contract-decl 'RPS
                   (list
                    (contract-member-decl (address-type) 'p1 (void))
                    (contract-member-decl (address-type) 'p2 (void))
                    (contract-member-decl (integer-type #f 256) 'num1 (void))
                    (contract-member-decl (integer-type #f 256) 'num2 (void))
                    (contract-member-decl (fixed-bytes-type 32) 'hashedHand1 (void))
                    (contract-member-decl (boolean-type) 'revealed1 #f)
                    (contract-member-decl (integer-type #t 8) 'hand1 (void))
                    (contract-member-decl (integer-type #t 8) 'hand2 (void))
                    (func-decl 'test (list (arg-decl (address-type) 'target)) (list)
                               (sequence-stmt (list (return-stmt (extract-var-stmt 'target)))))
                    ))))
  )

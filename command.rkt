#lang rosette/safe

(require "env.rkt")
(require "compile.rkt")
(require "syntax.rkt")
(require "type.rkt")

(define (call env func-name addr args source eth)
  (define func (env-resolve-func env func-name))
  (let ([sourcebalance (env-resolve-address env source 'balance)]
        [targetbalance (env-resolve-address env addr 'balance)])
    (if (bvult sourcebalance eth)
        env
        (let ([executed-env
               (func
                (env-set-status
                 (env-set-args
                  (env-new-heap-var-on-stack
                   (env-push-stack
                    (env-assign-address 
                     (env-assign-address env source 'balance (bvsub sourcebalance eth))
                     addr 'balance (bvadd eth targetbalance))
                    addr)
                   (struct-type '())
                   'msg
                   (list (binding (address-type) 'sender source)
                         (binding (integer-type #f 256) 'val eth))) args)
                 'reg))])
          (if (eq? (env-get-status executed-env) 'rev)
              env
              (env-pop-stack executed-env))))))

(define (deploy env addr name args eth)
  (define func (env-resolve-func env (list name 'internal 'constructor)))
  (let ([executed-env
         (func
          (env-set-status
           (env-set-args
            (env-new-heap-var-on-stack
             (env-push-stack
              env
              addr)
             (struct-type '())
             'msg
             (list (binding (address-type) 'sender addr)
                   (binding (integer-type #f 256) 'val eth)))
            args)
           'reg))])
    (if (eq? (env-get-status executed-env) 'rev)
        env
        (env-pop-stack executed-env))))

(module+ test
  (define env1 (compile-contract (get-initial-env)
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
                              (func-decl 'set-num1 (list (arg-decl (integer-type #f 256) 'val)) (list)
                                         (sequence-stmt (list
                                                         ; uint256 val1 = val
                                                         (local-decl-stmt (integer-type #f 256)
                                                                          'val1
                                                                          (extract-var-stmt 'val))
                                                         ; p1 = this.test(msg.sender)
                                                         (assign-var-stmt 'p1
                                                                          (func-call-stmt (extract-var-stmt 'this) '(RPS test)
                                                                                          (list
                                                                                           (extract-pointer-stmt (extract-var-stmt 'msg) 'sender))
                                                                                          ))
                                                         ; revert
                                                         ;(func-call-stmt (extract-var-stmt 'this) 'revert '())
                                                         ; num1 = val1
                                                         (assign-var-stmt 'num1
                                                                          (extract-var-stmt 'val1))
                                                         ; return val
                                                         (return-stmt (extract-var-stmt 'val)))))
                              (func-decl 'transfer-test (list (arg-decl (integer-type #f 256) 'amount)) (list)
                                         (func-call-stmt (extract-var-stmt 'this) 'transfer
                                                         (list
                                                          (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                                                          (extract-var-stmt 'amount))))
                              (func-decl 'assert-test (list (arg-decl (boolean-type) 'v)) (list)
                                         (func-call-stmt (extract-var-stmt 'this) 'assert
                                                         (list (extract-var-stmt 'v))))
                              )
                             )))
  (define env2 (deploy env1 (bv 1 (bitvector 160)) 'RPS '() (bv 10 (bitvector 256))))
  (define env3 (deploy env2 (bv 2 (bitvector 160)) 'RPS '() (bv 20 (bitvector 256))))
  (env-print-contracts env3)
  (displayln "" )
  (define env4 (call env3 '(RPS set-num1) (bv 1 (bitvector 160)) (list (bv 256 (bitvector 256))) (bv 2 (bitvector 160)) (bv 10 (bitvector 256))))
  (env-print-contracts env4)

  (define env5 (call env4 '(RPS transfer-test) (bv 1 (bitvector 160)) (list (bv 13 (bitvector 256))) (bv 2 (bitvector 160)) (bv 0 (bitvector 256))))
  (env-print-contracts env5)
  (define env6 (call env5 '(RPS transfer-test) (bv 1 (bitvector 160)) (list (bv 13 (bitvector 256))) (bv 2 (bitvector 160)) (bv 0 (bitvector 256))))
  (env-print-contracts env6)
  (define env7 (call env6 '(RPS assert-test) (bv 1 (bitvector 160)) (list #f) (bv 2 (bitvector 160)) (bv 1 (bitvector 256))))
  (env-print-contracts env7)
  (define env8 (call env7 '(RPS assert-test) (bv 1 (bitvector 160)) (list #t) (bv 2 (bitvector 160)) (bv 1 (bitvector 256))))
  (env-print-contracts env8)
  
  )
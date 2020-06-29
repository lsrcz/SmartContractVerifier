#lang rosette/safe

(require "command.rkt")

(require "env.rkt")
(require "compile.rkt")
(require "syntax.rkt")
(require "type.rkt")
(require rosette/lib/angelic) 

(define-symbolic* beat-func (~> (bitvector 8) (bitvector 8) boolean?))

(define custom-func-table
  (list (binding
         (function-type (list (integer-type #t 8) (integer-type #t 8)) (list (boolean-type)))
         'beat
         (lambda (e)
           (let ([e1 (env-set-this
                      (env-bind-args
                       e
                       (list (arg-decl
                              (integer-type #t 8)
                              'l)
                             (arg-decl
                              (integer-type #t 8)
                              'r))))])
             (let* ([l (env-resolve-local e1 'l)]
                    [r (env-resolve-local e1 'r)])
               (env-set-retval e1 (beat-func l r))))))))

(define test-contract1
  (contract-decl
   'RPS
   (list
    (contract-member-decl (ref-type (mapping-type (address-type) (address-type))) 'pmap (void))
    (contract-member-decl (ref-type (mapping-type (integer-type #f 256) (integer-type #f 256)))
                          'nummap (void))
    (contract-member-decl (ref-type (mapping-type (integer-type #t 8) (integer-type #t 8)))
                          'handmap (void))
    (constructor-decl
     '()
     (sequence-stmt
      (list
       (assign-var-stmt
        'pmap
        (new-slot-stmt (mapping-type (address-type) (address-type))))
       (assign-var-stmt
        'nummap
        (new-slot-stmt (mapping-type (integer-type #f 256) (integer-type #f 256))))
       (assign-var-stmt
        'handmap
        (new-slot-stmt (mapping-type (integer-type #t 8) (integer-type #t 8)))))))
    (func-decl
     'player1_bet
     (list (arg-decl (address-type) 'target)
           (arg-decl (integer-type #t 8) 'hand))
     '()
     (sequence-stmt
      ; this.pmap[msg.sender] = target
      (list(func-call-stmt (extract-var-stmt 'this) 'mapping-set
                           (list
                            (extract-var-stmt 'pmap)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                            (extract-var-stmt 'target)))
           ; this.nummap[msg.sender] = msg.value
           (func-call-stmt (extract-var-stmt 'this) 'mapping-set
                           (list
                            (extract-var-stmt 'nummap)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'val)))
           ; this.handmap[msg.sender] = hand
           (func-call-stmt (extract-var-stmt 'this) 'mapping-set
                           (list
                            (extract-var-stmt 'handmap)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                            (extract-var-stmt 'hand))))))
    (func-decl
     'player2_bet
     (list (arg-decl (address-type) 'target)
           (arg-decl (integer-type #t 8) 'hand))
     '()
     (sequence-stmt
      ; this.pmap[msg.sender] = target
      (list(func-call-stmt (extract-var-stmt 'this) 'mapping-set
                           (list
                            (extract-var-stmt 'pmap)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                            (extract-var-stmt 'target)))
           ; this.nummap[msg.sender] = msg.value
           (func-call-stmt (extract-var-stmt 'this) 'mapping-set
                           (list
                            (extract-var-stmt 'nummap)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'val)))
           ; this.handmap[msg.sender] = hand
           (func-call-stmt (extract-var-stmt 'this) 'mapping-set
                           (list
                            (extract-var-stmt 'handmap)
                            (extract-pointer-stmt (extract-var-stmt 'msg) 'sender)
                            (extract-var-stmt 'hand))))))
    (func-decl 'cash_out '() '()
               (sequence-stmt
                (list
                 ; p1 = msg.sender
                 (local-decl-stmt
                  (address-type) 'p1 (extract-pointer-stmt (extract-var-stmt 'msg) 'sender))
                 ; assert(pmap[p1])
                 (func-call-stmt
                  (extract-var-stmt 'this) 'assert
                  (list (func-call-stmt
                         (extract-var-stmt 'this) 'mapping-exists
                         (list (extract-var-stmt 'pmap) (extract-var-stmt 'p1)))))
                 ; p2 = pmap[p1]
                 (local-decl-stmt
                  (address-type) 'p2
                  (func-call-stmt (extract-var-stmt 'this) 'mapping-get
                                  (list (extract-var-stmt 'pmap) (extract-var-stmt 'p1))))
                 ; assert(pmap[p2])
                 (func-call-stmt
                  (extract-var-stmt 'this) 'assert
                  (list (func-call-stmt
                         (extract-var-stmt 'this) 'mapping-exists
                         (list (extract-var-stmt 'pmap) (extract-var-stmt 'p2)))))
                 ; num1 = nummap[p1]
                 (local-decl-stmt
                  (integer-type #t 256) 'num1
                  (func-call-stmt (extract-var-stmt 'this) 'mapping-get
                                  (list (extract-var-stmt 'nummap) (extract-var-stmt 'p1))))
                 ; num2 = nummap[p2]
                 (local-decl-stmt
                  (integer-type #t 256) 'num2
                  (func-call-stmt (extract-var-stmt 'this) 'mapping-get
                                  (list (extract-var-stmt 'nummap) (extract-var-stmt 'p2))))
                 ; h1 = handmap[p1]
                 (local-decl-stmt
                  (integer-type #t 256) 'h1
                  (func-call-stmt (extract-var-stmt 'this) 'mapping-get
                                  (list (extract-var-stmt 'handmap) (extract-var-stmt 'p1))))
                 ; h2 = handmap[p2]
                 (local-decl-stmt
                  (integer-type #t 256) 'h2
                  (func-call-stmt (extract-var-stmt 'this) 'mapping-get
                                  (list (extract-var-stmt 'handmap) (extract-var-stmt 'p2))))

                 ; if (beat(h1, h2)) {
                 (if-stmt (func-call-stmt (extract-var-stmt 'this) 'beat
                                          (list (extract-var-stmt 'h1)
                                                (extract-var-stmt 'h2)))
                          (sequence-stmt
                           (list
                    
                            ; transfer(p1, num1)
                            (func-call-stmt (extract-var-stmt 'this) 'transfer
                                            (list
                                             (extract-var-stmt 'p1)
                                             (extract-var-stmt 'num1)))
                            ; transfer(p1, num2)
                            (func-call-stmt (extract-var-stmt 'this) 'transfer
                                            (list
                                             (extract-var-stmt 'p1)
                                             (extract-var-stmt 'num2)))))
                          ; } else if (beat(h2, h1)) {
                          (if-stmt (func-call-stmt (extract-var-stmt 'this) 'beat
                                                   (list (extract-var-stmt 'h2)
                                                         (extract-var-stmt 'h1)))
                                   (sequence-stmt
                                    (list
                    
                                     ; transfer(p2, num1)
                                     (func-call-stmt (extract-var-stmt 'this) 'transfer
                                                     (list
                                                      (extract-var-stmt 'p2)
                                                      (extract-var-stmt 'num1)))
                                     ; transfer(p2, num2)
                                     (func-call-stmt (extract-var-stmt 'this) 'transfer
                                                     (list
                                                      (extract-var-stmt 'p2)
                                                      (extract-var-stmt 'num2)))))
                                   ; } else {
                                   (sequence-stmt
                                    (list
                    
                                     ; transfer(p1, num1)
                                     (func-call-stmt (extract-var-stmt 'this) 'transfer
                                                     (list
                                                      (extract-var-stmt 'p1)
                                                      (extract-var-stmt 'num1)))
                                     ; transfer(p1, num2)
                                     (func-call-stmt (extract-var-stmt 'this) 'transfer
                                                     (list
                                                      (extract-var-stmt 'p2)
                                                      (extract-var-stmt 'num2)))))))
                 ; } }

                 )))
    )))
  

(define (test1)
  (define env0
    (compile-contract
     (get-initial-env)
     test-contract1))
  (define env1 (env-append-func-table env0 custom-func-table))
    (define-symbolic* contract-original-balance (bitvector 256))
  (define-symbolic* alice-original-balance (bitvector 256))
  (define-symbolic* bob-original-balance (bitvector 256))
  (define contract (bv 10 (bitvector 160)))
  (define alice (bv 1 (bitvector 160)))
  (define bob (bv 2 (bitvector 160)))
  (define env2 (compile-contract env1 (contract-decl 'PLAYER (list))))
  (define env3 (deploy env2 contract 'RPS '() contract-original-balance))
  (define env4 (deploy env3 alice 'PLAYER '() alice-original-balance))
  (define env5 (deploy env4 bob 'PLAYER '() bob-original-balance))
  (define-symbolic* hand1 (bitvector 8))
  (define-symbolic* hand2 (bitvector 8))
  (define-symbolic* money1 (bitvector 256))
  (define-symbolic* money2 (bitvector 256))
  (define all-sym (list
                   (cons 'alice-original-balance alice-original-balance)
                   (cons 'bob-original-balance bob-original-balance)
                   (cons 'contract-original-balance contract-original-balance)
                   (cons 'money1 money1)
                   (cons 'money2 money2)
                   (cons 'hand1 hand1)
                   (cons 'hand2 hand2)
                   (cons 'beat-func beat-func)))
  ;(env-print-contracts env5)
  ;(env-print-global-defs env5)
  ; alice: contract.player1_bet(bob, 1) with 3 wei
  (define env6 (call env5
                     '(RPS player1_bet)
                     contract
                     (list bob hand1)
                     alice
                     money1))
  ;(env-print-contracts env6)
  ;(env-print-global-defs env6)

  ; bob: contract.player1_bet(alice, 2) with 4 wei
  (define env7 (call env6
                     '(RPS player2_bet)
                     contract
                     (list alice hand2)
                     bob
                     money2))

  ;(env-print-contracts env7)
  ;(env-print-global-defs env7)

  ; alice: contract.cash_out()
  (define env8 (call env7
                     '(RPS cash_out)
                     contract
                     '()
                     alice
                     (bv 0 (bitvector 256))))
  ;(env-print-contracts env8)
  ;(env-print-global-defs env8)

  (define big (bv 1000 (bitvector 256)))

  (define m
    (verify
     (assert
      (=> (and (bvuge alice-original-balance money1)
               (bvuge bob-original-balance money2)
               (bvult money1 big)
               (bvult money2 big)
               (bvult alice-original-balance big)
               (bvult bob-original-balance big)
               (bvult contract-original-balance big)
               (not (and (beat-func hand1 hand2) (beat-func hand2 hand1)))
               )
          
                     
          (and (=> (beat-func hand1 hand2)
                   (and (bveq (env-resolve-address env8 alice 'balance)
                              (bvadd money2 alice-original-balance))
                        (bveq (env-resolve-address env8 bob 'balance)
                              (bvsub bob-original-balance money2))))
               (=> (beat-func hand2 hand1)
                   (and (bveq (env-resolve-address env8 alice 'balance)
                              (bvsub alice-original-balance money1))
                        (bveq (env-resolve-address env8 bob 'balance)
                              (bvadd bob-original-balance money1))))
               (=> (and (not (beat-func hand1 hand2)) (not (beat-func hand2 hand1)))
                   (and (bveq (env-resolve-address env8 alice 'balance)
                              alice-original-balance)
                        (bveq (env-resolve-address env8 bob 'balance)
                              bob-original-balance)))
               )))))
  (displayln m)
  )

(define (test2)
    (define env0
    (compile-contract
     (get-initial-env)
     test-contract1))
  (define env1 (env-append-func-table env0 custom-func-table))
    (define-symbolic* contract-original-balance (bitvector 256))
  ;(define-symbolic* alice-original-balance (bitvector 256))
  ;(define-symbolic* bob-original-balance (bitvector 256))
  ;(define-symbolic* mallory-original-balance (bitvector 256))
  (define alice-original-balance (bv 100 (bitvector 256)))
  (define bob-original-balance (bv 100 (bitvector 256)))
  (define mallory-original-balance (bv 100 (bitvector 256)))
  
  (define contract (bv 10 (bitvector 160)))
  (define alice (bv 1 (bitvector 160)))
  (define bob (bv 2 (bitvector 160)))
  (define mallory (bv 3 (bitvector 160)))
  (define env2 (compile-contract env1 (contract-decl 'PLAYER (list))))
  (define env3 (deploy env2 contract 'RPS '() contract-original-balance))
  (define env4 (deploy env3 alice 'PLAYER '() alice-original-balance))
  (define env4p (deploy env4 bob 'PLAYER '() bob-original-balance))
  (define env5 (deploy env4p mallory 'PLAYER '() mallory-original-balance))
  ;(define-symbolic* hand1 (bitvector 8))
  ;(define-symbolic* hand2 (bitvector 8))
  ;(define-symbolic* money1 (bitvector 256))
  ;(define-symbolic* money2 (bitvector 256))
  (define hand1 (bv 0 (bitvector 8)))
  (define hand2 (bv 1 (bitvector 8)))
  (define money1 (bv 15 (bitvector 256)))
  (define money2 (bv 25 (bitvector 256)))
  (define all-sym (list
                   (cons 'alice-original-balance alice-original-balance)
                   (cons 'bob-original-balance bob-original-balance)
                   (cons 'contract-original-balance contract-original-balance)
                   (cons 'money1 money1)
                   (cons 'money2 money2)
                   (cons 'hand1 hand1)
                   (cons 'hand2 hand2)
                   (cons 'beat-func beat-func)))
  ;(env-print-contracts env5)
  ;(env-print-global-defs env5)
  ; alice: contract.player1_bet(bob, 1) with 3 wei
  (define env6 (call env5
                     '(RPS player1_bet)
                     contract
                     (list bob hand1)
                     alice
                     money1))
  ;(env-print-contracts env6)
  ;(env-print-global-defs env6)

  (define (get-trace)
    (define-symbolic* hand (bitvector 8))
    (define-symbolic* money (bitvector 256))
    (choose*
     (list (choose* '(RPS player1_bet) '(RPS player2_bet)) contract
           (list (choose* alice bob mallory) hand)
           mallory money)
     (list '(RPS cash_out) contract '() mallory (bv 0 (bitvector 256)))))

  ;(define trace1 (get-trace))
  (define-symbolic* hand (bitvector 8))
  (define trace1 (list '(RPS player2_bet) contract (list alice hand) mallory (bv 1 (bitvector 256))))

  (define env6p (apply call env6 trace1))
                      

  ; bob: contract.player1_bet(alice, 2) with 4 wei
  (define env7 (call env6p
                     '(RPS player2_bet)
                     contract
                     (list alice hand2)
                     bob
                     money2))
  (define trace2 (get-trace))
  ;(define trace2 (list '(RPS cash_out) contract '() mallory (bv 0 (bitvector 256))))
  (define env7p (apply call env7 trace2))

  ;(env-print-contracts env7)
  ;(env-print-global-defs env7)

  ; alice: contract.cash_out()
  (define env8 (call env7p
                     '(RPS cash_out)
                     contract
                     '()
                     alice
                     (bv 0 (bitvector 256))))
  ;(env-print-contracts env8)
  ;(env-print-global-defs env8)

  (define big (bv 1000 (bitvector 256)))
  
  (define m
    (verify
     (assert
      (=> (and (bvuge alice-original-balance money1)
               (bvuge bob-original-balance money2)
               (bvult money1 big)
               (bvult money2 big)
               (bvult alice-original-balance big)
               (bvult bob-original-balance big)
               (bvult contract-original-balance big)
               (not (and (beat-func hand1 hand2) (beat-func hand2 hand1)))
               )
          
                     
          (and (=> (beat-func hand1 hand2)
                   (and (bveq (env-resolve-address env8 alice 'balance)
                              (bvadd money2 alice-original-balance))
                        (bveq (env-resolve-address env8 bob 'balance)
                              (bvsub bob-original-balance money2))))
               (=> (beat-func hand2 hand1)
                   (and (bveq (env-resolve-address env8 alice 'balance)
                              (bvsub alice-original-balance money1))
                        (bveq (env-resolve-address env8 bob 'balance)
                              (bvadd bob-original-balance money1))))
               (=> (and (not (beat-func hand1 hand2)) (not (beat-func hand2 hand1)))
                   (and (bveq (env-resolve-address env8 alice 'balance)
                              alice-original-balance)
                        (bveq (env-resolve-address env8 bob 'balance)
                              bob-original-balance)))
               )))))
  (displayln m)
  (displayln (evaluate trace1 m))
  (displayln (evaluate trace2 m))
  )

(module+ test
  (test1)
  (test2)
  )
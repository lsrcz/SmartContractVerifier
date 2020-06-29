#lang rosette/safe

(require rosette/lib/match)
(require "syntax.rkt")
(require "type.rkt")
(provide (all-defined-out))

(define (error) (car '()))

(struct binding (type name val) #:transparent)
(struct ref (ptr) #:transparent)

(struct address-binding (addr val) #:transparent)

(define (empty-address-env) (list))

(define (empty-func-table-env) (list))

(struct frame (args locals retval status curraddr) #:transparent)

(define (frame-set-args frame-input args)
  (match frame-input
    [(frame _ locals retval status curraddr)
     (frame args locals retval status curraddr)]))

(define (frame-set-this frame-input)
  (match frame-input
    [(frame args _ _ _ curraddr)
     (frame-add-local frame-input (address-type) 'this curraddr)]))

(define (frame-add-local frame-input type name val)
  (match frame-input
    [(frame args locals retval status curraddr)
     (frame args (cons (binding type name val) locals) retval status curraddr)]))

(define (set-binding-list binding-list name val)
  (if (null? binding-list)
      '()
      (let ([cur (car binding-list)]
            [rest (cdr binding-list)])
        (if (eq? name (binding-name cur))
            (cons (binding (binding-type cur) name val) rest)
            (cons cur (set-binding-list rest name val))))))

(define (rebind-binding-list binding-list type name val)
  (if (null? binding-list)
      (list (binding type name val))
      (let ([cur (car binding-list)]
            [rest (cdr binding-list)])
        (if (eq? name (binding-name cur))
            (cons (binding (binding-type cur) name val) rest)
            (cons cur (rebind-binding-list rest type name val))))))

(define (resolve-binding-list binding-list name)
  (if (null? binding-list)
      (void)
      (let ([cur (car binding-list)]
            [rest (cdr binding-list)])
        (if (equal? name (binding-name cur))
            (binding-val cur)
            (resolve-binding-list rest name)))))

(define (frame-set-local frame-input name val)
  (match frame-input
    [(frame args locals retval status curraddr)
     (frame args (set-binding-list locals name val) retval status curraddr)]))

(define (frame-exists-local frame-input name)
  (define (iter locals)
    (if (null? locals)
        #f
        (if (eq? name (binding-name (car locals)))
            #t
            (iter (cdr locals)))))
  (match frame-input
    [(frame _ locals _ _ _) (iter locals)]))

(define (frame-set-retval frame-input retvals)
  (match frame-input
    [(frame args locals _ status curraddr)
     (frame args locals retvals status curraddr)]))

(define (frame-set-status frame-input status)
  (match frame-input
    [(frame args locals retval _ curraddr)
     (frame args locals retval status curraddr)]))

(define (frame-resolve-local frame-input name)
  (define (iter x)
    (if (null? x)
        (void)
        (match (car x)
          [(binding _ nameb val)
           (if (eq? nameb name)
               val
               (iter (cdr x)))])))
  (iter (frame-locals frame-input)))

(define (frame-set-curraddr frame-input curraddr)
  (match frame-input
    [(frame args locals retval status _)
     (frame args locals retval status curraddr)]))

(define (empty-execute-stack) (list))

(struct env (stack func-table address-env cur-ptr-val heap) #:transparent)

(define (env-push-stack env-input address)
  (match env-input
    [(env st func-table address-env cur-ptr-val heap)
     (env (cons (frame '() '() (void) 'reg address) st) func-table address-env cur-ptr-val heap)]))

(define (env-pop-stack env-input)
  (match env-input
    [(env st func-table address-env cur-ptr-val heap)
     (env (cdr st) func-table address-env cur-ptr-val heap)]))

(define (env-add-local env-input type name val)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (let ([cur (car stack)]
           [rest (cdr stack)])
       (env
        (cons (frame-add-local cur type name val) rest) func-table address-env cur-ptr-val heap))]))

(define (env-set-args env-input args)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (let ([cur (car stack)]
           [rest (cdr stack)])
       (env (cons (frame-set-args cur args) rest) func-table address-env cur-ptr-val heap))]))

(define (env-set-retval env-input retvals)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (let ([cur (car stack)]
           [rest (cdr stack)])
       (env (cons (frame-set-retval cur retvals) rest) func-table address-env cur-ptr-val heap))]))

(define (env-set-status env-input status)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (let ([cur (car stack)]
           [rest (cdr stack)])
       (env (cons (frame-set-status cur status) rest) func-table address-env cur-ptr-val heap))]))

(define (env-get-args env-input)
  (frame-args (car (env-stack env-input))))

(define (env-get-locals env-input)
  (frame-locals (car (env-stack env-input))))

(define (env-get-retval env-input)
  (frame-retval (car (env-stack env-input))))

(define (env-get-status env-input)
  (frame-status (car (env-stack env-input))))

(define (env-get-curraddr env-input)
  (frame-curraddr (car (env-stack env-input))))

(define (address-resolve address-val name)
  (if (null? address-val)
      (void)
      (if (eq? name (binding-name (car address-val)))
          (binding-val (car address-val))
          (address-resolve (cdr address-val) name))))

(define (env-resolve-address env-input addr name)
  (define (iter-address addr address-env)
    (if (null? address-env)
        (void)
        (let ([cur (car address-env)]
              [rest (cdr address-env)])
          (if (eq? addr (address-binding-addr cur))
              (address-resolve (address-binding-val cur) name)
              (iter-address addr rest)))))
  (match env-input
    [(env _ _ address-env _ _)
     (iter-address addr address-env)]))

(define (env-assign-address env-input addr name val)

  (define (iter-address address-env)
    (if (null? address-env)
        (void)
        (let ([cur (car address-env)]
              [rest (cdr address-env)])
          (if (eq? addr (address-binding-addr cur))
              (cons (address-binding addr (set-binding-list (address-binding-val cur) name val))
                    rest)
              (cons cur (iter-address rest))))))
  (match env-input
    [(env stack func-list address-env cur-ptr-val heap)
     (env stack func-list (iter-address address-env) cur-ptr-val heap)]))

(define (env-resolve-local env-input name)
  (define (iter-address addr address-env)
    (if (null? address-env)
        (void)
        (let ([cur (car address-env)]
              [rest (cdr address-env)])
          (if (eq? addr (address-binding-addr cur))
              (address-resolve (address-binding-val cur) name)
              (iter-address addr rest)))))
  (let* ([stack (env-stack env-input)]
         [l (frame-resolve-local (car stack) name)])
    (if (void? l)
        (env-resolve-address env-input (frame-curraddr (car stack)) name)
        l)))

(define (env-assign-local env-input name val)
  (match env-input
    [(env stack func-list address-env cur-ptr-val heap)
     (if (frame-exists-local (car stack) name)
         (env (cons (frame-set-local (car stack) name val) (cdr stack))
              func-list address-env cur-ptr-val heap)
         (env-assign-address env-input (frame-curraddr (car stack)) name val))]))

(define (env-set-address-val env-input addr val)
  (define (iter-address address-env)
    (if (null? address-env)
        (list (address-binding addr val))
        (let ([cur (car address-env)]
              [rest (cdr address-env)])
          (if (eq? addr (address-binding-addr cur))
              (cons (address-binding addr val) rest)
              (cons cur (iter-address rest))))))
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (env stack func-table (iter-address address-env) cur-ptr-val heap)]))

(define (env-resolve-func env-input func-name)
  (define (iter func-table)
    (if (null? func-table)
        (void)
        (let ([cur (car func-table)]
              [rest (cdr func-table)])
          (if (eq? func-name (binding-name cur))
              (binding-val cur)
              (iter rest)))))
  (iter (env-func-table env-input)))

(define (env-append-func-table env-input funcs)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (env stack (append funcs func-table) address-env cur-ptr-val heap)]))

(define (env-new-heap-var env-input type val)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (env stack func-table address-env
          (+ 1 cur-ptr-val) (cons (binding type cur-ptr-val val) heap))]))

(define (env-new-heap-var-on-stack env-input type name val)
  (define ptrval (ref (env-cur-ptr-val env-input)))
  (env-add-local (env-new-heap-var env-input type val) type name ptrval))

(define (env-rebind-heap-var env-input heapptr val)
  (define (iter heap)
    (if (null? heap)
        (car '())
        (let ([cur (car heap)]
              [rest (cdr heap)])
          (if (eq? heapptr (binding-name cur))
              (match cur
                [(binding type name _) (cons (binding type name val) rest)])
              (cons cur (iter rest))))))
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (env stack func-table address-env cur-ptr-val (iter heap))]))


(define (env-bind-args env-input form-args)
  (foldl (lambda (real-arg form-arg acc)
           (match form-arg
             [(arg-decl type name)
              (env-add-local acc type name real-arg)]))
         env-input
         (env-get-args env-input)
         form-args))

(define (env-set-this env-input)
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (env
      (cons (frame-set-this (car stack)) (cdr stack)) func-table address-env cur-ptr-val heap)]))

(define (env-resolve-ptr env-input ptr-val)
  (resolve-binding-list (env-heap env-input) ptr-val))

(define (env-resolve-ptr-member env-input ptr-val membername)
  (define val (env-resolve-ptr env-input ptr-val))
  (resolve-binding-list val membername))

(define (env-set-ptr-member env-input ptr-val membername val)
  (define derefval (env-resolve-ptr env-input ptr-val))
  (match env-input
    [(env stack func-table address-env cur-ptr-val heap)
     (env stack func-table address-env cur-ptr-val
          (set-binding-list heap (set-binding-list derefval membername val)))]))

(define (get-empty-env)
  (env '()
       '()
       '() 0 '()))

(define (format-val type val)
  (cond [(boolean-type? type)
         (format "~a" val)]
        [(integer-type? type)
         (if (integer-type-signed type)
             (format "~a" (bitvector->integer val))
             (format "~a" (bitvector->natural val)))]
        [(fixed-bytes-type? type)
         (format "~a" val)]
        [(address-type? type)
         (format "~a" val)]
        [(ref-type? type)
         (if (void? (ref-ptr val))
             "*null*"
             (format "*~a*" (ref-ptr val)))]
        [(struct-type? type) (format-binding-list val)]
        [else (format "~a" val)]))

(define (format-binding b)
  (match b
    [(binding type name val)
     (format "~a: ~a" name (format-val type val))]))

(define (format-binding-list bdlst)
  (define (iter bdlst)
    (if (null? bdlst)
        "}"
        (format " ~a\n~a" (format-binding (car bdlst)) (iter (cdr bdlst)))))
  (format "{\n~a" (iter bdlst)))

(define (env-print-contracts env-input)
  (define (iter address-env)
    (if (null? address-env)
        (void)
        (let ([cur (car address-env)]
              [rest (cdr address-env)])
          (match cur
            [(address-binding addr body)
             (begin (displayln (format "At ~a:\n~a" addr (format-binding-list body)))
                    (iter rest))]))))
  (iter (env-address-env env-input)))

(define (env-print-global-defs env-input)
  (define (iter heap)
    (if (null? heap)
        (void)
        (let ([cur (car heap)]
              [rest (cdr heap)])
          (match cur
            [(binding _ name val)
             (begin (displayln (format "At heap ~a:\n~a" name (format-binding-list val)))
                    (iter rest))]))))
  (iter (env-heap env-input)))

(define (get-default-val type)
  (cond [(boolean-type? type) #f]
        [(integer-type? type) (bv 0 (bitvector (integer-type-bitwidth type)))]
        [(fixed-bytes-type? type) (bv 0 (bitvector (* 4 (fixed-bytes-type-width type))))]
        [(address-type? type) (bv 0 (bitvector 160))]
        [(mapping-type? type) (list)]
        [(ref-type? type) (ref (void))]
        [else (displayln type) (car '())]))

(define (get-default-ref-val env type)
  (let ([cur-ptr-val (env-cur-ptr-val env)]
        [innerty (ref-type-type type)])
    (cons (env-new-heap-var env innerty (get-default-val env innerty))
          (ref cur-ptr-val))))





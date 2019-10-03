#lang racket/base

(require racket/class
         racket/contract
         racket/function
         ss/racket/syntax
         ;racket/pretty
         (for-syntax racket/base
                     ss/racket/syntax
                     racket/function
                     syntax/stx
                     syntax/parse))

(provide (all-from-out racket/class)
         method
         field-getter
         make-is-a
         make
         make-constructor
         new-constructor
         define-composed-mixins
         compose-mixin
         class-from-mixins
         inspectable<%>
         define-inspected-class
         get-field+)



(define-syntax get-field+
  (syntax-rules ()
    [(_ x l) (get-field l x)]
    [(_ x xs ... l) (get-field l (get-field+ x xs ...))]))


(define-syntax-rule (method m)
  (λ (obj . args) (send/apply obj m args)))

(define-syntax-rule (field-getter field)
  (λ (obj) (get-field field obj)))

(define (make-is-a %)
  (λ (obj) (is-a? obj %)))

(define-syntax (make stx)
  (syntax-case stx ()
    [(_ % [field value] ...) (with-syntax ([(initializer ...)  (map (λ (f) (format-id f "init-~a!" (syntax-e f)))
                                                                    (syntax->list #'(field ...)))])
                               
                               #'(let ([instance (new %)])
                                   (send instance initializer value)
                                   ...
                                   instance))]
    ))



(define-syntax-rule (new-constructor % init-field-id ...)
  (λ (init-field-id ...) (new %
                              [init-field-id init-field-id]
                              ...)))


(define-syntax-rule (make-constructor % init-field-id ...)
  (λ (init-field-id ...) (make %
                              [init-field-id init-field-id]
                              ...)))



(define-syntax (initializer/c stx)
  (syntax-parse stx  
    [(q field-id:id arg/c) #`(->i ([this any/c]
                                   (arg  arg/c))
                                  #:pre/name (this) #,(format "~a,  recurrent initialization" (syntax-e #'field-id)) (eq? (get-field field-id this) 'uninitialized)
                                  [result void])]))

(define mixin/c (-> class? class?))

(define/contract (mixin-sum . mixins)
  (->* () #:rest (listof mixin/c) mixin/c)
  (apply compose (reverse mixins)))




(define-syntax (compose-mixin stx)
  (syntax-parse stx
    [(_ mixin-id:id ...+)
     (with-syntax [((suffixed ...) (stx-map (curryr dotted-suffix-id #'mixin)
                                            #'(mixin-id ...)))]
       #'(mixin-sum suffixed ...))]))


(define-syntax-rule (class-from-mixins mixins ...)
  (let ([sum (compose-mixin mixins ...)])
    (sum object%)))




(define-syntax (define-composed-mixins stx)
  (syntax-parse stx
    [(_ (result-mixin:id (sub-mixin:id ...+))
        ...+)
     
     (with-syntax ([(suffixed-result-mixin ...) (stx-map (curryr dotted-suffix-id #'mixin)
                                                         #'(result-mixin ...))])
       #'(begin
           (define suffixed-result-mixin (compose-mixin sub-mixin ...))
           ...)
       )]))


(define inspectable<%>
  (interface ()
    [get-class-name (->m symbol?)]))


(define-syntax (define-inspected-class stx)
  (syntax-parse stx
    [(_ name:id cls:expr)
     #'(define name
         (class cls
           (super-new)
           (define/override (get-class-name) 'name)))]))



;; (pretty-display (syntax->datum (expand-once #'(define-interface+mixin ancestor
;;                                                 (#:intterface (node<%>)
;;                                                   some-method)
;;                                                 (#:mixin
;;                                                  (define/public (some-method) 'ok))))))


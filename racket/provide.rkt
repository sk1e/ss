#lang racket/base

(require racket/provide
         racket/provide-syntax
         
         (for-syntax racket/base
                     racket/function
                     racket/string
                     
                     syntax/parse))

(provide (all-from-out racket/provide)
         mixins+interfaces
         suffixed-as)


(define-provide-syntax (mixins+interfaces stx)
  (syntax-case stx ()
    [(_ sub-form) 
     #'(matching-identifiers-out #rx"<%>$|-mixin$" sub-form)]))


(define-provide-syntax (suffixed-as stx)
  (define suffix-ht #hash((class . "%")
                          (interface . "<%>")
                          (mixin . "-mixin")))
  (syntax-parse stx
    [(_ suffix-type ... #:from sub-form)
     (with-syntax ([rx (regexp (string-join (map (compose (curryr string-append "$")
                                                          (curry hash-ref suffix-ht)
                                                          syntax-e)
                                                 (syntax->list #'(suffix-type ...)))
                                            "|"))])
       #'(matching-identifiers-out rx sub-form))])
  ;; (syntax-case stx ()
  ;;   [(_ suffix-type ... sub-form) 
  ;;    #'(matching-identifiers-out #rx"<%>$|-mixin$" sub-form)])
  )


;#'(combine-out node-mixin node? (all-defined-out))
  ;#'(all-defined-out)
  ;(syntax (all-defined-out))
  ;#'(all-defined-out)
  ;#'(all-defined-out)
  ;(displayln a)
  ;#'node-mixin
  ;(syntax-e #'(all-defined-out))
  ;; #'(filtered-out (curry regexp-match? #rx"<%>$|-mixin$")
  ;;                 (all-defined-out))
  

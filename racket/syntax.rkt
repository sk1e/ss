#lang racket/base

(require racket/syntax
         racket/contract)

(provide (all-from-out racket/syntax)

         dotted-prefix-id
         dotted-suffix-id
         prefix-id
         suffix-id)

(define (prefix-id id morpheme)
  (-> identifier? identifier? identifier?)
  (format-id id "~a~a" (syntax-e morpheme) (syntax-e id)))


(define (suffix-id id morpheme)
  (-> identifier? identifier? identifier?)
  (format-id id "~a~a" (syntax-e id) (syntax-e morpheme)))


(define (dotted-prefix-id id morpheme)
  (-> identifier? identifier? identifier?)
  (format-id id "~a-~a" (syntax-e morpheme) (syntax-e id)))

(define (dotted-suffix-id id morpheme)
  (-> identifier? identifier? identifier?)
  (format-id id "~a-~a" (syntax-e id) (syntax-e morpheme)))


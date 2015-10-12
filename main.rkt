#lang racket/base

(require scribble/core scribble/manual
         (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out))

;; from Ben Lerner
(define-syntax (R-block stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:indent indent) #:defaults ([indent #'1]))
             (~optional (~seq #:line-numbers line-numbers) #:defaults ([line-numbers #'#f]))
             (~optional (~seq #:line-number-sep line-number-sep) #:defaults ([line-number-sep #'1])))
        ...
        arg args ...)
     #'(codeblock #:indent indent #:line-numbers line-numbers #:line-number-sep line-number-sep
                  #:keep-lang-line? #f #:context #'arg
                "#lang r-lexer\n" arg args ...)]))
(define-syntax-rule (R arg args ...)
  (make-element (make-style "RktBlk" '(tt-chars)) (code #:lang "r-lexer" arg args ...)))

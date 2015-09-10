#lang racket/base

(require scribble/core scribble/manual)

(provide (all-defined-out))

;; from Ben Lerner
(define-syntax-rule (R-block arg args ...)
  (codeblock #:keep-lang-line? #f #:context #'arg "#lang r-lexer\n" arg args ...))
(define-syntax-rule (R arg args ...)
  (make-element (make-style "RktBlk" '(tt-chars)) (code #:lang "r-lexer" arg args ...)))

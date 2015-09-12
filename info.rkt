#lang info
(define collection "r-lexer")
(define deps '("base"
               "rackunit-lib"
               "scribble-lib"
               "parser-tools-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/r-lexer.scrbl" ())))
(define pkg-desc "Lexer for R")
(define version "0.0")
(define pkg-authors '(leif))

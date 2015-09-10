#lang s-exp syntax/module-reader
--ignored--
#:read r-read
#:read-syntax r-read-syntax
#:info r-get-info

(require scribble/core
         parser-tools/lex
         (prefix-in re: parser-tools/lex-sre))

(define (r-read in)
  (syntax->datum (r-read-syntax #f in)))

(define (r-read-syntax src in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    (if (eq? type 'eof)
        eof
        (datum->syntax #f
                       type
                       (list src #f #f start (- end start))))))

(define (get-color-lexer in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    (values lexeme
            type
            data
            start
            end)))

(define (r-get-info key default default-filter)
  (case key
    [(color-lexer)
     get-color-lexer]
    [else
     (default-filter key default)]))

(define (syn-val lex a b c d)
  (values lex a b (position-offset c) (position-offset d)))

(define-lex-abbrevs
  (Operator (re:or "=" ">" "<" "!" "~" "?" ":" "==" "<=" ">=" "!=" "&&" "||" "+"
                   "-" "*" "/" "&" "|" "^" "%" "<<" ">>" "<-" "<<-")))

(define get-syntax-token
  (lexer
   (Operator (syn-val lexeme 'keyword #f start-pos end-pos))
   (any-char (syn-val lexeme 'error #f start-pos end-pos))))

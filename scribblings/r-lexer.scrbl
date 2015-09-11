#lang scribble/manual
@require[r-lexer
         @for-label[r-lexer
                    racket/base]]

@title{R Lexer}
@author{Leif Andersen}

@defmodule[r-lexer]

The @racket[R] and @racket[R-block] forms are used for typesetting R code.

@defform[(R str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to the @racket[code] function.
Parses R code from strings into the inline text of the document.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  This is @R{1 + 2}.
}|>|

produces the typeset result:

@nested[#:style 'inset]{
  This is @R{1 + 2}.
}

@racket[str-expr] is a list of strings representing R code.
}

@defform[(R-block str-expr ...+)
         #:contracts ([str-expr string?])]{
Similar to the @racket[codeblock] function.
Parses R code from strings into a block in the document.

For example,

@codeblock[#:keep-lang-line? #f]|<|{
  #lang scribble/manual
  @R-block|{
    f <- function (x) {
      if (f <= 1) 1;
      else        x*f(x-1);
    }
  }|
}|>|

produces the typset result:

@nested[#:style 'inset]{
  @R-block|{
    f <- function (x) {
      if (f <= 1) 1;
      else        x * f(x - 1);
    }
  }|
}

@racket[str-expr] is a list of strings representing R code.
}

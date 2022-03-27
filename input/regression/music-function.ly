\version "2.23.8"

\header
{
texidoc = "Music functions are generic music transformation functions,
which can be used to extend music syntax seamlessly.  Here we define
and use a @code{\\myBar} function which works like @code{\\bar}."
}

myBar = #(define-music-function (bar-type) (string?)
          (make-music 'BarEvent 'bar-type bar-type))

\layout { ragged-right = ##t }

{
    d4 \myBar ".|:" d4
}

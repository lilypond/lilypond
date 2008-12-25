\header
{
    
texidoc = "Music functions are generic music transformation functions,
which can be used to extend music syntax seamlessly.  Here we
demonstrate a @code{\myBar} function, which works similar to
@code{\bar}, but is implemented completely in Scheme."

}
\version "2.12.0"

myBar = #(define-music-function (parser location bar-type) (string?)
          (context-spec-music
           (context-spec-music (make-property-set 'whichBar bar-type) 'Timing)
           'Score))

\layout { ragged-right = ##t }

{
    d4 \myBar "|:" d4
}


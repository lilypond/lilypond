\header
{
    
texidoc = "Music function are generic music transformation functions,
which can be used to extend music syntax seamlessly.  Here we
demonstrate a @code{\myBar} function, which works similar to
@code{\bar}, but is implemented completely in Scheme."

}
\version "2.3.22"

#(define myBar
  (ly:make-music-function
   (list string?)
   (lambda (where type)
    (context-spec-music
     (context-spec-music (make-property-set 'whichBar type) 'Timing)
     'Score))
    ))

\layout { raggedright = ##t }

{
    d4 \myBar #"|:" d4
}


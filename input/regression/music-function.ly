\header
{
texidoc = "Music function are generic music transformation functions,
which can be used to extend music syntax seamlessly."

}
\version "2.3.16"

#(define myBar
  (ly:make-music-function
   (list string?)
   (lambda (where type)
    (context-spec-music
     (context-spec-music (make-property-set 'whichBar type) 'Timing)
     'Score))
    
    ))

\score{
     {
	d4 \myBar #"|:" d4
	
    }
}


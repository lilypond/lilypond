\header
{
texidoc = "Music heads are generic music transformation functions,
which can be used to extend music syntax seamlessly."

}
\version "2.3.1"

#(define myBar
  (ly:make-music-head
   (lambda (where type)
    (context-spec-music
     (context-spec-music (make-property-set whichBar type) 'Timing)
     'Score))
   (list string?)
    
    ))

\score{
    \notes {
	d4 \myBar #"|:" d4
	
    }
}


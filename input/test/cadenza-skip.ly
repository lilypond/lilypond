\header {


    texidoc = "A second staff can be aligned to a fragment (say, a
    cadenza) from the first staff, using a Scheme function: the
    function creates a skip of the same length as the cadenza. "

   }

\version "2.3.8"

cadenza =  \relative c' {
    c4 d8 << { e f g } \\ { d4. } >>
    g4 f2 g4 g
}


#(define (skip-of-length mus)
  "Create a skip of exactle the same length as MUS."
  (let*
   ((skip
     (make-music
      'SkipEvent
      'duration (ly:make-duration 0 0))))

   (make-event-chord (list (ly:music-compress skip (ly:music-length mus))))
))


#(define (mmrest-of-length mus)
  "Create a mmrest of exactly the same length as MUS."
  
  (let*
   ((skip
     (make-multi-measure-rest
      (ly:make-duration 0 0) '() )))
   (ly:music-compress skip (ly:music-length mus))
   skip
))


\score {
    
    \relative c' \new GrandStaff <<
	\new Staff { \cadenza c4 \bar "|." }
	\new Staff {
	    #(ly:export (mmrest-of-length cadenza))
	    c4 \bar "|." }
    >>

    \paper {}
}

\version "2.19.22"

\header{
  texidoc= "Test optional music function arguments.
The output is nonsensical, but if you wrack your brain,
you'll figure it out.  Remember that optional arguments
are matched left to right, and after the first non-match,
the rest is skipped."
}

\layout { ragged-right = ##t }

% Just like \relative, but defaulting to f as reference, making the
% first note of the music the same as if written as absolute pitch
ablative =
#(define-music-function (ref music)
  ((ly:pitch? #{ f #}) ly:music?)
  #{ \relative $ref $music #})

% Let's take a duration and four pitches, defaulting to 2 c' d' e'
zap = 
#(define-music-function (dur a b c d)
  ((ly:duration? #{ 2 #}) (ly:pitch? #{ c' #})
   (ly:pitch? #{ d' #}) (ly:pitch? #{ e' #})
   ly:music?) #{ $a $dur $b $c ^\markup{!} $d  #})

\new Voice { \relative c' e' \relative { e'' } \ablative c' e' \ablative { e' }
  \zap 8. c'' d'' {e''4..} \zap f''8 g'' \zap 4 a'' b'' c''' d'''2 }

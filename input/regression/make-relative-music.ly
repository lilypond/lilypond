\version "2.19.22"

\header {
  texidoc = "@code{make-relative} can make relativization on music
function calls behave as one would expect from looking at the
function's arguments rather than at the actually resulting
expressions.  This regtest defines an example function
@code{\\withOctave} which works equally well inside and outside of
@code{\\relative}."
}

withOctave =
#(define-music-function (music)
  (ly:music?)
  (make-relative
   (music) music
   #{ \context Bottom << $music \transpose c c' $music >> #}))

mus = {
  \partial 4. c'8 e g |
  c2 e,4 g |
  c,8 c' b a <g d'> <f c'> <e b'> <d a'> |
  <c g'>1 | \bar "|."
}

<<
  \relative \new Staff { <>^"original" \mus }
  \relative \new Staff { <>^\markup \typewriter "\\relative \\withOctave"
			 \withOctave \mus }
  \new Staff { <>^\markup \typewriter "\\withOctave \\relative"
	       \withOctave \relative \mus }
>>

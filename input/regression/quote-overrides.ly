\version "2.21.0"

\header {
  texidoc = "The @code{\\quoteDuring} command shall also quote correctly all
  @code{\\override}, @code{\\once \\override}, @code{\\revert}, @code{\\set},
  @code{\\unset} and @code{\\tweak} events. The first line contains the
  original music, the second line quotes the whole music and should look
  identical.

  By default, not all events are quoted. By setting the quoted event types to
  @code{'(StreamEvent)}, everything should be quoted."
}

mus = \relative {
  % Acciaccaturas contain a slur and  \override Flag.stroke-style
  % Thus, we're checking \override here
  c'4 \acciaccatura d8 c4
  % Checking \set and \unset
  \set fontSize = #6 f
  \unset fontSize f |

  \set autoBeaming = ##f
  % Checking \once \override
  \once \override Stem.thickness = #8.0 d8
  % Checking two overrides
  \override Stem.thickness = #8.0 \override Flag.stroke-style = "grace"
  d8
  % reverting one of them
  \revert Stem.thickness d8
  % and the other
  \revert Flag.stroke-style c8

  % checking tweaks
  c2-\tweak color #red ->
}
\addQuote "music" \mus

\layout {
  \context {
    \Score
    quotedEventTypes = #'(StreamEvent)
  }
}

\score
{ <<
  \new Staff \mus
  \new Voice { \quoteDuring "music" s1*2 }
>> }

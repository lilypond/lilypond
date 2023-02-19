\version "2.27.6"

\header {

  texidoc = "
Overriding the usual properties and @code{'bound-details} sub-properties works.
If a @code{TempoMark} is desired at end of score, use the special
@code{tempoEndMark} and set @code{'to-barline} false."
}

\layout { ragged-right = ##t }

#(ly:set-option 'warning-as-error #t)

{
  \tempoMark "Allegro"
  \override Score.TempoSpanner.to-barline = ##f
  \override Score.TempoSpanner.style = #'dotted-line
  \override Score.TempoSpanner.dash-period = #1
  \override Score.TempoSpanner.thickness = 4
  \override Score.TempoSpanner.padding = -0.05
  \override Score.TempoSpanner.bound-details.left.text = "accelerando"
  \override Score.TempoSpanner.bound-details.right.arrow = ##t
  \override Score.TempoSpanner.bound-details.right.text = "molto "
  \tempoSpan
  b'1 b''
  b' b'
  b' b'
  \tempoEndMark "Vivace"
  \fine
}

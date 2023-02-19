\version "2.27.6"

\header {
  texidoc = "
A tempo line spanner may be interrupted by using @code{\\breakTempoSpan},
similar to @code{\\breakDynamicSpan} for dynamic line spanners.  If the
@code{direction} property of the @code{TempoSpanner} changes,
@code{TempoLineSpanner} will get broken and restarted.  This is always done, if
the property @code{style} of the tempo spanner is @code{'none}.
A metronome mark can be used for finishing the tempo spanner at the end of the
score if @code{'to-barline} is false.
" }

\layout { ragged-right = ##t }
#(ly:set-option 'warning-as-error #t)

{
  \tempoMark "Largo"
  b1
  \tempoMark "Adagio"
  \tempoSpan
  \breakTempoSpan
  b''1
  b1
  \tempoMark "Andante"
  \tempoSpan
  b'''1
  \breakTempoSpan
  b1
  \tempoMark "Allegro"
  \tempoSpan
  b1
  b''1
  \breakTempoSpan
  \tempoMark "Vivace"
  b1
  \break
  \override Score.TempoSpanner.style = #'none
  \tempoMark "Largo"
  b1
  \tempoMark "Adagio"
  \tempoSpan
  b''1
  b1
  \tempoMark "Andante"
  \tempoSpan
  b'''1
  b1
  \tempoMark "Allegro"
  \tempoSpan
  b1
  b''1
  \tempoMark "Vivace"
  b1
  \break
  \revert Score.TempoSpanner.style
  b1
  \tempoMark "Allegro"
  \tempoSpan
  b1
  b1
  \override Score.TempoSpanner.direction = #DOWN
  \tempoMark "Andante"
  \tempoSpan
  b'''1
  b1
  \override Score.TempoSpanner.direction = #UP
  \tempoMark "Adagio"
  \tempoSpan
  \breakTempoSpan
  b1
  b''1
  \override Score.TempoSpanner.to-barline = ##f
  \tempoMark "Largo"
  \tempoSpan
  b1
  b1
  \tempo 4=40
  \fine
}

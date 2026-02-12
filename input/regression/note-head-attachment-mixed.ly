\version "2.25.34"

\header {
  texidoc = "When note heads with different attachment points share a stem, they
attach in their usual places.  The first set of chords mixes default heads with
``triangle'' heads, which attach to up-stems at the right vertex and to
down-stems at the bottom vertex.  The second set of chords mixes default heads
with ``petrucci'' heads, which are centered on the stem."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 40)

\layout {
  indent = 0
  ragged-right = ##t
  %% Hide unnecessary symbols.
  \context {
    \Score
    \remove Bar_number_engraver
  }
  \context {
    \Staff
    \remove Clef_engraver
    \remove Time_signature_engraver
    \remove Staff_symbol_engraver
  }
}

{
  <\tweak #'style #'triangle c' c''>
  <c' \tweak #'style #'triangle c''>
  <\tweak #'style #'triangle f' f''>
  <f' \tweak #'style #'triangle f''>

  \break

  <\tweak #'style #'petrucci c' c''>
  <c' \tweak #'style #'petrucci c''>
  <\tweak #'style #'petrucci f' f''>
  <f' \tweak #'style #'petrucci f''>
}

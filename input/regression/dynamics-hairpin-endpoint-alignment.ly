\header {
  texidoc = "By default hairpins extend to the extremes of the bound
  if there is no adjacent hairpin or dynamic text.  A hairpin may
  instead extend to the @code{LEFT}, @code{CENTER} or @code{RIGHT}
  of @code{NoteColumn} grobs by overriding property
  @code{endpoint-alignments}, which is a pair of numbers representing
  the left and right ends of the hairpin.  @code{endpoint-alignments}
  are expected to be directions (either -1, 0 or@tie{}1). Other
  values will be transformed with a warning.  The right end of a
  hairpin terminating at a rest is not affected, always ending at the
  left edge of the rest."
}

\version "2.23.1"

\layout { indent = 0\mm }

music = {
  c'2\< 4\! 4\> | \noBreak
  1\! | \noBreak

  <c' d'>2\< 4\! 4\> | \noBreak
  1\! | \noBreak

  c'1\< | \noBreak
  1\! | \noBreak
  1\> | \noBreak
  1\! | \noBreak

  c'2\< r2\! | \bar "||" \break
}

music_dynamic_text = {
  c'2\p\< 4\! 4\> | \noBreak
  1\ppp | \bar "||" \break
}

{
  \override Hairpin.to-barline = ##f
  \tempo \markup { "endpoint-alignments = #`(,LEFT . ,RIGHT)" }
  \music
  \tempo \markup { "endpoint-alignments = #`(,LEFT . ,LEFT)" }
  \override Hairpin.endpoint-alignments = #`(,LEFT . ,LEFT)
  \music
  \tempo \markup { "endpoint-alignments = #`(,RIGHT . ,LEFT)" }
  \override Hairpin.endpoint-alignments = #`(,RIGHT . ,LEFT)
  \music
  \tempo \markup { "endpoint-alignments = #`(,RIGHT . ,RIGHT)" }
  \override Hairpin.endpoint-alignments = #`(,RIGHT . ,RIGHT)
  \music
  \tempo \markup { "endpoint-alignments = #`(,CENTER . ,CENTER)" }
  \override Hairpin.endpoint-alignments = #`(,CENTER . ,CENTER)
  \music

  \tempo \markup { \left-column {
    "Ends adjacent to dynamic text are not"
    "influenced by endpoint-alignments"
  } }
  \override Hairpin.endpoint-alignments = #`(,LEFT . ,RIGHT)
  \music_dynamic_text
  \override Hairpin.endpoint-alignments = #`(,RIGHT . ,LEFT)
  \music_dynamic_text
}

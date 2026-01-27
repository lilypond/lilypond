\version "2.25.34"

\header {
  texidoc  = "An @code{ApproximatePitchNoteHead} at the end of a stem always
points away from the stem by default.  Elsewhere, it points away from the center
of the staff."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 30) % magnify differences in note heads

\layout {
  indent = 0
}

\new Staff \with {
  \remove Clef_engraver
  \remove Time_signature_engraver
  \autoBeamOff
} {
  \time 4/8

  <>^\markup \teeny "Stems forced"
  \stemDown
  \approximatePitch c'8
  \approximatePitch b'
  \approximatePitch a''
  <<
    \stemDown \approximatePitch a''
    \\
    \stemUp \approximatePitch c'
  >>
  \stemUp
  \approximatePitch c'
  \approximatePitch b'
  \approximatePitch a''
  <<
    \approximatePitch a''
    \\
    \approximatePitch c'
  >>

  \break

  \time 3/8

  << {
    <a' \approximatePitch a''>
    <b' \approximatePitch a''>
    <c'' \approximatePitch a''>
    <\approximatePitch a a'>
    <\approximatePitch a b'>
    <\approximatePitch a c''>
  } \\ {
    <\approximatePitch a a'>
    <\approximatePitch a b'>
    <\approximatePitch a c''>
    <a' \approximatePitch a''>
    <b' \approximatePitch a''>
    <c'' \approximatePitch a''>
  } >>

  \break

  <>^\markup \teeny "Stems neutral"
  \stemNeutral
  \approximatePitch c'
  \approximatePitch b'
  \approximatePitch a''
  <\approximatePitch a \approximatePitch c'>
  <\approximatePitch a' \approximatePitch c''>
  <\approximatePitch a'' \approximatePitch c'''>

  \break

  <c' \approximatePitch a'>
  <c' \approximatePitch b'>
  <c' \approximatePitch c''>
  <\approximatePitch a' a''>
  <\approximatePitch b' a''>
  <\approximatePitch c'' a''>

  \break

  <\approximatePitch c' a'>
  <\approximatePitch c' b'>
  <\approximatePitch c' c''>
  <a' \approximatePitch a''>
  <b' \approximatePitch a''>
  <c'' \approximatePitch a''>
}

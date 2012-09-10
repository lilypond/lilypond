\version "2.17.0"

\header {
  texidoc = "Cross-staff slurs are accounted for in vertical spacing.
"
}

%#(ly:set-option 'debug-skylines #t)

\new PianoStaff <<
  \new Staff = "up" {
    <e' c'>8
    \change Staff = "down"
    \slurDown
    g,,8 ( fis g
    \change Staff = "up"
    <g'' c''>8 )
    \change Staff = "down"
    e8 dis e
    \change Staff = "up"
    \break
    a'8 a'8 a'8^\markup \column { "f" "o" "o" } a'8 a'8 a'8 a'8 a'8
  }
  \new Staff = "down" {
    \clef bass
    % keep staff alive
    s1 s1
  }
>>

\new PianoStaff <<
  \new Staff = "up" {
    <e' c'>8
    \change Staff = "down"
    \slurDown
    g,,8 (
    \change Staff = "up"
    fis'' g <g c''>8 )
    \change Staff = "down"
    e8 dis e
    \change Staff = "up"
    \break
    a'8 a'8 a'8^\markup \column { "f" "o" "o" } a'8 a'8 a'8 a'8 a'8
  }
  \new Staff = "down" {
    \clef bass
    % keep staff alive
    s1 s1
  }
>>

\new PianoStaff <<
  \new Staff = "up" {
    R1
    <e' c'>8
    \change Staff = "down"
    \slurUp
    g,,8 (
    \change Staff = "up"
    fis'' g \override Stem #'direction = #UP
    <g'' c''>8 )
    \change Staff = "down"
    e8 dis e
    \change Staff = "up"
  }
  \new Staff = "down" {
    \clef bass
    % keep staff alive
    a8 a8 a8_\markup \column { "f" "o" "o" }
    a8 a8 a8 a8 a8
    \break s1
  }
>>

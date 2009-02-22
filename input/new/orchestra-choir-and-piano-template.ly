\version "2.12.0"
\header {
  lsrtags = "template"
  texidoc = "
This template demonstrates the use of nested @code{StaffGroup}
and @code{GrandStaff} contexts to sub-group instruments of the same
type together, and the use of @code{\transpose} for transposing
instuments so that all the music can be entered in the same key of
C.
"
  doctitle = "Orchestra, choir and piano template"
}

fluteMusic    = \relative c' { c1 d }
saxMusic      = \transpose bes c' { \key c \major \relative c' { c1 d } }
oboeMusic     = \relative c' { c1 d }
clarinetMusic = \transpose a c' { \key c \major \relative c' { c1 d } }
bassoonMusic  = \transpose a c' { \key c \major \relative c { c1 d } }
trumpetMusic  = \transpose bes c' { \key c \major \relative c' { c1 d } }
tromboneMusic = \relative c { c1 d }
hornOneMusic  = \transpose f c' { \key c \major \relative { c1 d } }
hornTwoMusic  = \transpose f c' { \key c \major \relative c { c1 d } }
sopranoMusic  = \relative c'' { c1 d }
sopranoLyrics = \lyricmode { Sop -- ra }
altoOneMusic  = \relative c' { c1 d }
altoOneLyrics = \lyricmode { A -- one }
altoTwoMusic  = \relative c' { c1 d }
altoTwoLyrics = \lyricmode { A -- two }
tenorMusic    = \relative c' { c1 d }
tenorLyrics   = \lyricmode { Ten -- or }
pianoRHMusic  = \relative c' { c1 d }
pianoLHMusic  = \relative c { c1 d }

\score {
  <<  % Start full staff group
    \new StaffGroup <<  % Woodwinds
      \new Staff {  % Flute
        \set Staff.instrumentName = "Flute"
        \fluteMusic
      }
      \new StaffGroup <<
        \new Staff {  % Bb Sax
          \set Staff.instrumentName = \markup { \concat {"B" \flat} "Sax" }
          \saxMusic
        }
        \new Staff {  % Oboe
          \set Staff.instrumentName = "Oboe"
          \oboeMusic
         }
        \new Staff {  % Clarinet in A
          \set Staff.instrumentName = "Clarinet"
          \clarinetMusic
        }
      >>
      \new Staff {  % Bassoon
        \set Staff.instrumentName = "Bassoon"
        \clef bass
        \bassoonMusic
      }
    >>
    \new StaffGroup <<  % Start Brass group
      \new Staff {  % Trumpet
        \set Staff.instrumentName = "Trumpet"
        \trumpetMusic
      }
      \new Staff {  % Trombone
        \set Staff.instrumentName = "Trombone"
        \clef bass
        \tromboneMusic
      }
      \new GrandStaff << % Horns need a GrandStaff (same instrument)
        \new Staff {  % Horn 1
          \set Staff.instrumentName = "Horn 1"
          \hornOneMusic
        }
        \new Staff {  % Horn 2
          \set Staff.instrumentName = "Horn 2"
          \clef bass
          \hornTwoMusic
        }
      >>
    >>  % End Brass group
    \new ChoirStaff <<
      \new Staff {
        \set Staff.instrumentName = "S"
        \new Voice = "soprano"
        \sopranoMusic
      }
      \new Lyrics \lyricsto "soprano" { \sopranoLyrics }
      \new GrandStaff \with { \accepts Lyrics } <<
        \new Staff  {
          \set Staff.instrumentName = "A1"
          \new Voice = "altoOne"
          \altoOneMusic
        }
        \new Lyrics \lyricsto "altoOne" { \altoOneLyrics }
        \new Staff {
          \set Staff.instrumentName = "A2"
          \new Voice = "altoTwo"
          \altoTwoMusic
        }
        \new Lyrics \lyricsto "altoTwo" { \altoTwoLyrics }
      >>
      \new Staff {
        \set Staff.instrumentName = "T"
        \clef "treble_8"
        \new Voice = "tenor"
        \tenorMusic
      }
      \new Lyrics \lyricsto "tenor" { \tenorLyrics }
    >>  % End ChoirStaff
    \new PianoStaff \with { \consists Instrument_name_engraver } <<
      \set PianoStaff.instrumentName = "Piano"
      \new Staff { \pianoRHMusic }
      \new Staff {
        \clef bass
        \pianoLHMusic
      }
    >>  % End PianoStaff
  >>  % End full staff group
}


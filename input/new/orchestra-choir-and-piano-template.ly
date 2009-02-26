\version "2.12.0"
\header {
  lsrtags = "template"
  texidoc = "
This template demonstrates the use of nested @code{StaffGroup}
and @code{GrandStaff} contexts to sub-group instruments of the same
type together, and the use of @code{\\transpose} for transposing
instruments.  All music in variables is stored in C.  Music may be
entered in C or, alternatively, entered in the instrument key and
transposed to C (see trumpet for an example) before being assigned
to a variable.
"
  doctitle = "Orchestra, choir and piano template"
}

fluteMusic    = \relative c' { \key c \major c1 d }
saxMusic      = \relative c' { \key c \major c1 d }
oboeMusic     = \relative c' { \key c \major c1 d }
clarinetMusic = \relative c' { \key c \major c1 d }
bassoonMusic  = \relative c  { \key c \major c1 d }
trumpetMusic  = \transpose c' bes {
  \relative c' { \key d \major d1 e }
}
tromboneMusic = \relative c  { \key c \major c1 d }
hornOneMusic  = \relative c' { \key c \major c1 d }
hornTwoMusic  = \relative c  { \key c \major c1 d }
sopranoMusic  = \relative c'' {\key c \major c1 d }
sopranoLyrics = \lyricmode { Sop -- ra }
altoOneMusic  = \relative c' { \key c \major c1 d }
altoOneLyrics = \lyricmode { A -- one }
altoTwoMusic  = \relative c' { \key c \major c1 d }
altoTwoLyrics = \lyricmode { A -- two }
tenorMusic    = \relative c' { \key c \major c1 d }
tenorLyrics   = \lyricmode { Ten -- or }
pianoRHMusic  = \relative c' { \key c \major c1 d }
pianoLHMusic  = \relative c  { \key c \major c1 d }

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
          \transposition bes
          \transpose bes c' \saxMusic
        }
        \new Staff {  % Oboe
          \set Staff.instrumentName = "Oboe"
          \oboeMusic
         }
        \new Staff {  % Clarinet in A
          \set Staff.instrumentName = "Clarinet"
          \transposition a
          \transpose a c' \clarinetMusic
        }
      >>
      \new Staff {  % Bassoon
        \set Staff.instrumentName = "Bassoon"
        \clef bass
        \transposition a,
        \transpose a c' \bassoonMusic
      }
    >>
    \new StaffGroup <<  % Start Brass group
      \new Staff {  % Trumpet
        \set Staff.instrumentName = "Trumpet"
        \transposition bes
        \transpose bes c' \trumpetMusic
      }
      \new Staff {  % Trombone
        \set Staff.instrumentName = "Trombone"
        \clef bass
        \tromboneMusic
      }
      \new GrandStaff << % Horns need a GrandStaff (same instrument)
        \new Staff {  % Horn 1
          \set Staff.instrumentName = "Horn 1"
          \transposition f
          \transpose f c' \hornOneMusic
        }
        \new Staff {  % Horn 2
          \set Staff.instrumentName = "Horn 2"
          \clef bass
          \transposition f
          \transpose f c' \hornTwoMusic
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


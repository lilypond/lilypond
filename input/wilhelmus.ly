\version "2.12.0"

\header {
  texidoc = "Wilhelmus van Nassouwe"
  title = "Wilhelmus van Nassouwe"
  composer = "Valerius"
  subtitle = "Neder-landtsche gedenck-clanck"
  opus = "1626"
  copyright = "public domain"
  enteredby = "janneke@gnu.org"
}

%% hymn tricks
noclefs =  {
  s1
  \override Staff.Clef #'break-visibility =
    #all-invisible
}

setMargins = {
  %% \context Staff \applyContext #(set-extra-space 'TimeSignature
  %% 'first-note -> extra-space 4.5

  \override Staff.TimeSignature #'space-alist #'first-note = #'(extra-space . 9.5)
  \override Staff.KeySignature #'space-alist #'staff-bar = #'(extra-space . 15)
  \override Score.LeftEdge #'space-alist #'key-signature = #'(extra-space . 1)
  
}

pipeSymbol = {
  %% Set height of bar line to 2 staff-spaces
  \once \override Staff.BarLine #'bar-size = #2
  %% Move barline one staff-space up
  \once \override Staff.BarLine #'extra-offset = #'(0 . 1)
  \bar "|"
}

myBreak = { \bar "" \break }

\layout {
  indent = 0.0\mm
  line-width = 120.0\mm
  textheight = 270.0\mm
}

voice =  \relative c' {
  \clef violin
  \key g \major
  d4 | g g a a b | a8 \myBreak
  b8 | c4 b a a | g2. \myBreak

  d4 | g g a a | b a8 \myBreak
  b8 | c4 b a a| g2.  \myBreak

  %% Hack for better left tekst margin
  %% b8[ c] | d2 e4 d2 c4 | b a8 \myBreak
  \set melismaBusyProperties = #'()
  b8[ c] |
  \unset melismaBusyProperties
  d2 e4 d2 c4 b | a8 \myBreak
  b8 | c4 b a g | a2. \myBreak

  d,4 | g4.\melisma a8\melismaEnd b2 a2 g4 | fis e8 \myBreak
  d8 | e4 g g fis | 
  
  \override NoteHead #'style = #'neomensural

  g\breve

  %% justified lines:
  %%\override Staff.BarLine #'extra-offset = #'(12 . 0)
  %% ragged-right:
  \override Staff.BarLine #'extra-offset = #'(23 . 0)
  \bar "|."
}

stich =  \relative c'' {
  \override Staff.NoteCollision #'merge-differently-dotted = ##t
  \set fontSize = #-3
  \override Stem #'length-fraction = #(magstep -3)

  \voiceTwo
  \partial 4
  s4 s s a8[ b] c[ a] s4.
  s8 s4 s a8 g s4 s2.

  s4 s s a8[ b] c[ a] s4.
  s8 s4 s a8 g s4 s2.
  
  s8 s s2 s4 s2 s4 s s8
  s8 s4 s s s s2.

  s4 g8[ fis g a] s2 s2 s4 s s8
  s8 e8[ fis]
}

modernText = \lyricmode {
  Wil -- hel -- mus van Nas -- sou -- we
  ben ik van duit -- sen bloed,

  den va -- der -- land ge -- trou -- we
  blijf ik tot in den dood.

  Een prin -- se van O -- ran -- je
  ben ik vrij on -- ver -- veerd,

  den ko -- ning van His -- pan -- je
  heb ik al -- tijd ge -- eerd.
}

text = \lyricmode {
  Wil -- hel -- mus van Nas -- sou -- we
  Ben ick van duyt -- schen bloet,
  Den Va -- der -- landt ghe -- trou -- we,
  blyf ick tot in den doot!
  %% Hack for better left text margin:
  %% Een Prin -- ce van O -- ran -- gien
  Een " " Prin -- ce van O -- ran -- gien
  Ben ick, vry, on -- ver -- veert;
  Den Co -- ninck van His -- pan -- gien
  Heb ick al -- tijt ghe -- eert.
}

oneHalfNoteTime = \markup {
  \override #'(baseline-skip . 0)
  \column {
    \line { \number "1" }
    \line { \smaller \smaller \note #"2" #-0.6 }
  }
}

\layout {
  ragged-right = ##t

  \context {
    \Score
    %% defaults
    %% (shortest-duration-space . 2.0)
    %% (spacing-increment . 1.2)
    %% (base-shortest-duration . ,(ly:make-moment 1 8))
    %% wider spacing
    \override SpacingSpanner #'shortest-duration-space = #3.0
    \override SpacingSpanner #'spacing-increment = #1.2
    \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
  }
}

\score {
  \context Score <<
    \context Staff <<
      \override Staff.StaffSymbol #'width = #'80
      \set Staff.autoBeaming = ##f
      \set Score.timing = ##f
      \setMargins
     
      %% Less vertical space needed with lyrics
      \override Staff.VerticalAxisGroup #'minimum-Y-extent = #'(2 . 2)
      
      %% Custom time signature
      \override Staff.TimeSignature #'stencil = #ly:text-interface::print
      \override Staff.TimeSignature #'text = #oneHalfNoteTime
      \override Staff.TimeSignature #'style = #'()
      \noclefs
    
      \new Voice =  "voice" \voice
      \new Voice =  "stich" \stich
    >>
    \lyricsto "voice" \new Lyrics {
      \override VerticalAxisGroup #'minimum-Y-extent = #'(2 . 2)
      \text
    }
  >>
  \layout {}
  \midi {}
}

%%% Local variables:
%%% LilyPond-indent-level:2
%%% End:

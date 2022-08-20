\version "2.23.12"

\header {
  lsrtags = "ancient-notation, vocal-music"

  texidoc = "
Using tags, it is possible to produce both mensural and modern
notation from the same music.  In this snippet, a function
@code{menrest} is introduced, allowing mensural rests to be pitched as
in the original, but with modern rests in the standard staff position.
Tags can also be used where other differences are needed: for example
using @qq{whole measure rests} (@code{R1}, @code{R\\breve}, etc.) in
modern music, but normal rests (@code{r1}, @code{r\\breve}, etc.) in
the mensural version.  Converting mensural music to its modern
equivalent is usually referred to as @qq{transcription}.
"

  doctitle = "Using tags to produce mensural and modern music from the same source"
}


menrest = #(define-music-function (note)
  (ly:music?)
#{
    \tag #'mens $(make-music 'RestEvent note)
    \tag #'mod $(make-music 'RestEvent note 'pitch '())
#})

MenStyle = {
  \autoBeamOff
  \override NoteHead.style = #'petrucci
  \override Score.BarNumber.transparent = ##t
  \override Stem.neutral-direction = #up
}

finalis = \section

Music = \relative c'' {
  \set Score.tempoHideNote = ##t
  \key f \major
  \time 4/4
  g1 d'2 \menrest bes4 bes2 a2 r4 g4 fis2.
  \finalis
}

MenLyr = \lyricmode { So farre, deere life, deare life }
ModLyr = \lyricmode { So far, dear life, dear life }

\score {
  \keepWithTag #'mens {
    <<
      \new MensuralStaff
      {
        \new MensuralVoice = Cantus
          \clef "mensural-c1" \MenStyle \Music
      }
      \new Lyrics \lyricsto Cantus \MenLyr
    >>
  }
}

\score {
  \keepWithTag #'mod {
    \new ChoirStaff <<
      \new Staff
      {
        \new Voice = Sop \with {
          \remove "Note_heads_engraver"
          \consists "Completion_heads_engraver"
          \remove "Rest_engraver"
          \consists "Completion_rest_engraver" }
        {
          \shiftDurations #1 #0 { \autoBeamOff \Music }
        }
      }
      \new Lyrics \lyricsto Sop \ModLyr
    >>
  }
}

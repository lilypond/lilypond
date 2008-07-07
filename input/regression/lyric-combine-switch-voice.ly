\header {

  texidoc = "Switching the melody to a different voice works even
if the switch occurs together with context instantiation."

}

\version "2.11.51"

<<
  \relative \new Voice = "lahlah" {
    \set Staff.autoBeaming = ##f
    c4
    <<
      \new Voice = "alternative" {
        \voiceOne
        \times 2/3 {
	  %% show associations clearly.
          \override NoteColumn #'force-hshift = #-3
          r8 f g
        }
      }
      {
        \voiceTwo
        f8.[ g16]
        \oneVoice
      } >>
    a8( b) c
  }
  \new Lyrics \lyricsto "lahlah" {
    %% Tricky: need to set associatedVoice
    %% one syllable too soon!
    
    \set associatedVoice = alternative % applies to "ran"
    Ty --
    ran --
    \set associatedVoice = lahlah % applies to "sau"
    no --
    sau -- rus Rex
  }
>>




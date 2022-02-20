\header
{
texidoc = "switching voices in the middle of the lyrics is possible using @code{lyricsto}."
  }
\version "2.19.21"

<<
  \relative \new Voice = "lahlah" {
    c'4
    <<
      \new Voice = "alternative" {
        \voiceOne
	\override NoteColumn.force-hshift = #-3
	a'4 
      } 
      { \voiceTwo
        c,4
	\oneVoice
      } >>
    c4 c 
    
  }
  \new Lyrics \lyricsto "lahlah" {
    \set associatedVoice = alternative
    two  
    \set associatedVoice = lahlah
    two this
  } 
>>

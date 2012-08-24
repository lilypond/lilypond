\version "2.16.0"

\header {

  texidoc = "When figures appear inside a voice, @code{ignoreFiguredBassRest} 
  causes all figures on rests to be discarded and all spanners ended.
  If set to @code{#f}, figures on rests are printed.
"

}


\paper { ragged-right = ##t }


<<
  \new Voice <<
    {
      \clef bass
      c4 r c r |
      c4 r c r |
      c4 r c r |
    }
    \figuremode {
      % Default:
      <3>4 <3> <3> \set Staff.useBassFigureExtenders = ##t <3> \set Staff.useBassFigureExtenders = ##f |
      % ignore figures on rests, regardless of extenders
      \set Staff.ignoreFiguredBassRest = ##t
      <3>4 <3> <3> \set Staff.useBassFigureExtenders = ##t <3> \set Staff.useBassFigureExtenders = ##f |
      % print figures on rests, regardless of extenders
      \set Staff.ignoreFiguredBassRest = ##f
      <3>4 <3> <3> \set Staff.useBassFigureExtenders = ##t <3> \set Staff.useBassFigureExtenders = ##f |
    }
  >>
>>



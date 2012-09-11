\version "2.16.0"

\header {

  texidoc = "Bass figures and extenders shall also work correctly if the
figure has a different duration than the bass note. In particular, if a
timestep does not have a new figure (because the old figure still goes on),
extenders should be drawn and not be reset.
"

}


\paper { ragged-right = ##t }


<<
  \context Voice <<
    {
      \clef bass
      c4 c c c | c c c c |
    }
    \figuremode {
      <3>2 <3> \set Staff.useBassFigureExtenders = ##t <3> \set Staff.useBassFigureExtenders = ##f <3>
    }
  >>
>>



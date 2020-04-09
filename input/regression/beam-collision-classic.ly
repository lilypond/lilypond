\header {
  texidoc = "Beam collisions from modern works"
  }

\layout {
  ragged-right = ##t
%  debug-beam-scoring = ##t
}

\version "2.19.21"

\new Staff
{
  % Stockhausen (without hemiolas)
  \relative {
    \stemUp
    a''8[ \clef bass es,,,,
	r8 <b' g'>
	\clef G
	gis''']
  }
  r8 r4 |

  % Ligeti 1st etude.
  \relative
  <<
    { g'8[ a b c d] } \\
    { s4. <f, f'>4. }
    >>
  r4.

  % Ligeti 1st etude.
  \relative
  <<
    {
      s4. <g' g'>4.
    } \\
    {
%      \override Beam.inspect-quants = #'(-4 . -3)
      a8[ d e f g]
    }
    >>
  r4.

  % Ligeti 1st etude.
  \relative
  <<
    { <d' d'>2. } \\
    { a'8[ b c] }
    >>

  % Schubert morgenlied.
  \clef bass
  \relative {
    a,16[ d fis d a d]
  }

}

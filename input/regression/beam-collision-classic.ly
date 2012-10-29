\header {
  texinfo = "Beam collisions from modern works"
  }

\layout {
  ragged-right = ##t
%  debug-beam-scoring = ##t
}

\version "2.17.6"

\new Staff
{
  % Stockhausen (without hemiolas)
  \relative c''' {
    \stemUp
    a8[ \clef bass es,,,,
	r8 <b' g'>
	\clef G
	gis''']
  }
  r8 r4 |

  % Ligeti 1st etude.
  \relative c''
  <<
    { g8[ a b c d] } \\
    { s4. <f, f'>4. }
    >>
  r4.

  % Ligeti 1st etude.
  \relative c''
  <<
    {
      s4. <g g'>4.
    } \\
    {
%      \override Beam.inspect-quants = #'(-4 . -3)
      a8[ d e f g]
    }
    >>
  r4.
  
  % Ligeti 1st etude.
  \relative c'
  <<
    { <d d'>2. } \\
    { a'8[ b c] }
    >>

  % Schubert morgenlied.
  \clef bass
  \relative c {
    a16[ d fis d a d]
  }
  
}

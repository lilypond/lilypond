\version "2.3.4"
\header { texidoc = "@cindex Incipit
This example shows how to make an ``incipit'' to indicate 
scordatora tuning of a violin part, by overriding the @code{style} of
a @code{TimeSignature}.
Here are the two first bars of Biber's Rosary sonata III. " }




violinincipit = \relative c''{
  \clef "french"
  \time 2/2
  \override Staff.TimeSignature  #'style = #'old
  a4. b8 c4 fis |
%  <b fis' b d>1
  \override Staff.TimeSignature  #'style = #'C
}

bcincipit = \relative c{
  \clef bass
  \override Staff.TimeSignature  #'style = #'old
  b2. cis4 | 
  \override Staff.TimeSignature  #'style = #'C
}

violin = \relative c''{
% Key signatures with different alterations in different octaves
% are broken since 1.3.58!
%  \specialkey \keysignature f' fis'' g' gis''
  \key d \major
  \time 2/2
  \clef treble

  a4. b8 c4 fis |
  gis~ gis8 fis16^\trill (e) b8 c
  <<{ a d}\\ { es,4}>>|
}

BC = \relative c{
  \key d \major
  \time 2/2
  \clef "bass"

 \key \default
  b2. cis4 | 
  d e fis g |
}

\score{
  <<
    \context Staff = violin {{
      \override Staff.Clef  #'transparent = ##t
      \violinincipit \bar ".|" 
      \revert Staff.Clef #'transparent 
      \endincipit
      \violin
    }}
    \new Staff {{
      \override Staff.Clef  #'transparent = ##t
      \bcincipit \bar ".|" 
      \revert Staff.Clef #'transparent 
      \endincipit
      \BC
    }}
  >>
	\paper { raggedright = ##t }
}  



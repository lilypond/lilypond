\version "1.3.110"
%{
  See scm/chord-names.scm: chord::names-alist-american
  James Hammons <jlhamm@pacificnet.net>
%}

\include "english.ly"
#(set! chord::names-alist-american
      (append 
      '(
	;; any changes here, see scm/chord-name.scm

	)
      chord::names-alist-american))

chord = \notes\transpose c''\chords{
	\property ChordNames.ChordName \override #'style = #'american
c         % Major triad
cs:m      % Minor triad
df:m5-    % Diminished triad
c:5^3     % Root-fifth chord
c:4^3     % Suspended fourth triad
c:5+      % Augmented triad
c:2^3     % "2" chord
c:m5-.7-  % Diminished seventh
c:7+      % Major seventh
c:7.4^3   % Dominant seventh suspended fourth
c:5+.7    % Augmented dominant seventh
c:m5-.7   % "Half" diminished seventh
c:5-.7    % Dominant seventh flat fifth
c:5-.7+   % Major seventh flat fifth
c:m7+     % Minor-major seventh
c:m7      % Minor seventh
c:7       % Dominant seventh
c:6       % Major sixth
c:m6      % Minor sixth
c:9^7     % Major triad w/added ninth
c:6.9^7   % Six/Nine chord
c:9       % Dominant ninth 
c:7+.9    % Major ninth
c:m7.9    % Minor ninth
}

\score{
<
\context ChordNames \chord
\context Staff \chord
>
    \paper
    {
        	\translator { 
			\ChordNamesContext
			ChordNames \override #'word-space = #1 
		}
    }
}

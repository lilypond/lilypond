%
% Make sure the correct msamxx.tfm is where lily can find it
% (ie cwd or lily's tfm dir).
%
% For normal (20pt) paper, do
%
%   cp locate `msam9.tfm` $LILYPONDPREFIX/tfm
%

#(set! chord::names-alist-american
      (append 
      '(
	 ;; any changes here, see scm/chord-names.scm


	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (("o7" (type . "super"))))
	 ;jazz: the delta, see jazz-chords.ly
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .  (("N" (type . "super") (style . "msam") (size . -3))))

	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (("x7" (type . "super"))))
	 ; slashed o
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (("o" (type . "super")) ("/" (size . -2) (offset . (-0.58 . 0.5))) ("7" (type . "super"))))

	)
      chord::names-alist-american))

chord = \notes\transpose c''\chords{
\property ChordNames.chordNameStyle = "american"
c:m5-.7-
c:m5-.7
}

\score{
<
\context ChordNames \chord
\context Staff \chord
>
    \paper
    {
        \translator { \ChordNameContext chordNameWordSpace = #1 }
        \translator { \LyricsContext textScriptWordSpace = #0.3 }
    }
}


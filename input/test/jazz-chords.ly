\version "1.3.96"
%% This should only be necessary if your kpathsea setup is broken
%%
%% Make sure the correct msamxx.tfm is where lily can find it
%% (ie cwd or lily's tfm dir).
%%
%% For normal (20pt) paper, do
%%
%%   cp locate `msam9.tfm` $LILYPONDPREFIX/tfm

#(set! chord::names-alist-american
      (append 
      '(
	 ;; any changes here, see scm/chord-names.scm


	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . (super "o7"))
	 ;jazz: the delta, see jazz-chords.ly
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) .  (super ((font-family . math) "N")))
	 (((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (rows ((raise . 1) "o") ((raise . 0.5) ((kern . -0.5) ((font-relative-size . -3) "/"))) "7")) ; slashed o

	 ;(((0 . 0) (2 . -1) (4 . -1) (6 . -1)) . (super "x7"))
	 ; slashed o
	)
      chord::names-alist-american))

chord = \notes\transpose c''\chords{
	\property ChordNames.ChordNames \push #'style = #"american"
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
	  \translator { 
		\ChordNamesContext
		ChordNames \push #'word-space = #1 
	  }
    }
}


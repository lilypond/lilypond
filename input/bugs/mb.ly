
% mb.ly: midi-bug
% midi notes get stuck

%{
From: Mats Bengtsson <mats.bengtsson@s3.kth.se>
Subject: Re: request simple .ly showing MIDI tie bug 
To: Jan Nieuwenhuizen <janneke@gnu.org>
Date: Mon, 26 Feb 2001 23:18:06 +0100

I was lucky enough to find a short snippet which triggered
the bug (a few bars from David Lattermanns typesetting of
the Dvorak Bagatelles. I spent the Sunday upgrading them
to 1.3.131). I hope it simplifies the bug search.
%}

\score{
  \notes \relative c{
    \time 2/4;
    \clef bass;
    \property Staff.midiInstrument = "harmonica"
    <
      \context Voice=ua {
        \stemDown
	e4 a, | b2 |
      }
      \context Voice=ub {
        \stemUp
	e2 ~ | e4 dis |
      }
    >
    R2*5 |
    c'2
  }
\paper{}
\midi{\tempo 4 = 140;}
}


\header {
  filename =    "just-friends.ly";
  title =       "Just Friends";
  description = "Jazz chords example with bar lines";
  opus =        "";
  source =      "mutex@gmd.de";
  composer =    "";
  enteredby =   "jcn";
  copyright =   "Public Domain";
}

%{
In response to

Message-Id: <19990206153950.A7474@gil.physik.rwth-aachen.de>
Date: Sat, 6 Feb 1999 15:39:50 +0100
From: Christoph Kukulies <kuku@gilberto.physik.RWTH-Aachen.DE>
Subject: chord bars

I was looking out for a FAQ on the www.gmd.de/Misc/Music page but didn't
find any so bear with me when asking a FAQ question:

I want to generate some sheets that contain only the chords (changes)
of a certain tune (Jazz, Real Book, for example), like

              Just Friends

[...]

%}

\score{
	\context ChordNames \chords{
		\property Score.barAtLineStart = 1
		\repeat semi 2 {
			f1:maj f:maj f:7 bes:7
			c:maj c:maj es es
			d:7 g:7 b2:7/fis e:7 a1:7\break
			d:7 d:7 d:7 g2:7 ges:7

			f1:maj f1:maj f:7 bes:7
			c:maj c:maj es es
			d:7 g:7 b2:7/fis e:7 a1:7\break
			d:7 d2:7 g:7 c1:6 g2:7 c:7
		}
	}
	\paper{
		indent = 0.0\mm;
		linewidth = 100.0\mm;
		\translator{
			\ChordNamesContext
			barAuto = "1";
			\consists Bar_engraver;
			\consists "Repeat_engraver";
		}
	}
}



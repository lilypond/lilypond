\header{
  title =	 "Konzert Nr. 3 Es dur";
  subtitle = "f\\\"ur Horn und Orchester";
  composer =	 "Wolfgang Amadeus Mozart (1756-1792)";
  enteredby =	 "HWN";
  opus = "K.V. 447";

  copyright = "public domain";
  instrument = "Horn in Es";
  
  mutopiatitle =	 "Horn Concerto 3";
  mutopiacomposer = "W.A.Mozart";
  mutopiaopus = "KV447";
  style = "classical";
  tagline =    "\\\\This music is part of the Mutopia project, http://sca.uwaterloo.ca/Mutopia/\\\\It has been typeset and placed in the public domain by Han-Wen Nienhuys.\\\\Unrestricted modification and redistribution is permitted and encouraged - copy this music and share it!";
  maintainer = "hanwen@cs.uu.nl";
  lastupdated = "1999/Oct/16";
  source = "Breitkopf & Haertel + own modifications";
}

%%%%%%% This is the main file.

%{

This is the Mozart 3 for horn.  It's from an old and fumbled part I
have with lots of unidentified staccato dots, marcato's and dynamic
markings; I left off most of them.

Some of the marks are mine. Some of them are my teachers'.  Some of
them are my teachers' teachers.  Some of them are B&H's. Some of them
are Mozart's.

--hwn

%}

longgrace = \property Grace.stemStyle = ""
aftergrace = \property Grace.graceAlignPosition = \right


\version "1.3.4";

\include "allegro.ly"
\include "romanze.ly"
\include "rondo.ly"

\paper{
 \translator { \StaffContext \consists Mark_engraver;  }
 \translator { \ScoreContext
    	skipBars = 1;
  }
  linewidth = 180. \mm;
}

\score
{
	< \allegro
	  \property Score.midiInstrument = "french horn"
	>
	\paper{ }
	\header { piece = "allegro"; opus = ""; }	
	\midi{ \tempo 4=90; }
}

\score
{
	< \romanze
	  \property Score.midiInstrument = "french horn"
	>
	\header { piece = "romanze"; opus = ""; }	
	\midi{ \tempo 4 = 70;  }
	\paper{}
}
\score
{
	< \rondo
	  \property Score.midiInstrument = "french horn"
	>
	\header { piece = "rondo"; opus = ""; }
	\paper{
		\translator { \BarNumberingStaffContext }
	}
	\midi{ \tempo 4 = 100; }
}

%%%%%%%%% HAS TO BE ADJUSTED to pl32


% martien.ly
%
% Viola Quartet
% Martien Lohman (194x-????) 
%
% quite alive at the time of writing; 
% copyright by ml
%
% lilypond: src/\stem.cc:42: void Stem::set_\stemend(double): Assertion `(dir > 0 && se >= maxnote) || (se <= minnote && dir <0)' failed.
% 
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright.
%
% \barnumbering5
% \barnumberstyle\boxed

globalmusic= \music {
		\meter {2/4}
%		\key fis
		\skip {56*2}
%		\key bes es as
		\skip {8*2}
		\meter {6/8}
		\skip{ 48*8}
		\meter {2/4}
		\skip {16*2}
%		\key fis
}

include "mlalt.ly"
include "mlvio1.ly"
include "mlvio2.ly"
include "mlcello.ly"

\score {
	\staff { 
		\melodic 
		\music { alto }
		\music { globalmusic }
	}
	\staff { 
		\melodic 
		\music { violinI }
		\music { globalmusic }
	}
	\staff { 
		\melodic 
		\music { violinII }
		\music { globalmusic }
	} 
	\staff { 
		\melodic 
		\music { cello }
		\music { globalmusic }
	} 
	\paper {
		\unitspace 24\mm
		\width 195\mm
		\output "martien.out"
	}
	\midi { 
		\tempo 4:80
	}
}

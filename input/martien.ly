%% BUG in MIDI!
%
% Viola Quartet
% Martien Lohman (194x-????) 
%
% quite alive at the time of writing; 
% copyright by ml
%
% 
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright. %% VAAG!
%
% \barnumbering5
% \barnumberstyle\boxed

globalmusic= \melodic{
		\meter 2/4;
%		\key fis
		\skip 2*56;
%		\key bes es as
		\skip 2*8;
		\meter 6/8;
		\skip 8*48;
		\meter 2/4;
		\skip 2*16;
%		\key fis
}

include "mlalt.ly"
include "mlvio1.ly"
include "mlvio2.ly"
include "mlcello.ly"

\score{
	\staff{ melodicregs globalmusic alto }
	\staff{melodicregs  globalmusic violinI }
	\staff{ melodicregs globalmusic violinII }
	\staff{ melodicregs globalmusic cello }
	\paper{
		\unitspace 24\mm
		\width 195\mm
		\output "martien.out"
	}
	\midi{ 
		\tempo 4:80
	}
}

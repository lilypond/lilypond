% martien.ly
%
% Viola Quartet
% Martien Lohman (194x-????) 
%
% quite alive at the time of writing; 
% copyright by ml
%
% lilypond: src/stem.cc:42: void Stem::set_stemend(double): Assertion `(dir > 0 && se >= maxnote) || (se <= minnote && dir <0)' failed.
% 
% The purpose of this file is to demonstrate features of LilyPond;
% respect the copyright.
%
% \barnumbering5
% \barnumberstyle\boxed

include "mlalt.ly"
include "mlvio1.ly"
include "mlvio2.ly"
include "mlcello.ly"

score {
	staff { 
		melodic 
		music { alto }
		commands {
%			meter 2*4
			clef "alto"
			key $fis$
			skip 56:0
			key $bes es as$
			skip 8:0
%			meter 6*8
			skip 8:0
%			meter 2*4
			skip 16:0
			key $fis$
		}
	}
	staff { 
		melodic 
		music { violin1 }
		commands {
%			meter 2*4
			clef "violin"
			key $fis$
			skip 56:0
			key $bes es as$
			skip 8:0
%			meter 6*8
			skip 8:0
%			meter 2*4
			skip 16:0
			key $fis$
		}
	}
	staff { 
		melodic 
		music { violin2 }
		commands {
%			meter 2*4
			clef "violin"
			key $fis$
			skip 56:0
			key $bes es as$
			skip 8:0
%			meter 6*8
			skip 8:0
%			meter 2*4
			skip 16:0
			key $fis$
		}
	} 
	staff { 
		melodic 
		music { cello }
		commands {
%			meter 2*4
			clef "bass"
			key $fis$
			skip 56:0
			key $bes es as$
			skip 8:0
%			meter 6*8
			skip 8:0
%			meter 2*4
			skip 16:0
			key $fis$
		}
	} 
	commands {
		meter 2*4
		skip 56:0
		bar "||"
		skip 8:0
		meter 6*8
		bar "||"
		skip 8:0
		meter 2*4
		bar "||"
		skip 16:0
		bar "||"
	}
	paper {
%		unitspace 16mm
		unitspace 24mm
		width 195mm
		output "martien.out"
	}
}

%
% test file to get a4 paper really filled,
% without having to resort to the ever-ugly oversizing hack:
%
%   `textheight = 310.0\mm'
%
% process this file with ly2dvi, and make sure footskip/headsep are set
% at a reasonably (small) value.  -- jcn
%
\header{
tagline="Ligly";
}
\include "paper13.ly";
\score{
	\context Voice \notes\relative c'{
		\clef alto;
		\repeat "unfold" 36 c1
	}
	\paper{
		\paper_thirteen
		indent = 0.0\mm;
		% URG
		% Vertical space is rather precious when typesetting
		% music.  But we can only set textheight here, and must
		% guess and subtract the height needed for headers and 
		% footers.  If we want a header or footer on some page,
		% all other pages suffer shortened `textheight'.
		% Try the maximum for a4, without loosing footers:
		textheight = 297.0\mm - 7.0\mm;
		papersize = "a4";
		linewidth = 15.0\mm;
		\translator {
			\StaffContext
			StaffMinimumVerticalExtent = #(cons 0 0)
		}
	}
}

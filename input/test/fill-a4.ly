\version "2.3.2"
%possible rename to paper-fill-a4.ly -gp
% candidate for reg -gp
% Han says no, but keeping this comment temporarily so that I don't
% forget and nominate it for reg again.  :)  -gp

% test file to get a4 paper really filled,
% without having to resort to the ever-ugly oversizing hack:
%
%   `textheight = 310.0\mm'
%
% process this file with lilypond, and make sure footskip/headsep are set
% at a reasonably (small) value.  -- jcn
%
\header{ texidoc="@cindex Paper a4 Fill
This should fill a4 paper. "
}

#(set-global-staff-size 13)

\score{
	\context Voice \notes\relative c'{
		\clef alto
		\repeat "unfold" 36 c1
	}
	\paper{
		\paperThirteen
		indent = 0.0\mm
		% URG
		% Vertical space is rather precious when typesetting
		% music.  But we can only set textheight here, and must
		% guess and subtract the height needed for headers and 
		% footers.  If we want a header or footer on some page,
		% all other pages suffer shortened `textheight'.
		% Try the maximum for a4, without loosing footers:
		textheight = 297.0\mm - 7.0\mm
		papersize = "a4"
		linewidth = 15.0\mm
		\context {
			\Staff
			minimumVerticalExtent = #(cons 0 0)
		}
	}
}


% when tightly spaced, hinterfleisch -> 0 (and not: -> note-width)
% we need a mininum of about a note-width/interline space before
% bar line

% set rediculously tight
\score {
	\notes { \time 2/2; c'2 c'2 \time 2/2; }
	\paper { linewidth = 2.0 \cm;
	indent = 0.0;
	}
}


\version "1.3.120";

Time = \time 4/4;
Key = \notes { \key es \major; }
End = { \skip 1*314; \bar "|."; }

global = \notes {
	\Time
	\Key
	\End
}

#(set! point-and-click #t)
#(define text-flat '((font-relative-size . -2) (music "accidentals--1")))



\version "1.3.120";

Time = \time 4/4;
Key = \notes { \key es \major; }
End = { \skip 1*314; \bar "|."; }

global = \notes {
	\Time
	\Key
	\End
}

staffCombinePianoStaffProperties = {
	\property PianoStaff.devNullThread = #'()
	\property PianoStaff.soloADue = ##t
	\property PianoStaff.soloText = #""
	\property PianoStaff.soloIIText = #""
	% This is non-conventional, but currently it is
	% the only way to tell the difference.
	\property PianoStaff.aDueText = #"\\`a2"
	\property PianoStaff.splitInterval = #'(1 . 0)
	\property PianoStaff.changeMoment = #`(,(make-moment 1 1) . ,(make-moment 1 1))
}

#(set! point-and-click #t)
#(define text-flat '((font-relative-size . -2) (music "accidentals--1")))


\version "1.3.146"
% JUNKME.

%% deprecated
papersizename = \papersize 

% DO NOT change this without fixing -f ps output
papersize = \papersize

% FIXME
% direct PostScript line height for single line staves
lineheight = 14

paperfile = \papersize + "-init.ly"

% paperfile = "a4-init.ly"

\include \paperfile
\include "paper-init.ly"

staffspace = \staffheight / 4.0
stafflinethickness = \staffspace / 10.0
outputscale = \staffheight / 4.0

blotdiameter = 0.4 \pt


\translator { \NoteNamesContext }
\translator { \ScoreContext }
\translator { \ChoirStaffContext}
\translator { \InnerChoirStaffContext}

\translator { \RhythmicStaffContext}
\translator { \StaffContext }
\translator { \VoiceContext}
\translator { \StaffGroupContext }
\translator { \InnerStaffGroupContext }
\translator { \ChordNamesContext }
\translator { \GrandStaffContext}
\translator { \LyricsContext }
\translator { \ThreadContext}
\translator { \PianoStaffContext}
\translator { \LyricsVoiceContext }
\translator { \StaffContainerContext }
\translator { \FiguredBassContext }





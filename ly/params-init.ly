\version "1.3.146"
% JUNKME.

%% deprecated
papersizename = \papersize 

% ly2dvi now uses `papersize' internally (and on cmd line)
papersize = \papersize

% FIXME
% direct PostScript line height for single line staffs
lineheight = 14

paperfile = \papersize + "-init.ly"

% paperfile = "a4-init.ly"

\include \paperfile
\include "paper-init.ly"

staffspace = \staffheight / 4.0
stafflinethickness = \staffspace / 10.0
outputscale = \staffheight / 4.0

blotdiameter = 0.4 / \outputscale

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






\version "1.3.146"
% JUNKME.

papersizename = \papersize 

paperfile = \papersize + ".ly"

% paperfile = "a4.ly"

\include \paperfile
\include "paper.ly"

staffspace = \staffheight / 4.0
stafflinethickness = \staffspace / 10.0
outputscale = \staffheight / 4.0

\translator { \NoteNamesContext }
\translator { \ScoreContext }
\translator { \ChoirStaffContext}
\translator { \InnerChoirStaffContext}
\translator { \GraceContext}
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






% JUNKME.

papersizename = \papersize ;

paperfile = \papersize + ".ly";
% paperfile = "a4.ly";
\include \paperfile;
\include "paper.ly";

staffspace = \staffheight / 4.0;
stafflinethickness = \staffspace / 10.0;
outputscale = \staffheight / 4.0;

\translator { \NoteNamesContext }
\translator { \ScoreContext }
\translator { \ChoirStaffContext}
\translator { \GraceContext}
\translator { \RhythmicStaffContext}
\translator { \StaffContext }
\translator { \VoiceContext}
\translator { \StaffGroupContext }
\translator { \ChordNamesContext }
\translator { \ChordNamesVoiceContext}
\translator { \GrandStaffContext}
\translator { \LyricsContext }
\translator { \ThreadContext}
\translator { \PianoStaffContext}
\translator { \LyricsVoiceContext }







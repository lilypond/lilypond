\version "1.3.146"
% params-as-init.ly
% generic paper parameters

outputscale = \staffheight / 4.0

linewidth = 60.0 \char
textheight = 60.0 \char
indent = 8.0\char

staffspace = (\staffheight - 1.0 ) / 4.0
stafflinethickness = \staffspace / 2.0

\translator { \NoteNamesContext }
\translator { \ScoreContext }
\translator { \ChoirStaffContext }
\translator { \GraceContext }
\translator { \RhythmicStaffContext}
\translator { \StaffContext }
\translator { \VoiceContext }
\translator { \StaffGroupContext }
\translator { \ChordNamesContext }
\translator { \GrandStaffContext }
\translator { \LyricsContext }
\translator { \ThreadContext }
\translator { \PianoStaffContext }
\translator { \LyricsVoiceContext }
\translator { \StaffContainerContext }



% params-as.ly
% generic paper parameters

%%paperfile = \papersize + ".ly";
%%% paperfile = "a4.ly";
%%\include \paperfile;
%hsize = 60.0\char;
%vsize = 60.0\char;  %?

%%\include "paper.ly";
linewidth = 60.0\char;
textheight = 60.0\char;
indent = 8.0\char;

staffspace = (\staffheight - 1.0 ) / 4.0;
stafflinethickness = \staffspace / 2.0;

% paperfile = "a4.ly";
%\include \paperfile;
%\include "paper.ly";

%staffspace = \staffheight / 4.0;
%stafflinethickness = \staffspace / 10.0;

outputscale = \staffheight / 4.0;

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



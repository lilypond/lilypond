\version "1.5.68"
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

unit = "mm"
staffspace = \staffheight / 4.0
linethickness = \staffspace / 10.0
outputscale = \staffheight / 4.0
ledgerlinethickness = 2.0 * \linethickness



% blotdiameter = 0.4 \pt
blotdiameter = 0.04 \pt
interscoreline = 4. \mm


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
\translator { \TabStaffContext }
\translator { \TabVoiceContext }




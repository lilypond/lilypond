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
staffspace = #(/ staffheight 4.0)
linethickness = #(/ staffspace  10.0)
outputscale =  #(/ staffheight 4.0)
ledgerlinethickness = #(* 2.0 linethickness)



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



#(define font-defaults
      '((font-family . music)
	(font-relative-size . 0)
	(font-shape . upright)
	(font-series . medium)
	))

#(define style-alist
      '((finger . ((font-family . number) (font-relative-size . -3)))
	(volta . ((font-family . number) (font-relative-size . -2)))
	(tuplet . ((font-family . roman) (font-shape . italic) (font-relative-size . -1)))

	(timesig . ((font-family . number) ))
	(timesig-symbol . ((font-family . music) ))
	
	(mmrest . ((font-family . number) ))
	(mmrest-symbol . ((font-family . music) ))

	(mark-number . ((font-family . number) (font-relative-size . 1)))
	(mark-letter . ((font-family . roman)
			(font-series . bold)
			(font-shape . upright)
			(font-relative-size . 2)))
	
	(script . ((font-family . roman) (font-relative-size . -1)))
	(large . ((font-family . roman) (font-relative-size . 1)))
	(Large . ((font-series . bold) (font-family . roman)
		  (font-relative-size . 2)))
	(dynamic . ((font-family . dynamic) (font-relative-size . 0)))
	))
#(define properties-to-font Font_interface::properties_to_font_name)
#(define markup-to-properties markup-to-properties)
#(define abbreviation-alist
      '((columns . ((axis . 0)))
	(lines . ((axis . 1)))
	(roman . ((font-family . roman)))
	(music . ((font-family . music) (lookup . name)))
	(finger . ((font-style . finger)))
	(bold . ((font-series . bold)))
	(upright . ((font-shape . upright)))
	(italic . ((font-shape . italic)))
	(named . ((lookup . name)))
	(overstrike . ((extent . (0 . 0))))
	(super . ((raise . 1) (font-relative-size . -1) (extent . (0 . 0))))
	(sub . ((raise . -1) (font-relative-size . -1) (extent . (0 . 0))))
	(text . ((lookup . value)))
	)
     )

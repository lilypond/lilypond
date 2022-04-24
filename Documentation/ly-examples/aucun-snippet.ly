\version "2.19.22"
\include "example-header.ily"

\paper {
%  line-width=158\mm  % produces 624 pixels
  line-width = 143\mm  % reserve 15mm for incipit
}

% Aucun ont trouvé, from Montpellier Codex
%
% Put brackets in where the original has ligatures, using
% "Analysis Brackets," and adjust the "bracket-flare" property
% to make the ends vertical instead of slanted.

#(set-global-staff-size 15)

%%%%%%%%%%% INCIPIT DEFS %%%%%%%%%%

incipitGlobal = {
		\override NoteHead.style = #'mensural
		\override Accidental.style = #'mensural
		\override KeySignature.style = #'mensural
		\override Rest.style = #'mensural
%		\override Staff.TimeSignature.style = #'mensural
		\cadenzaOn
	  \override Score.Clef.extra-offset = #'(-0.0 . 0.5)
	  \override Score.Clef.font-size = #3
	  \clef "vaticana-do1"
}

incipitTriplum = \markup{
	\score{
		{
		\set Staff.instrumentName="Triplum "
		\incipitGlobal
		s1.
    }
	  \layout {
		%indent = 1\cm
		  \context {\Voice
			  \remove Ligature_bracket_engraver
			  \consists Mensural_ligature_engraver
		  }
		  \context {\Staff
		    \remove Time_signature_engraver
		  }
		 line-width=5\mm
	  }
	}
}

incipitMotetus = \markup{
	\score{
		{
		\set Staff.instrumentName="Motetus"
		\incipitGlobal
		s1.
		}
	  \layout {
		%	indent = 1\cm
		  \context {\Voice
			  \remove Ligature_bracket_engraver
			  \consists Mensural_ligature_engraver
		  }
		  \context {\Staff
		    \remove Time_signature_engraver
		  }
		 line-width=5\mm
	  }
	}
}

incipitTenor = \markup{
    \score{
		{
    \set Staff.instrumentName = "Tenor  "
		\incipitGlobal
		s1.*2
    }
    \layout {
		%indent = 1\cm
		\context {\Voice
			\remove Ligature_bracket_engraver
			\consists Mensural_ligature_engraver
		}
		\context {\Staff
		  \remove Time_signature_engraver
		}
		 line-width=5\mm
}
}
}

incipitBassus = \markup{
    \score{ {
    \set Staff.instrumentName = "Bassus  "
    \override NoteHead.style = #'neomensural
		\override Accidental.style = #'neomensural
	\override Rest.style = #'neomensural
	\override Staff.TimeSignature.style = #'neomensural
	\cadenzaOn
	\clef "petrucci-f3"
	\key f \major
	\time 3/2
  \relative c' {
    s1. % R1.*2
  }
    }
    \layout {
		  \context { \Voice
			  \remove Ligature_bracket_engraver
			  \consists Mensural_ligature_engraver
		  }
		 line-width=5\mm
    }
  }
}

%%%%%%%%%%% END INCIPIT DEFS %%%%%%%%%%%

#(ly:set-option 'point-and-click #f)

global = {
  \override Staff.TimeSignature.stencil = #(lambda (grob)
	(bracketify-stencil (ly:time-signature::print grob) Y 0.1 0.2 0.1))
  \time 3/4
	\hide Staff.BarLine
  \override HorizontalBracket.direction = #UP
  \override HorizontalBracket.bracket-flare = #'(0 . 0)
}

%%%%%%%%% MACRO FOR MAKING SLASHES THROUGH STEMS %%%%%%%%%%
MakeSlash = #(define-music-function (angle len-left len-right
thick y-factor offset)
                                    (number? number? number? number? number?
pair?)
#{
\once \override Voice.Stem.text = \markup {
    \postscript #(let ((x-off (car offset))
                       (y-off (cdr offset)))
    (string-append
    (ly:number->string (car offset)) " " (ly:number->string (cdr offset)) "
translate "
    (ly:number->string angle) " rotate "
    (ly:number->string (- x-off)) " "
    (ly:number->string (- y-off)) " translate 0 setlinewidth "
    (ly:number->string (- x-off len-left))  " " (ly:number->string (+ y-off
thick)) " moveto "
    (ly:number->string (- x-off len-left))  " " (ly:number->string y-off)
                                             " " (ly:number->string thick) "
90 270 arc "
    (ly:number->string (+ x-off len-right)) " " (ly:number->string y-off)
                                             " " (ly:number->string thick) "
270 90 arc "
                                             " gsave fill grestore stroke")) }

\once \override Voice.Stem.stencil = #(lambda (grob)
    (let* ((sten1 (ly:stem::print grob))
           (sten2 (ly:text-interface::print grob))
           (extent1 (ly:stencil-extent sten1 Y))
           (extent2 (ly:stencil-extent sten2 Y)))
    (ly:stencil-add
        sten1
        (ly:stencil-translate sten2
                              (cons 0 (+ (* y-factor (cdr extent1))
                                         (* (- 1 y-factor) (car extent1))))))))
#})


slash = { \MakeSlash #20 #1.0 #1.1 #0.05 #0.75 #'(0 . -.5) }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

triplumWords = \lyricmode {
  Au -- cun ont trou -- ve chant par u -- sa -- ge,
	mes a moi en doune o -- choi -- son __
  a -- mours, qui res -- bou -- dist mon cou -- ra -- ge
	si que m'ès -- tuet fai -- re _ chan -- _ _ çon
}

triplumNotes = \relative {
  \clef "treble_8"
	%\set Staff.midiInstrument = "flute"
%	\global
	\override StemTremolo.beam-thickness = #.125
	\override StemTremolo.slope = #1.0
  f'8 f4 e8 d c f f f | % 1
	% the \scaleDurations command below makes 5 notes last the
	% duration of a dotted quarter
	e8 c4 \scaleDurations 3/2 {	\tuplet 5/4{e16[ d e d e]} } e8 f4 | % 2
  g2. ~ 4. | % 3
	f8 d4 f4. \scaleDurations 3/2 { \tuplet 6/4{ g16[ f e f e f]}} % 4
	g8 g4 g4. e4. | % 5
	fis8 d4 e8\startGroup g4\stopGroup f8[ e d] | % 6
	c2. r4. | % 7
}

motetusWords = \lyricmode {
  lonc tans _ _  me fiu -- te -- nu de chan -- _ _ ter __
  mes or ai _ _
}

motetusNotes = \relative {
  \clef "treble_8"
  c'2. c8\startGroup b8 \slash c\stopGroup | % 1
	a2. c4. | d2. e4. | % 2-3
  f2. f8 e d | % 4
	c2. ~ 4. | r2. r4. | % 5-6
	g'4. g g8 f e | % 7
}


tenorNotes = \relative {
  \clef "treble_8"
	f2. | a2. | g2. | r2. | % 1-4
	c2. | b2. | c2. |  % 5-7
}

\score {
  \new StaffGroup <<
	  \new Staff = "triplum" <<
		  %\set Staff.instrumentName = "Triplum"
		  \set Staff.instrumentName = \incipitTriplum
			\set Staff.shortInstrumentName = "Tr."
      \set Staff.timeSignatureFraction = 9/8
      \scaleDurations 2/3
		  \context Voice = "triplum" { \global \triplumNotes }
%      \scaleDurations 2/3
%			\context Voice = "slashes" { \triplumSkips }
	    \new Lyrics { \lyricsto "triplum" { \triplumWords }}
		>>
		\new Staff = "motetus" <<
		  %\set Staff.instrumentName = "Motetus"
		  \set Staff.instrumentName = \incipitMotetus
			\set Staff.shortInstrumentName = "M."
      \set Staff.timeSignatureFraction = 9/8
      \scaleDurations 2/3
			\context Voice = "motetus" { \global \motetusNotes }
	    \new Lyrics { \lyricsto "motetus" { \motetusWords }}
	  >>
		\new Staff = "tenor" {
		  %\set Staff.instrumentName = "Tenor"
		  \set Staff.instrumentName = \incipitTenor
			\set Staff.shortInstrumentName = "T."
			\global \tenorNotes
		}
	>>
	%\midi {}
	\layout {
	  %\context {
	   % \Staff \consists Horizontal_bracket_engraver
	  %}
		\context {
		  \Voice \consists Horizontal_bracket_engraver
	  }
	}
}

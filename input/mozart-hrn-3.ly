\header{
  title =	 "Konzert Nr. 3 Es dur"
  subtitle = "f\\\"ur Horn und Orchester"
  composer =	 "Wolfgang Amadeus Mozart (1756-1792)"
  enteredby =	 "HWN"
  opus = "K.V. 447"

  copyright = "public domain"
  instrument = "Horn in F"
  editor = "Henri Kling"
  mutopiatitle = "Horn Concerto 3"
  mutopiacomposer = "W.A.Mozart"
  mutopiaopus = "KV447"
  style = "classical"
  maintainer = "hanwen@cs.uu.nl"
  maintainerEmail = "hanwen@cs.uu.nl"
  maintainerWeb = "http://www.cs.uu.nl/~hanwen/"	
  lastupdated = "2002/March/26"
  source = "Edition Breitkopf 2563"

  tagline =  "\\parbox{\hsize}{\\thefooter\\quad\\small This music is part of the Mutopia project, \\texttt{http://sca.uwaterloo.ca/Mutopia/}. It has been typeset and placed in the public domain by " + \maintainer + ". Unrestricted modification and redistribution is permitted and encouraged---copy this music and share it!}"

}
%{

This is the Mozart 3 for horn.  It's from an Edition Breitkopf EB
2563, edited by Henri Kling. Henri Kling (1842-1918) was a horn
virtuoso that taught in Geneva. 

%}


#(set! point-and-click line-column-location)

longgrace = \property Voice.Stem \override #'flag-style = #'()
endlonggrace = \property Voice.Stem \revert #'flag-style
ritenuto = \textscript #'(italic  "rit.")

\version "1.5.47"

#(define italic-bf '((font-shape . italic) (font-series . bold)))

%% burp
%% the very idea of a style sheet, is that it's easy to override
#(define (set-style! sheet name style)
  (set-cdr! (assoc 'mark-letter (cdr (assoc 'style-alist sheet))) style))

#(define my-sheet (make-style-sheet 'paper20))
#(set-style! my-sheet 'mark-letter '((font-family . roman)
				     (font-series . bold)
				     (font-shape . upright)
				     (font-relative-size . 3)))
  
cresc = \notes {
    \commandspanrequest \start "crescendo" 
    \property Voice.crescendoText = #`(,italic-bf "cresc.")
    \property Voice.crescendoSpanner = #'dashed-line
}

\paper{
    \stylesheet #my-sheet
    \translator {
	\ScoreContext
    	skipBars = ##t
	midiInstrument = #"french horn"
	%% try to mimic Breitkopf
	RehearsalMark \override #'padding = #1
	MultiMeasureRest \override #'padding = #0.5
	MultiMeasureRest \override #'number-threshold = #1
	
	Beam \override #'thickness = #0.6
	Beam \override #'beam-space = #0.8
	Slur \override #'beautiful = #0.3
    }
    \translator {
	\StaffContext
	MinimumVerticalExtent = #'(-4.5 . 4.5)
    }
    indent = 10. \mm
    linewidth = 189. \mm
}

\include "mozart-hrn3-allegro.ly"
\include "mozart-hrn3-romanze.ly"
\include "mozart-hrn3-rondo.ly"



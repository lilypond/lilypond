\version "1.3.117";

%{

Look at ly/engraver.ly for inspiration on which XXXX
there are.

Generally, you can find interesting element properties associated with
\property in scm/generic-property.scm. For example, this file contains

	(define generic-stem-properties
	  (cons 'stem-interface
		(list
		 (list 'stemVerticalDirection dir? 'direction)
		 (list 'verticalDirection dir? 'direction)	 
		 (list 'stemLength number? 'length)
		 (list 'flagStyle string? 'flag-style)
	)))


which means that setting \property stemVerticalDirection overrides
setting \property verticalDirection, and that both have the effect of
setting `direction' in Stem object. You can use \overrideproperty for
setting stem directions by doing.


	\overrideproperty #'(Stem) #'direction #1

(#-1 if you want down).  

A modest amount of memory is involved each time you do a
\overrideproperty. If you do \revertproperty in the right order (reversed
from \overrideproperty), then \revertproperty doesn't cost memory.

Correct:

	\overrideproperty #'(  ... ) #'symbolA #valueA
	\overrideproperty #'(  ... ) #'symbolB #valueB
	\revertproperty #'(  ... ) #'symbolB 
	\revertproperty #'(  ... ) #'symbolA 

Incorrect (\revertproperty costs memory):

	\overrideproperty #'(  ... ) #'symbolA #valueA
	\overrideproperty #'(  ... ) #'symbolB #valueB
	\revertproperty #'(  ... ) #'symbolA 
	\revertproperty #'(  ... ) #'symbolB 

You can use identifiers, eg.

    slursUp = \context Voice \overrideproperty '(Slur)
	    #'direction  #1
    slursBoth = \context Voice \revertproperty '(Slur)

%}

\score { \notes
\relative c' {
	c4-.(
	\property Voice.Dots \override #'direction =  #-1
	\property Voice.Stem \override #'direction =  #-1
	\property Voice.noteColumnProperties \override #'direction =  #-1
	\property Voice.Stem \override #'direction =  #-1		
	
	) c4-. (
	) c4-. (	
	 \property Voice.Slur \override #'direction =  #-1
	) c4-. (

	\property Dots \revert  #'direction
	\property Stem \revert #'direction
	\property Script \revert #'direction
	\property Text \revert #'direction

	 ) c4-.  () c4-. 
}

\paper {
\translator { \VoiceContext
	NoteHead \override #'font-relative-size =  #-2
}
}
}

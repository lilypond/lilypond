\version "2.3.4"
% TODO: check to see if this example can be simplified using \small
% or \tiny.
\header { texidoc = "@cindex Cue Notes
Cue notes are typeset in a smaller font. "
%  Cue clefs are usually not restored explicitly. "
}


\score {
  
   {
       \set Staff.instrument = #"Horn in F"
       \set Score.skipBars = ##t
       R1*21
    <<
	{
	    \once \override Staff.MultiMeasureRest  #'staff-position = #-6
	    R1
	}
     \new Voice { s2
       \clef tenor

	%% this should probably be put into an identifier.
       \set Staff.fontSize = #-1
	\override Stem  #'length = #5.5
	\override Beam  #'thickness = #0.384
	\override Beam  #'space-function =
	   #(lambda (beam mult) (* 0.8 (Beam::space_function beam mult)))

	r8^"Bsn." c'8  f'8[ f'8]

% note: the clef should be cancelled from the cue.  
%  this is under debate; I don't think it should.  - Graham

% if you want to cancel it in the main part, uncomment the following:
%       \set Staff.Clef = \turnOff
			    
       \unset Staff.fontSize 

       \clef treble
     } >>
   c'8^"Horn" cis'
   \unset Staff.Clef 
   eis'4 fis'4
 }
 \paper { raggedright = ##t}
}

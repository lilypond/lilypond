\version "1.9.8"
% TODO: check to see if this example can be simplified using \small
% or \tiny.
\header { texidoc = "@cindex Cue Notes
Cue notes should be set in smaller type. "
%  Cue clefs are usually not restored explicitly. "
}


\score {
  \notes
   {
       \property Staff.instrument = #"Horn in F"
       \property Score.skipBars = ##t
       R1*21
    <<
	{
	    \property Staff.MultiMeasureRest \override #'staff-position = #-6
	    R1
	    \property Voice.MultiMeasureRest \revert #'staff-position
	}
     \new Voice { s2
       \clef tenor

	%% this should probably be put into an identifier.
       \property Staff.fontSize = #-1
	\property Voice.Stem \override #'length = #5.5
	\property Voice.Beam \override #'thickness = #0.384
	\property Voice.Beam \override #'space-function =
	   #(lambda (beam mult) (* 0.8 (Beam::space_function beam mult)))

	r8^"Bsn." c'8  f'8[ f'8]

% note: the clef should be cancelled from the cue.  
%  this is under debate; I don't think it should.  - Graham

% if you want to cancel it in the main part, uncomment the following:
%       \property Staff.Clef = \turnOff
			    
       \property Staff.fontSize\unset

       \clef treble
     } >>
   c'8^"Horn" cis'
   \property Staff.Clef \unset
   eis'4 fis'4
 }
 \paper { raggedright = ##t}
}

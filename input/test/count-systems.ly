
\version "2.2.0"
\header{
    
    texidoc="@cindex Count Systems
After a line break, some function may called by overriding 
@code{after-line-breaking-callback}. This can be most useful to assertain 
that a piece uses a specified number of lines; typically the number of
lines (or systems) is not engraved, but it can be printed to console when 
generating the output. The number of lines may be associated either
to the number of systems or the system number of a grob.
" }

%% -- jcn:
%% See, e.g., input/mutopia/J.S.Bach/baerenreiter-sarabande.ly for
%% an application:

%% We want this to perfectly match the Baerenreiter spacing.
%% If we're not using 6 systems, there's definately a problem.
%% #(define (assert-system-count smob n) ...

#(define (display-systemno smob)
  (let* ((this-system (ly:grob-system smob))
	 (systems (ly:spanner-broken-into
		   (ly:grob-original this-system))))
   (display smob)
   (display systems)
   (display this-system)
   
   (display (list-index systems this-system))
   (newline)))
  

#(define (display-system-count smob)
  (display (length
	    (ly:spanner-broken-into
	     (ly:grob-original
	      (ly:grob-system smob))))))

  
  
\score{
    \notes\relative c''{
	\override NoteHead  #'after-line-breaking-callback
	 = #display-system-count
%	= #display-systemno
	c1
	d
    }
    \paper{ indent = 0.0\mm
	    linewidth = 10.0\mm
		raggedright = ##t
	}
}


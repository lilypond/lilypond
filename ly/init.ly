% Toplevel initialisation file. 

#(define-public point-and-click #f)
#(define-public midi-debug #f)


\version "2.2.0"

\include "declarations-init.ly"


#(ly:set-option 'new-relative)
#(ly:set-point-and-click #f)

\maininput
% there is a problem at the end of the input file


%%
%% above and below message is to compensate for look ahead of the parser.
%%

#(if (and (ly:get-option 'old-relative)
      (not (ly:get-option 'old-relative-used)))
  (ly:warn (string-append
	    "\n"
	    input-file-name ": old relative compatibility was not used."
	)))% there is a problem at the end of the input file



#(if (ly:get-option 'verbose)
  (begin
   (gc)
   (write (gc-stats) (current-error-port))
   ))


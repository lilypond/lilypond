%% Toplevel initialisation file. 

#(define-public point-and-click #f)
#(define-public midi-debug  #f)


\version "2.3.22"

\include "declarations-init.ly"


#(ly:set-option 'new-relative)
#(ly:set-point-and-click #f)
#(define  toplevel-scores '())
#(define $globalheader #f)

\maininput
%% there is a problem at the end of the input file

%%
%% Above and below comments compensate for the parser's look-ahead.
%%

#(if (and (ly:get-option 'old-relative)
      (defined? 'input-file-name)
      (not (ly:get-option 'old-relative-used)))
  (ly:warn (string-append
	    "\n"
	    input-file-name ": old relative compatibility was not used."
	)))%% there is a problem at the end of the input file

#(if (pair? toplevel-scores)
  (ly:parser-print-book parser
   (apply ly:make-book $defaultpaper $globalheader (reverse toplevel-scores))))


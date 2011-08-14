\version "2.15.9"

#(ly:set-option 'warning-as-error #f)

\header{
  texidoc="
Test the different loglevels of lilypond. Run this file with --loglevel=NONE,
ERROR, WARNING, PROGRESS, DEBUG to see the different loglevels. The errors
are commented out. Comment them in to check the output manually.
"
}

%%%% message functions of the Input class:
#(display "\nMessage functions of the Input class:\n")

messageTest = #(define-music-function (parser location) ()
   (ly:input-message location "Test ly:input-message" )
   (make-music 'Music))

{
%   #(display "-) Testing message\n")
  \messageTest % message
%   #(display "-) Testing warning\n")
  c4( c( c) % warning
%   #(display "-) Testing error\n")
%   sr  % error
}

%%%% message functions in the warn.hh file:
#(display "Message functions in the warn.hh file:\n")

% #(display "-) Testing debug\n")
#(ly:debug "Test debug\n")
% #(display "-) Testing progress\n")
#(ly:progress "Test progress\n")
% #(display "-) Testing message\n")
#(ly:message "Test message\n")
% #(display "-) Testing warning\n")
#(ly:warning "Test warning\n")
% #(display "-) Testing error\n")
% #(ly:error "Test error\n")

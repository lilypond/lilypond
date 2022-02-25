\version "2.19.22"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "Test warning\n"))
#(ly:expect-warning (G_ "already have slur"))

\header{
  texidoc="
Test the different loglevels of lilypond. Run this file with --loglevel=NONE,
ERROR, WARNING, PROGRESS, DEBUG to see the different loglevels. The errors
are commented out. Comment them in to check the output manually.
"
}

%%%% message functions of the Input class:
#(display "\nMessage functions of the Input class:\n" (current-error-port))

messageTest = #(define-music-function () ()
   (ly:input-message (*location*) "Test ly:input-message" )
   (make-music 'Music))

{
%   #(display "-) Testing message\n" (current-error-port))
  \messageTest % message
%   #(display "-) Testing warning\n" (current-error-port))
  c4( c( c) % warning
%   #(display "-) Testing error\n" (current-error-port))
%   sr  % error
}

%%%% message functions in the warn.hh file:
#(display "Message functions in the warn.hh file:\n" (current-error-port))

% #(display "-) Testing debug\n" (current-error-port))
#(ly:debug "Test debug\n")
% #(display "-) Testing progress\n" (current-error-port))
#(ly:progress "Test progress\n")
% #(display "-) Testing message\n" (current-error-port))
#(ly:message "Test message\n")
% #(display "-) Testing warning\n" (current-error-port))
#(ly:warning "Test warning\n")
% #(display "-) Testing error\n" (current-error-port))
% #(ly:error "Test error\n" (current-error-port))

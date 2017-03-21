\version "2.16.0"

#(load-user-init)

% This loads the user's .guile file for interactive sessions.
% One typical thing you might want to put there is
% (use-modules (ice-9 readline))
% (activate-readline)
% in order to activate command line editing for interactive sessions.
% You need libreadline support and the respective Guile module to be
% installed for that.  In Debian, for example, this is part of the
% guile-1.8-libs package.  Depending on your system and version, the
% requirements may be different.

#(newline)
#(if (guile-v2)
     (begin
       (use-modules (system repl repl))
       (start-repl))
     (scm-style-repl))

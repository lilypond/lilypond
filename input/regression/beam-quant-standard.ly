\header {

    texidoc = "This file tests a few standard beam quants."
    
}

\version "2.3.6"

%
% todo: make the check-quant function throw an error for incorrect quants
%

\paper  {
    raggedright = ##t
    #(define debug-beam-quanting #t)

      }

filler = \relative { e4 e }
% 


primes = \relative {
    \assertBeamQuant #'(0 . 0)  #'(0 . 0)
    c8[ c]
    \filler
    \assertBeamQuant #'(1 . -1)  #'(1 . -1)
    d8[ d]
     
    \filler
     
    \assertBeamQuant #'(1 . 0)  #'(1 . 0)
    e8[ e]
    \filler
     
     
    \assertBeamQuant #'(2 . -1)  #'(2 . -1)
    f8[ f]
    \filler
     
     
    \assertBeamQuant #'(2 . 0)  #'(2 . 0)
    g8[ g]
    \filler
     
     
    \assertBeamQuant #'(2 . 1)  #'(2 . 1)
    a8[ a]
    \filler
      
    \once \override Beam #'inspect-quants = #'(2.2 . 2.2)
    a8[ a]
    \filler
}

seconds = \relative {
    \assertBeamQuant #'(0 . 1)  #'(0 . 1)
    c8[ d]
    \filler
     
    \assertBeamQuant #'(1 . -1)  #'(1 . 0)
    d8[ e]
    \filler
     
     
    \assertBeamQuant #'(1 . 0)  #'(1 . 1)
    e8[ f]
    \filler
     
     
    \assertBeamQuant #'(2 . -1)  #'(2 . 0)
    f8[ g]
    \filler
     
     
    \assertBeamQuant #'(2 . 0)  #'(2 . 1)
    g8[ a]
    \filler

    \assertBeamQuant #'(3 . -1)  #'(3 . 0)
    a8[ b]
    \filler
}


{ \primes \seconds }


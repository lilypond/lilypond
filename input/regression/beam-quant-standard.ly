\header {

    texidoc = "This file tests a few standard beam quants."
    
}

\version "2.3.6"

%
% todo: make the check-quant function throw an error for incorrect quants
%

\paper  { raggedright = ##t }

filler = \relative { e4 e }
% 
#(ly:set-option 'debug-beam #t)

assertquant =
#(def-music-function (location l r) (pair? pair?)
  (let* ((f (check-quant-callbacks l r)))
   
   #{
   \override Beam #'position-callbacks = $f
   #}
   
))




primes = \relative {
    \assertquant #'(0 . 0)  #'(0 . 0)
    c8[ c]
    \filler
    \assertquant #'(1 . -1)  #'(1 . -1)
    d8[ d]
     
    \filler
     
    \assertquant #'(1 . 0)  #'(1 . 0)
    e8[ e]
    \filler
     
     
    \assertquant #'(2 . -1)  #'(2 . -1)
    f8[ f]
    \filler
     
     
    \assertquant #'(2 . 0)  #'(2 . 0)
    g8[ g]
    \filler
     
     
    \assertquant #'(2 . 1)  #'(2 . 1)
    a8[ a]
    \filler
      
    \once \override Beam #'inspect-quants = #'(2.2 . 2.2)
    a8[ a]
    \filler
}

seconds = \relative {
    \assertquant #'(0 . 1)  #'(0 . 1)
    c8[ d]
    \filler
     
    \assertquant #'(1 . -1)  #'(1 . 0)
    d8[ e]
    \filler
     
     
    \assertquant #'(1 . 0)  #'(1 . 1)
    e8[ f]
    \filler
     
     
    \assertquant #'(2 . -1)  #'(2 . 0)
    f8[ g]
    \filler
     
     
    \assertquant #'(2 . 0)  #'(2 . 1)
    g8[ a]
    \filler

    \assertquant #'(3 . -1)  #'(3 . 0)
    a8[ b]
    \filler
}


{ \primes \seconds }

#(ly:set-option 'debug-beam #f)

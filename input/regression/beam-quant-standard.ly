\header {

    texidoc = "This file tests a few standard beam quants."
    
}


%
% todo: make the check-quant function throw an error for incorrect quants
%

\paper  { raggedright = ##t }


% 
% #(ly:set-option 'debug-beam #t)

assertquant =
#(def-music-function (location l r) (pair? pair?)
  (let* ((f (check-quant-callbacks l r)))
   
   #{
   \override Beam #'position-callbacks = $f
   #}
   
))
  

\relative {
    \assertquant #'(1 . 0)  #'(1 . 0)
    e8[ e]
    e4 
    e4 
    \assertquant #'(2 . -1)  #'(2 . -1)
    f8[ f]
    e4 
    e4 
    \assertquant #'(2 . 0)  #'(2 . 0)
    g8[ g]
    e4 
    e4 
    \assertquant #'(2 . 1)  #'(2 . 1)
    a8[ a]
    e4 e4 e4
    \once \override Beam #'inspect-quants = #'(2.2 . 2.2)
    a8[ a]
    e4 
    e4 

    \assertquant #'(0 . 1)  #'(1 . 0)
    d8[ e]
    e4 
    e4 
    \assertquant #'(1 . 0)  #'(1 . 1)
    e8[ f]
    e4 
    e4 
    \assertquant #'(2 . -1)  #'(2 . 0)
    f8[ g]
    e4 
    e4 
    \assertquant #'(2 . 0)  #'(2 . 1)
    g8[ a]
    e4 
    e4 
    }

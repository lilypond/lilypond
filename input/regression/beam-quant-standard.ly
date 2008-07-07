\header {

  texidoc = "This file tests a few standard beam quants, taken from
    Ted Ross' book. If LilyPond finds another quant, the correct quant
    is printed over the beam."
  
}

\version "2.11.51"

\layout  {
  ragged-right = ##t
  #(define debug-beam-quanting #t)
}

filler = \new Voice \relative  {
  \hideNotes
  e4 e
}
				% 

%%
%% Ross p108--112
primes = \relative {
  \time 3/4 
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
  
%{
  \once \override Beam #'inspect-quants = #'(2.2 . 2.2)
  \assertBeamQuant 
  a8[ a]
  \filler
  
%}
  
}


seconds = \relative {
  
  \assertBeamQuant #'(0 . 0)  #'(0 . 1)
  a8[ b]
  \filler
  

  \assertBeamQuant #'(0 . 0)  #'(0 . 1)
  b8[ c]
  \filler
  

  \assertBeamQuant #'(0 . 0)  #'(0 . 1)
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

filler = \new Voice \relative  {
  \hideNotes
  e4 e4.
}

				% Ross, p122 
primeSixteenths = \relative {
  \stemUp
  \assertBeamQuant #'(0 . -1)  #'(0 . -1)
  g16[ g]
  \filler
  \assertBeamQuant #'(0 . -1)  #'(0 . -1)
  a16[ a]
  \filler
  \assertBeamQuant #'(0 . -1)  #'(0 . -1)
  b16[ b]
  \filler
  \assertBeamQuant #'(0 . 0)  #'(0 . 0)
  c16[ c]
  \filler
  \assertBeamQuant #'(1 . -1)  #'(1 . -1)
  d16[ d]
  \filler
  \assertBeamQuant #'(1 . 0)  #'(1 . 0)
  e16[ e]
  \filler
  \assertBeamQuant #'(2 .  -1)  #'(2 . -1)
  f16[ f]
  \filler
  \assertBeamQuant #'(2 . 0)  #'(2 . 0)
  g16[ g]
  \filler
  \assertBeamQuant #'(3 . -1)  #'(3 . -1)
  a16[ a]
  \filler
  \assertBeamQuant #'(3 . 0)  #'(3 . 0)
  b16[ b]
  \filler
}

\new Voice { \primes \seconds \primeSixteenths }


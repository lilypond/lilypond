\version "2.25.25"

\header {
  texidoc = "This tests the internal function @code{ly:number->duration}.
Problems are reported on stderr."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% special values

#(expect-equal "-inf"
  (ly:number->duration -inf.0)
  (ly:make-duration 0 0 -inf.0))

#(expect-equal "-5/8"
  (ly:number->duration -5/8)
  (ly:make-duration 1 0 -5/4))

#(expect-equal "zero"
  (ly:number->duration 0)
  ZERO-DURATION)

#(expect-equal "+inf"
  (ly:number->duration +inf.0)
  (ly:make-duration 0 0 +inf.0))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sweep log values

#(expect-equal "ridiculously long note"
  (ly:number->duration 65536)
  (ly:make-duration -16 0))

#(expect-equal "double whole note"
  (ly:number->duration 2)
  (ly:make-duration -1 0))

#(expect-equal "whole note"
  (ly:number->duration 1)
  (ly:make-duration 0 0))

#(expect-equal "half note"
  (ly:number->duration 1/2)
  (ly:make-duration 1 0))

#(expect-equal "64th note"
  (ly:number->duration 1/64)
  (ly:make-duration 6 0))

%% TODO: It's surprising that the conversion is willing to create a duration
%% with so many dots, but isn't willing to set the log higher than 64th notes.
#(expect-equal "128th note"
  (ly:number->duration 1/128)
  (ly:make-duration 6 0 1/2))

#(expect-equal "ludicrously short note"
  (ly:number->duration 1/65536)
  (ly:make-duration 6 0 1/1024))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sweep dots

#(expect-equal "whole note; one dot"
  (ly:number->duration (ly:duration->number #{ 1. #} ))
  (ly:make-duration 0 1))

#(expect-equal "whole note; two dots"
  (ly:number->duration (ly:duration->number #{ 1.. #} ))
  (ly:make-duration 0 2))

#(expect-equal "whole note; three dots"
  (ly:number->duration (ly:duration->number #{ 1... #} ))
  (ly:make-duration 0 3))

#(expect-equal "whole note; four dots"
  (ly:number->duration (ly:duration->number #{ 1.... #} ))
  (ly:make-duration 0 4))

#(expect-equal "whole note; 20 dots"
  (ly:number->duration (ly:duration->number #{ 1.................... #} ))
  (ly:make-duration 0 20))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% other combinations

#(expect-equal "5/8"
  (ly:number->duration 5/8)
  (ly:make-duration 1 0 5/4))

#(expect-equal "3/10"
  (ly:number->duration 3/10)
  (ly:make-duration 2 0 6/5))

%% TODO: Try favoring scaling factors closer to 1 in cases like this.
#(expect-equal "2.*1000/1001"
  (ly:number->duration (ly:duration->number #{ 2.*1000/1001 #} ))
  (ly:make-duration 1 0 1500/1001))

#(expect-equal "2.*1001/1000"
  (ly:number->duration (ly:duration->number #{ 2.*1001/1000 #} ))
  (ly:make-duration 1 1 1001/1000))

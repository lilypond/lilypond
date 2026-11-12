\version "2.27.6"

\header {
  texidoc = "The pure part of an unpure-pure container takes the same
arguments as the unpure part.  The range of columns of the pure calculation
is available as @code{(*start*)} and @code{(*end*)}, and @code{(*pure?*)}
tells whether a pure calculation is going on.  All three are inactive
(@code{#f}) in unpure calculations.  Pure functions with the legacy signature
@code{(grob start end)} still work and get the same range.  A container
made from a single function evaluates it like an unpure one, also for pure
lookups, so it does not see a pure context; a function that is to behave in a
pure way has to be given as both parts.

In each of the groups of two eighth
notes with a pure function of its own (all but the first and the fourth),
the second note should move to accommodate the flag, as in the first group of
@file{unpure-pure-container.ly}.  Any warning is an error.
"
}

#(ly:set-option 'warning-as-error #t)

#(define (checked-unpure grob)
   (when (or (*pure?*) (*start*) (*end*))
     (ly:warning "pure context active in unpure calculation"))
   (ly:flag::calc-y-offset grob))

#(define (checked-pure grob)
   (unless (and (*pure?*)
                (integer? (*start*))
                (integer? (*end*))
                (<= (*start*) (*end*)))
     (ly:warning "no valid pure context in pure calculation"))
   8)

#(define (checked-single grob)
   (when (or (*pure?*) (*start*) (*end*))
     (ly:warning "single function evaluated in a pure context"))
   (ly:flag::calc-y-offset grob))

#(define (checked-both grob)
   (if (*pure?*)
       (begin
         (unless (and (integer? (*start*))
                      (integer? (*end*))
                      (<= (*start*) (*end*)))
           (ly:warning "no valid range in pure calculation"))
         8)
       (begin
         (when (or (*start*) (*end*))
           (ly:warning "range set in unpure calculation"))
         (ly:flag::calc-y-offset grob))))

#(define (legacy-pure grob start end)
   (unless (and (*pure?*)
                (eqv? start (*start*))
                (eqv? end (*end*)))
     (ly:warning "legacy pure function got wrong range"))
   8)

\relative {
  \stemUp \autoBeamOff
  d'8 eis'
  \once \override Flag.Y-offset =
    #(ly:make-unpure-pure-container checked-unpure checked-pure)
  d,8 eis'!
  \once \override Flag.Y-offset =
    #(ly:make-unpure-pure-container checked-unpure legacy-pure)
  d,8 eis'!
  \once \override Flag.Y-offset =
    #(fixed-callback checked-single)
  d,8 eis'!
  \once \override Flag.Y-offset =
    #(pure-callback checked-both)
  d,8 eis'!
}

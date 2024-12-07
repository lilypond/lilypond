\version "2.25.23"

\header {
  texidoc = "The deprecated context property
@code{maximumBeamSubdivisionInterval} accesses the value of
@code{beamMaximumSubdivision}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "maximumBeamSubdivisionInterval" "beamMaximumSubdivision")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting maximumBeamSubdivisionInterval sets beamMaximumSubdivision
  \set Score.maximumBeamSubdivisionInterval = #(ly:make-moment 22/7)
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'beamMaximumSubdivision) 22/7)
     (ly:error "fail")))

  %% getting maximumBeamSubdivisionInterval gets a moment made from
  %% beamMaximumSubdivision
  \set Score.beamMaximumSubdivision = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'maximumBeamSubdivisionInterval)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

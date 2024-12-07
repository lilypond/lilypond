\version "2.25.23"

\header {
  texidoc = "The deprecated context property
@code{minimumBeamSubdivisionInterval} accesses the value of
@code{beamMinimumSubdivision}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "minimumBeamSubdivisionInterval" "beamMinimumSubdivision")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting minimumBeamSubdivisionInterval sets beamMinimumSubdivision
  \set Score.minimumBeamSubdivisionInterval = #(ly:make-moment 22/7)
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'beamMinimumSubdivision) 22/7)
     (ly:error "fail")))

  %% getting minimumBeamSubdivisionInterval gets a moment made from
  %% beamMinimumSubdivision
  \set Score.beamMinimumSubdivision = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'minimumBeamSubdivisionInterval)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

\version "2.25.23"

\header {
  texidoc = "The deprecated context property @code{completionUnitAsMoment}
accesses the value of @code{completionUnit}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "completionUnitAsMoment" "completionUnit")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting completionUnitAsMoment sets completionUnit
  \set Score.completionUnitAsMoment = #(ly:make-moment 22/7)
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'completionUnit) 22/7)
     (ly:error "fail")))

  %% getting completionUnitAsMoment gets a moment made from completionUnit
  \set Score.completionUnit = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'completionUnitAsMoment)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}
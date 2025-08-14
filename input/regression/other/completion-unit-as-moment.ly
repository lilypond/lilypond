\version "2.25.28"

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
  \contextPropertyCheck Score.completionUnit #22/7

  %% getting completionUnitAsMoment gets a moment made from completionUnit
  \set Score.completionUnit = #5/12
  \contextPropertyCheck Score.completionUnitAsMoment #(ly:make-moment 5/12)

  s
}

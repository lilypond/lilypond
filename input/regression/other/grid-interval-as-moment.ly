\version "2.25.28"

\header {
  texidoc = "The deprecated context property @code{gridIntervalAsMoment}
accesses the value of @code{gridInterval}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "gridIntervalAsMoment" "gridInterval")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting gridIntervalAsMoment sets gridInterval
  \set Score.gridIntervalAsMoment = #(ly:make-moment 22/7)
  \contextPropertyCheck Score.gridInterval #22/7

  %% getting gridIntervalAsMoment gets a moment made from
  %% gridInterval
  \set Score.gridInterval = #5/12
  \contextPropertyCheck Score.gridIntervalAsMoment #(ly:make-moment 5/12)

  s
}

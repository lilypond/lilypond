\version "2.25.28"

\header {
  texidoc = "The deprecated context property
@code{measureLengthAsMoment} accesses the value of
@code{measureLength}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "measureLengthAsMoment" "measureLength")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting measureLengthAsMoment sets measureLength
  \set Score.measureLengthAsMoment = #(ly:make-moment 22/7)
  \contextPropertyCheck Score.measureLength #22/7

  %% getting measureLengthAsMoment gets a moment made from
  %% measureLength
  \set Score.measureLength = #5/12
  \contextPropertyCheck Score.measureLengthAsMoment #(ly:make-moment 5/12)

  s
}

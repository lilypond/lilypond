\version "2.25.28"

\header {
  texidoc = "The deprecated context property @code{baseMoment} accesses the
value of @code{beatBase}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "baseMoment" "beatBase")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting baseMoment sets beatBase
  \set Timing.baseMoment = #(ly:make-moment 22/7)
  \contextPropertyCheck Timing.beatBase #22/7

  %% getting baseMoment gets a moment made from beatBase
  \set Timing.beatBase = #5/12
  \contextPropertyCheck Timing.baseMoment #(ly:make-moment 5/12)

  s
}

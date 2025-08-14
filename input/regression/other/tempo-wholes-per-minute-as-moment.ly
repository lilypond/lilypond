\version "2.25.28"

\header {
  texidoc = "The deprecated context property @code{tempoWholesPerMinuteAsMoment}
accesses the value of @code{tempoWholesPerMinute}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "tempoWholesPerMinuteAsMoment" "tempoWholesPerMinute")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting tempoWholesPerMinuteAsMoment sets tempoWholesPerMinute
  \set Score.tempoWholesPerMinuteAsMoment = #(ly:make-moment 22/7)
  \contextPropertyCheck Score.tempoWholesPerMinute #22/7

  %% getting tempoWholesPerMinuteAsMoment gets a moment made from
  %% tempoWholesPerMinute
  \set Score.tempoWholesPerMinute = #5/12
  \contextPropertyCheck Score.tempoWholesPerMinuteAsMoment
  #(ly:make-moment 5/12)

  s
}

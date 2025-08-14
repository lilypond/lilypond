\version "2.25.28"

\header {
  texidoc = "The deprecated context property
@code{proportionalNotationDurationAsMoment} accesses the value of
@code{proportionalNotationDuration}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "proportionalNotationDurationAsMoment" "proportionalNotationDuration")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting proportionalNotationDurationAsMoment sets
  %% proportionalNotationDuration
  \set Score.proportionalNotationDurationAsMoment = #(ly:make-moment 22/7)
  \contextPropertyCheck Score.proportionalNotationDuration #22/7

  %% getting proportionalNotationDurationAsMoment gets a moment made from
  %% proportionalNotationDuration
  \set Score.proportionalNotationDuration = #5/12
  \contextPropertyCheck Score.proportionalNotationDurationAsMoment
  #(ly:make-moment 5/12)

  s
}

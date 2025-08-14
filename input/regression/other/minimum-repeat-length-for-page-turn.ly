\version "2.25.28"

\header {
  texidoc = "The deprecated context property
@code{minimumRepeatLengthForPageTurn} accesses the value of
@code{pageTurnMinimumRepeatLength}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "minimumRepeatLengthForPageTurn" "pageTurnMinimumRepeatLength")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting minimumRepeatLengthForPageTurn sets pageTurnMinimumRepeatLength
  \set Score.minimumRepeatLengthForPageTurn = #(ly:make-moment 22/7)
  \contextPropertyCheck Score.pageTurnMinimumRepeatLength #22/7

  %% getting minimumRepeatLengthForPageTurn gets a moment made from
  %% pageTurnMinimumRepeatLength
  \set Score.pageTurnMinimumRepeatLength = #5/12
  \contextPropertyCheck Score.minimumRepeatLengthForPageTurn
  #(ly:make-moment 5/12)

  s
}

\version "2.25.23"

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
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'pageTurnMinimumRepeatLength) 22/7)
     (ly:error "fail")))

  %% getting minimumRepeatLengthForPageTurn gets a moment made from
  %% pageTurnMinimumRepeatLength
  \set Score.pageTurnMinimumRepeatLength = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'minimumRepeatLengthForPageTurn)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

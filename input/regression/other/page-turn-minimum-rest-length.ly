\version "2.25.23"

\header {
  texidoc = "The deprecated context property @code{minimumPageTurnLength}
accesses the value of @code{pageTurnMinimumRestLength}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "minimumPageTurnLength" "pageTurnMinimumRestLength")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting minimumPageTurnLength sets pageTurnMinimumRestLength
  \set Score.minimumPageTurnLength = #(ly:make-moment 22/7)
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'pageTurnMinimumRestLength) 22/7)
     (ly:error "fail")))

  %% getting minimumPageTurnLength gets a moment made from
  %% pageTurnMinimumRestLength
  \set Score.pageTurnMinimumRestLength = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'minimumPageTurnLength)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

\version "2.25.22"

\header {
  texidoc = "The deprecated context property @code{baseMoment} accesses the
value of @code{beatBase}."
}

#(ly:set-option 'warning-as-error #t)
%% TODO: do expect this warning
%% #(ly:expect-warning
%%   (ly:translate-cpp-warning-scheme
%%    "the property '%s' is deprecated; use '%s'")
%%   "baseMoment" "beatBase")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting baseMoment sets beatBase
  \set Timing.baseMoment = #(ly:make-moment 22/7)
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'beatBase) 22/7)
     (ly:error "fail")))

  %% getting baseMoment gets a moment made from beatBase
  \set Timing.beatBase = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal? (ly:context-property ctx 'baseMoment) (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

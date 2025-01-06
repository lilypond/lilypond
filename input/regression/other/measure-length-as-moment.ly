\version "2.25.23"

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
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'measureLength) 22/7)
     (ly:error "fail")))

  %% getting measureLengthAsMoment gets a moment made from
  %% measureLength
  \set Score.measureLength = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'measureLengthAsMoment)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

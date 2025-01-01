\version "2.25.23"

\header {
  texidoc = "The deprecated context property
@code{tupletSpannerDurationAsMoment} accesses the value of
@code{tupletSpannerDuration}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' is deprecated; use '%s'")
  "tupletSpannerDurationAsMoment" "tupletSpannerDuration")

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting tupletSpannerDurationAsMoment sets tupletSpannerDuration
  \set Score.tupletSpannerDurationAsMoment = #(ly:make-moment 22/7)
  \applyContext
  #(lambda (ctx)
    (or
     (= (ly:context-property ctx 'tupletSpannerDuration) 22/7)
     (ly:error "fail")))

  %% getting tupletSpannerDurationAsMoment gets a moment made from
  %% tupletSpannerDuration
  \set Score.tupletSpannerDuration = #5/12
  \applyContext
  #(lambda (ctx)
    (or
     (equal?
      (ly:context-property ctx 'tupletSpannerDurationAsMoment)
      (ly:make-moment 5/12))
     (ly:error "fail")))

  s
}

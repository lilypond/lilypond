\version "2.25.28"

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
  \contextPropertyCheck Score.tupletSpannerDuration #22/7

  %% getting tupletSpannerDurationAsMoment gets a moment made from
  %% tupletSpannerDuration
  \set Score.tupletSpannerDuration = #5/12
  \contextPropertyCheck Score.tupletSpannerDurationAsMoment
  #(ly:make-moment 5/12)

  s
}

\version "2.25.28"

\header {
  texidoc = "The deprecated context property @code{voltaSpannerDurationAsMoment}
accesses the value of internal property @code{voltaBracketMusicalLength}."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (G_ "'voltaSpannerDurationAsMoment' is deprecated; \
use the grob property VoltaBracket.musical-length"))

%% It is unnecessary to test every available access method: there are other
%% tests covering them.  It is enough to cover this property's old->new and
%% new->old conversions somehow.
{
  %% setting voltaSpannerDurationAsMoment sets voltaBracketMusicalLength
  \set Score.voltaSpannerDurationAsMoment = #(ly:make-moment 22/7)
  \contextPropertyCheck Score.voltaBracketMusicalLength #(ly:make-moment 22/7)

  %% getting voltaSpannerDurationAsMoment gets a moment made from
  %% voltaBracketMusicalLength
  \set Score.voltaBracketMusicalLength = #(ly:make-moment 5/12)
  \contextPropertyCheck Score.voltaSpannerDurationAsMoment
  #(ly:make-moment 5/12)

  s
}

\version "2.13.5"

\header {
  lsrtags = "rhythms"
  texidoc = "
The @code{\\time} command sets the properties
@code{timeSignatureFraction}, @code{beatLength}, @code{beatGrouping}
and @code{measureLength} in the @code{Timing} context, which is
normally aliased to @code{Score}. Changing the value of
@code{timeSignatureFraction} causes the new time signature symbol to be
printed without changing any of the other properties:

"
  doctitle = "Changing the time signature without affecting the beaming"
}

\markup {
  This snippet is deprecated as of 2.13.5 and will be removed in 2.14
}


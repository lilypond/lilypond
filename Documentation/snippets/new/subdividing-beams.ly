\version "2.25.23"

\header {
  categories = "Rhythms"

  texidoc = "
The beams of consecutive 16th (or shorter) notes are, by default,
not subdivided.  That is, the beams of more than two stems stretch
over the entire group of notes without a break.  This behavior can
be modified to subdivide the beams into sub-groups by setting the
property @code{subdivideBeams} to @code{#t}.  When set, beams are
subdivided at (rhythmic) intervals to match the metric value of
the subdivision.

Using the properties @code{beamMinimumSubdivision} and
@code{beamMaximumSubdivision} it is possible to configure the
limits of automatic beam subdivision, namely the minimum and
maximum rhythmic lengths at which beamlets are removed.  The
default values are@tie{}@code{0} for the former and @code{+inf.0}
for the latter, making LilyPond subdivide beams as much as
possible.

There are two special cases to consider.

@itemize
@item
If the numerator of @code{beamMaximumSubdivision} is not a power
of@tie{}2, the rhythmic lengths considered for subdivision are
@code{beamMaximumSubdivision} divided by powers of@tie{}2 that
stay greater than or equal to @code{beamMinimumSubdivision}.

@item
If @code{beamMaximumSubdivision} is smaller than
@code{beamMinimumSubdivision}, the depth of beam subdivisions is
limited by @code{beamMaximumSubdivision}, but not the frequency
and rhythmic intervals, therefore possibly deviating from the
correct, expected metric value.
@end itemize

If @code{respectIncompleteBeams} is set to @code{#t}, incomplete
subdivisions with more than two stems are treated as an
@q{extension} of the previous subdivision group, i.e., the length
of the previous subdivision group gets extended to also cover the
incomplete subdivision.  If set to @code{#f} (which is the
default), a new subdivision group gets started instead.
"

  doctitle = "Subdividing beams"
}


\relative c'' {
  \time 1/4

  <>^"default"
  c32 c c c c c c c

  <>^"with subdivision"
  \set subdivideBeams = ##t
  c32 c c c c c c c

  <>^"min 1/8"
  \once \set beamMinimumSubdivision = #1/8
  c32 c c c c c c c

  <>^"max 1/16"
  \once \set beamMaximumSubdivision = #1/16
  c32 c c c c c c c

  <>^"max 3/8"
  \once \set beamMaximumSubdivision = #3/8
  \repeat unfold 16 c64

  <>^"min 1/32, max 1/64"
  % Set maximum beam subdivision interval to 1/64 to limit
  % subdivision depth, despite not being metrically correct.
  \once \set beamMinimumSubdivision = #1/32
  \once \set beamMaximumSubdivision = #1/64
  \repeat unfold 32 c128
  \break

  <>^"beams with incomplete subdivisions"
  c32 c c c c c c r32
  c32 c c c c r16.

  <>^\markup { "the same with"
               \typewriter { "respectIncomplete=#t" } }
  \set respectIncompleteBeams = ##t
  % The incomplete subgroup extends the completed subgroup.
  c32 c c c c c c r32
  % No visual change since we have only two stems in the
  % incomplete subgroup.
  c32 c c c c r16.
}

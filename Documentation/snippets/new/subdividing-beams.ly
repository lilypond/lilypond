\version "2.25.8"

\header {
  lsrtags = "rhythms"

  texidoc = "
The beams of consecutive 16th (or shorter) notes are, by default, not
subdivided.  That is, the beams of more than two stems stretch unbroken
over entire groups of notes.  This behavior can be modified to subdivide
the beams into sub-groups by setting the property @code{subdivideBeams}
to true (@code{#t}).  When set, a number of beamlets between two
consecutive stems are removed at intervals multiple beams will be
subdivided at intervals to match the metric value of the subdivision.
Properties @code{minimumBeamSubdivisionInterval} and
@code{maximumBeamSubdivisionInterval} allow configuring limits of
automatic beam subdivision: the minimum rhythmic interval at which to
subdivide beams and the number of beamlets removed depending on the
interval respectively.  If the numerator of
@code{maximumBeamSubdivisionInterval} is not a power of@tie{}2, the
smaller rhythmic intervals considered for subdivision are
@code{maximumBeamSubdivisionInterval} divided by powers of@tie{}2 and
stay greater than or equal to @code{minimumBeamSubdivisionInterval}.  If
@code{respectIncompletBeams} is set to true (@code{##t}), the depth of
the subdivision (number of beams) reflects the longest possible
subdivision interval within the remaining length of the beam from the
current stem.  However, the last two stems of the beam are exempt from
this rule.
"

  doctitle = "Subdividing beams"
}


\relative c'' {
  c32[ c c c c c c c]

  \set subdivideBeams = ##t
  c32[ c c c c c c c]

  % Set minimum beam subdivision interval to 1/8 just for this beam
  \once \set minimumBeamSubdivisionInterval = \musicLength 8
  c32[ c c c c c c c]

  % Set maximum beam subdivision interval to 1/16 just for this beam
  \once \set maximumBeamSubdivisionInterval = \musicLength 16
  c32[ c c c c c c c]

  % Set maximum beam subdivision interval to 3/8 just for this beam
  \once \set maximumBeamSubdivisionInterval = \musicLength 8*3
  c64[ c c c c c c c c c c c c c c c]

  r2.

  % Shorten beam by 1/32
  c32[ c c c c c c] r32

  % Shorten beam by 3/32
  c32[ c c c c] r16.

  % Respect the incomplete beams of the previous two examples
  \set respectIncompleteBeams = ##t
  c32[ c c c c c c] r32
  % no visual change here as last two stems are exempt from this special rule
  c32[ c c c c] r16.
}


\header{
texidoc="
When tieing notes with accidentals across a bar boundary, the
accidental must not be drawn on the note in the next bar.  Unless the
tie crosses a line break, in which case the accidental is repeated if
it would be different from an untied note.  The next note of the same
pitch in this next bar should always show the accidental (even if it's
natural).  Slurring a accidentaled note to a natural one across bar
boundaries should be explicit.

Pitches can be verified by printing them  with the @code{NoteNames} context.
";
}

thenotes = \notes \relative cis' {
  \time 4/4;
  g'2 g ~ |
  g g4 gis |
  gis2 gis ~ |
  gis4 gis8 ~ gis g4 gis |
  g2 gis ~ |
  gis g4 gis |
  g2 gis( |
  )g! gis4 gis |
  \break
  \key a \major;
  gis2 gis ~ |
  gis4 gis8 ~ gis g4 gis |
  gis2 g ~ |
  g4 gis8 ~ gis g4 gis |
  g2 gis ~ |
  gis g4 gis |
  g2 gis( |
  
  % FIXME: check for accidentals at line break could look more cute
  % maybe move to tie-break-accidental?
  
  % Btw: I don't even know what the rule is for the second note in the
  % next bar, if a reminder accidental was printed.  These are really
  % not very common cases.  Technically, if it is not tied, it
  % should get an accidental; but it looks a bit silly and redundant.
  % See last g.
  
  )g! gis4 gis ~ | \break
  gis2  gis ~ | gis g ~\break
  g2 g
}

\score {
  <
    \context Staff \thenotes
    \context NoteNames \thenotes
  >
}

\version "2.3.4"
\header { texidoc = "The property @code{chordNameExceptions} can used
    to store a list of special notations for specific chords.

Putting the exceptions list encoded as
@example
	\ @{ <c f g bes>1\\markup @{ \\super \"7\" \"wahh\" @} @}
@end example
into @code{chordNameExceptions} takes a little manoeuvring. The
following code transforms @code{chExceptionMusic} (which is a
sequential music) into a list of exceptions.
@example
	(sequential-music-to-chord-exceptions chExceptionMusic \#t)
@end example
Then,
@example
	(append
	  ... ignatzekExceptions)
@end example
adds the new exceptions to the default ones, which are defined in
@file{ly/chord-modifier-init.ly}.
" }


% 7sus4 denoted with ^7 wahh
chExceptionMusic =  {
  <c f g bes>1-\markup { \super "7" "wahh" }}

% add to existing exceptions.
chExceptions = #(append
  (sequential-music-to-chord-exceptions chExceptionMusic #t)
  ignatzekExceptions)

theMusic = \chords {
      c:7sus4 c:dim7/+f
      \set chordNameExceptions = #chExceptions
      c:7sus4 c:dim7/+f }

  
\score {
  << \context ChordNames \theMusic
    \context Voice \theMusic
  >>  
  }

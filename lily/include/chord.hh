/*
  chord.hh -- declare Chord

  source file of the GNU LilyPond music typesetter

  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_HH
#define CHORD_HH

#include "pitch.hh"

/*
  This is not an Item, just a collection of Chord manipulation helper
  functions
  
  ``chord'' is encoded:
  (PITCHES . (INVERSION . BASS))

  Chord:: namespace...  */
class Chord
{
public:
  static SCM base_pitches (SCM tonic);
  static SCM transpose_pitches (SCM tonic, SCM pitches);
  static SCM lower_step (SCM tonic, SCM pitches, SCM step);
  static SCM member_notename (SCM p, SCM pitches);
  static SCM member_pitch (SCM p, SCM pitches);
  static SCM step_scm (SCM tonic, SCM p);
  static SCM missing_thirds (SCM pitches);
  static SCM to_pitches (SCM chord);
  static SCM add_above_tonic (SCM pitch, SCM pitches);
  static SCM add_below_tonic (SCM pitch, SCM pitches);
  static SCM tonic_add_sub_to_pitches (SCM tonic, SCM add, SCM sub);
  static Simultaneous_music *get_chord (SCM tonic, SCM add, SCM sub, SCM inversion, SCM bass, SCM dur);
};

#endif /* CHORD_HH */

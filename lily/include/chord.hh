/*
  chord.hh -- declare Chord

  source file of the GNU LilyPond music typesetter

  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_HH
#define CHORD_HH

#include "pitch.hh"

/*
  ``chord'' is encoded:
  (PITCHES . (INVERSION . BASS))

  Chord:: namespace...
 */
class Chord
{
public:
  static SCM pitches_and_requests_to_chord (SCM pitches,
				     SCM tonic_req,
				     SCM inversion_req,
				     SCM bass_req,
				     bool find_inversion_b);
  static SCM base_pitches (SCM tonic);
  static SCM transpose_pitches (SCM tonic, SCM pitches);
  static SCM lower_step (SCM tonic, SCM pitches, SCM step);
  static SCM member_notename (SCM p, SCM pitches);
  static int step_i (Pitch tonic, Pitch p);
  static SCM step_scm (SCM tonic, SCM p);
  static SCM missing_thirds (SCM pitches);
  static SCM to_pitches (SCM chord);
  static SCM guess_tonic (SCM pitches);
  static SCM rebuild_from_base (SCM pitches, SCM base);
  static SCM rebuild_insert_inversion (SCM pitches); //, SCM tonic);
  static SCM rebuild_with_bass (SCM pitches, SCM bass);
  static SCM tonic_add_sub_inversion_bass_to_scm (SCM tonic, SCM add, SCM sub,
					    SCM inversion, SCM bass);
  static Simultaneous_music *get_chord (SCM tonic, SCM add, SCM sub, SCM inversion, SCM bass, SCM dur);
};

#endif /* CHORD_HH */

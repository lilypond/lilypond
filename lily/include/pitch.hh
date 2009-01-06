/*
  pitch.hh -- declare Pitch

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSICAL_PITCH_HH
#define MUSICAL_PITCH_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "rational.hh"


/** A "tonal" pitch. This is a pitch used in diatonal western music
    (24 quartertones in an octave), as opposed to a frequency in Hz or a
    integer number of semitones.

    Pitch is lexicographically ordered by (octave, notename,
    alteration).

*/
class Pitch
{
private:
  int octave_;
  int notename_;
  Rational alteration_;
  Scale *scale_;

  void transpose (Pitch);
  void up_to (int);
  void down_to (int);
  void normalize_octave ();
  void normalize_alteration ();
  void normalize ();

public:
  int get_octave () const;
  int get_notename () const;
  Rational get_alteration () const;

  Pitch (int octave, int notename, Rational accidental);
  Pitch (int octave, int notename);
  Pitch ();

  Pitch transposed (Pitch) const;
  Pitch to_relative_octave (Pitch) const;

  static int compare (Pitch const &, Pitch const &);

  int steps () const;
  Rational tone_pitch () const;
  int rounded_semitone_pitch () const;
  int rounded_quartertone_pitch () const;
  Pitch negated () const;
  string to_string () const;

  DECLARE_SCHEME_CALLBACK (less_p, (SCM a, SCM b));
  DECLARE_SIMPLE_SMOBS (Pitch);
};


enum {
  DOUBLE_FLAT = -4,
  THREE_Q_FLAT,
  FLAT,
  SEMI_FLAT,
  NATURAL,
  SEMI_SHARP,
  SHARP,
  THREE_Q_SHARP,
  DOUBLE_SHARP,
};

extern Rational  DOUBLE_FLAT_ALTERATION;
extern Rational  THREE_Q_FLAT_ALTERATION;
extern Rational  FLAT_ALTERATION;
extern Rational  SEMI_FLAT_ALTERATION;
extern Rational  NATURAL_ALTERATION;
extern Rational  SEMI_SHARP_ALTERATION;
extern Rational  SHARP_ALTERATION;
extern Rational  THREE_Q_SHARP_ALTERATION;
extern Rational  DOUBLE_SHARP_ALTERATION;

SCM ly_pitch_diff (SCM pitch, SCM root);
SCM ly_pitch_transpose (SCM p, SCM delta);
DECLARE_UNSMOB (Pitch, pitch);

INSTANTIATE_COMPARE (Pitch, Pitch::compare);

extern SCM pitch_less_proc;
Pitch pitch_interval (Pitch const &from, Pitch const &to);

#endif /* MUSICAL_PITCH_HH */


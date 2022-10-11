/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef PITCH_HH
#define PITCH_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "rational.hh"

/*
  A "tonal" pitch.  This is a pitch used in diatonal western music
  (24 quartertones in an octave), as opposed to a frequency in Hz or a
  integer number of semitones.

  Pitch is lexicographically ordered by (octave, notename, alteration).
*/

class Pitch : public Simple_smob<Pitch>
{
public:
  static SCM equal_p (SCM, SCM);
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;

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

  Pitch (int octave, int notename, Rational accidental = 0);
  Pitch ();

  Pitch transposed (Pitch) const;
  Pitch to_relative_octave (Pitch) const;

  Pitch normalized () const;

  static int compare (Pitch const &, Pitch const &);

  int steps () const;
  Rational tone_pitch () const;
  int rounded_semitone_pitch () const;
  int rounded_quartertone_pitch () const;
  Pitch negated () const;
  std::string to_string () const;

  DECLARE_SCHEME_CALLBACK (less_p, (SCM a, SCM b));
};

enum
{
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

extern Rational DOUBLE_FLAT_ALTERATION;
extern Rational THREE_Q_FLAT_ALTERATION;
extern Rational FLAT_ALTERATION;
extern Rational SEMI_FLAT_ALTERATION;
extern Rational NATURAL_ALTERATION;
extern Rational SEMI_SHARP_ALTERATION;
extern Rational SHARP_ALTERATION;
extern Rational THREE_Q_SHARP_ALTERATION;
extern Rational DOUBLE_SHARP_ALTERATION;

SCM ly_pitch_diff (SCM pitch, SCM root);
SCM ly_pitch_transpose (SCM p, SCM delta);

INSTANTIATE_COMPARE (Pitch, Pitch::compare);

extern SCM pitch_less_proc;
Pitch pitch_interval (Pitch const &from, Pitch const &to);
extern SCM Pitch_type_p_proc;

#include "context.hh"
void set_middle_C (Context *context);

#endif /* PITCH_HH */

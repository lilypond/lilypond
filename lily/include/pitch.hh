/*
  pitch.hh -- declare Pitch

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSICAL_PITCH_HH
#define MUSICAL_PITCH_HH

#include "lily-proto.hh"
#include "smobs.hh"

#include "std-vector.hh"

struct Scale
{
  vector<int> step_semitones_;
  Scale ();
  Scale (Scale const&);
  DECLARE_SMOBS (Scale);
};


/** A "tonal" pitch. This is a pitch used in diatonal western music
    (24 quartertones in an octave), as opposed to a frequency in Hz or a
    integer number of semitones.

    Pitch is lexicographically ordered by (octave, notename,
    alteration).


    TODO:

    - add indeterminate octaves, so it can be used as a key in keySigature
*/
class Pitch
{
private:				// fixme
  /*
    TODO: use SCM
  */

  int notename_;
  int alteration_;
  int octave_;
  Scale *scale_;
  
  void transpose (Pitch);
  void up_to (int);
  void down_to (int);
  void normalise ();

public:

  int get_octave () const;
  int get_notename () const;
  int get_alteration () const;

  Pitch (int octave, int notename, int accidental);
  Pitch ();

  Pitch transposed (Pitch) const;
  Pitch to_relative_octave (Pitch) const;

  static int compare (Pitch const &, Pitch const &);

  int steps () const;
  int semitone_pitch () const;
  int quartertone_pitch () const;
  string to_string () const;

  DECLARE_SCHEME_CALLBACK (less_p, (SCM a, SCM b));
  DECLARE_SIMPLE_SMOBS (Pitch);
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

SCM ly_pitch_diff (SCM pitch, SCM root);
SCM ly_pitch_transpose (SCM p, SCM delta);
DECLARE_UNSMOB (Pitch, pitch);

INSTANTIATE_COMPARE (Pitch, Pitch::compare);

extern SCM pitch_less_proc;
Pitch pitch_interval (Pitch const &from, Pitch const &to);
extern Scale *default_global_scale;

#endif /* MUSICAL_PITCH_HH */


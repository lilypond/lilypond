/*   
  key-def.hh -- declare Key_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef KEY_DEF_HH
#define KEY_DEF_HH

#include "array.hh"
#include "musical-pitch.hh"

/**
  Universal key definition (Should rename class Key to 'Accidentals'?)

  FIXME: merge key.hh and key-def.hh classes.
 */
class Key_def
{
public:
  Key_def ();
  
  Array<Musical_pitch> pitch_arr_;
  int modality_i_;
  bool ordinary_key_b_;

  /// squash the octaves to 1
  void squash_octaves ();

  /// return number accidentals in key; ordinary key only
  int ordinary_accidentals_i () const;

  /// return number of flats in key
  int flats_i () const;

  /// return number of sharps in key
  int sharps_i () const;

  /// modality == 3
  bool minor_b () const;

  void transpose (Musical_pitch d);
};

#endif /* KEY_DEF_HH */

/*
  key.hh -- declare Key, Octave_key

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef KEY_HH
#define KEY_HH

#include "array.hh"
#include "lily-proto.hh"

/// administration of current key in one octave.
class Octave_key {

public:
  Array<int> accidental_i_arr_;
  Array<bool> internal_forceacc_b_arr_;
  void clear ();
  Octave_key();
  void set (int i, int acc);
  int acc (int i) const { return accidental_i_arr_[i]; }
  void print () const;
};

/// administration of accidentals
class Key
{
  /** for each octave a key. Has to be private since octave 0 isn't member 0.
   */
  Array<Octave_key> octaves_;
  int octave_to_index (int) const;
public:
  bool multi_octave_b_;
  
  void clear ();
  Octave_key const& oct (int) const;
  void set (int name, int acc);
  void set (Musical_pitch);
  void set_internal_forceacc (Musical_pitch);
  void clear_internal_forceacc (Musical_pitch);

  bool different_acc (Musical_pitch) const;
  bool internal_forceacc (Musical_pitch) const;
  bool double_to_single_acc (Musical_pitch) const;
  
  Key();
  void print () const;  
};

#endif // KEY_HH



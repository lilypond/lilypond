/*
  key.hh -- declare Key, Octave_key

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef KEY_HH
#define KEY_HH

#include "varray.hh"
#include "scalar.hh"

/// administration of current key in one octave.
class Octave_key {

public:
  Array<int> accidental_i_arr_;
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
  Octave_key&oct (int);
  void set (int name, int acc);
  void set (int oct, int name, int acc);
  Key();
  void print () const;  
};

#endif // KEY_HH



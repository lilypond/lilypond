/*
  key.hh -- declare Key

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef KEY_HH
#define KEY_HH

#include "varray.hh"
#include "scalar.hh"

/// administration of current key
class Key {
    Array<int> accidental_i_arr_;

    /* *************** */

public:
 
    Key();
    void set(int i, int acc);
    int acc(int i)const { return accidental_i_arr_[i]; }
};

/// administration of accidentals
struct Local_key
{
    void reset(Key);    
    Key& oct(int);
    Local_key();

private:
    Array<Key> octaves;
};

#endif // KEY_HH



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
    Array<int> accidentals;

    /* *************** */

public:
    Array<int> read(Array<Scalar> );
    Array<int> oldkey_undo(Array<Scalar>);

    Key();
    void set(int i, int acc);
    int acc(int i) { return accidentals[i]; }
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



/*
  key.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef KEY_HH
#define KEY_HH

#include "vray.hh"
#include "string.hh"

class Key {
    svec<int> accidentals;

    /****************/

public:
    svec<int> read(svec<String> );
    svec<int> oldkey_undo(svec<String>);

    Key();
    void set(int i, int acc);
    int acc(int i) { return accidentals[i]; }
};

struct Local_key
{
    void reset(Key);    
    Key& oct(int);
    Local_key();

private:
    svec<Key> octaves;
};

#endif // KEY_HH



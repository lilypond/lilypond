/*
  scriptdef.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SCRIPTDEF_HH
#define SCRIPTDEF_HH
#include "string.hh"
struct Script_def{
    int stemdir;
    int staffdir;

    bool invertsym;
    String symidx;

    /****************/
    void print() const;
    Script_def(String, int, int ,bool);
};


#endif // SCRIPTDEF_HH


/*
  identparent.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef IDENTPARENT_HH
#define IDENTPARENT_HH

#include "proto.hh"
#include "string.hh"

struct Identifier
{
    void *data;
    String name;
    
    Identifier(String n) : name(n) { }
    virtual ~Identifier() {}
    
    virtual Input_staff * staff(bool = false) { assert(false); }
    virtual Horizontal_music*hmusic(bool = false) { assert(false); }
    virtual Vertical_music*vmusic(bool = false) { assert(false); }
    virtual Music_voice *mvoice(bool = false) { assert(false); }
    virtual Symtables *symtables(bool = false) { assert(false); }
    virtual Music_general_chord *mchord(bool = false) { assert(false); }
    virtual Lookup*lookup(bool = false) { assert(false); }
};
#endif // IDENTPARENT_HH




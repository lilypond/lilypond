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
    virtual Input_staff * staff() { assert(false); }
    virtual Voice_list * voices() { assert(false); }
    virtual Horizontal_music*hmusic() { assert(false); }
    virtual Vertical_music*vmusic() { assert(false); }
    virtual Music_voice *mvoice() { assert(false); }
    virtual Music_general_chord *mchord() { assert(false); }    
};
#endif // IDENTPARENT_HH




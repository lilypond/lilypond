
/*
  identifier.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH
#include "proto.hh"
#include "string.hh"

struct Identifier
{
    void *data;
    String name;
    
    Identifier(String n) ;
    virtual ~Identifier();
    virtual Staff * staff() { assert(false); }
    virtual Voice_list * voices() { assert(false); }
};

struct Staff_id : Identifier {
    Staff_id(String s, Staff*st):Identifier(s) { data = st; }
    virtual Staff* staff() { return (Staff*) data; }
    ~Staff_id();
};

struct Voices_id : Identifier {
    Voices_id(String s, Voice_list*st):Identifier(s) { data = st; }
    virtual Voice_list * voices() { return (Voice_list*)data; }
    ~Voices_id();
};
#endif // IDENTIFIER_HH


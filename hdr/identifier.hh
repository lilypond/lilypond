
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
    virtual Voice * voice() { assert(false); }
};

struct Staff_id : Identifier {
    Staff_id(String s, Staff*st):Identifier(s) { data = st; }
    virtual Staff* staff() { return (Staff*) data; }
    ~Staff_id();
};

struct Voice_id : Identifier {
    Voice_id(String s, Voice*st):Identifier(s) { data = st; }
    virtual Voice * voice() { return (Voice*)data; }
    ~Voice_id();
};
#endif // IDENTIFIER_HH


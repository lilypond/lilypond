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
    virtual const char*classname() { return "new Identifier"; }
    void error();
    virtual Input_staff * staff(bool = false) { error(); return 0; }
    virtual Input_music *music(bool = false) { error(); return 0; }
    virtual Music_voice *mvoice(bool = false) { error(); return 0; }
    virtual Symtables *symtables(bool = false) { error(); return 0; }
    virtual Music_general_chord *mchord(bool = false) { error(); return 0; }
    virtual Lookup*lookup(bool = false) { error(); return 0; }
    virtual Notename_tab*notename_tab(bool = false) { error(); return 0; }
};
#endif // IDENTPARENT_HH




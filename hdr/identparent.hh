/*
  identparent.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef IDENTPARENT_HH
#define IDENTPARENT_HH

#include "proto.hh"
#include "string.hh"
#define IDACCESSOR( Input_staff, staff)\
    virtual Input_staff * staff(bool = false) { error(#Input_staff); return 0; }

struct Identifier {
    void *data;
    String name;
    
    Identifier(String n) : name(n) { }
    virtual ~Identifier() {}
    virtual const char*classname() { return "new Identifier"; }
    void error(String);
    IDACCESSOR(Input_staff, staff)
    IDACCESSOR(Input_music, music)
    IDACCESSOR(Music_voice, mvoice)
    IDACCESSOR(Script_def, script)	
    IDACCESSOR(Symtables, symtables)
    IDACCESSOR(Music_general_chord, mchord)
    IDACCESSOR(Lookup,lookup)
    IDACCESSOR(Notename_tab, notename_tab)
private:
    Identifier(Identifier const&){}
};
#endif // IDENTPARENT_HH




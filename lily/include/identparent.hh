/*
  identparent.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef IDENTPARENT_HH
#define IDENTPARENT_HH

#include "proto.hh"
#include "string.hh"

/* boolean argument to accesor is copy_b..*/
#define IDACCESSOR( Input_staff, staff)\
    virtual Input_staff * staff(bool) { error(#Input_staff); return 0; }

struct Identifier {
    void *data;
    String name;
    
    Identifier(String n) : name(n) { }
    virtual ~Identifier() {}

    void print()const;
    virtual const char*classname() { return "new Identifier"; }
    void error(String);
    IDACCESSOR(Input_staff, staff)
    IDACCESSOR(Input_music, music)
    IDACCESSOR(Music_voice, mvoice)
    IDACCESSOR(Script_def, script)	
    IDACCESSOR(Symtables, symtables)
    IDACCESSOR(Music_general_chord, mchord)
    IDACCESSOR(Lookup,lookup)
    IDACCESSOR(Real,real)
    IDACCESSOR(Notename_tab, notename_tab)
protected:
    virtual void do_print()const=0;
private:
    Identifier(Identifier const&){}
    
};
#endif // IDENTPARENT_HH




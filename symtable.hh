/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef SYMTABLE_HH
#define SYMTABLE_HH
#include "assoc.hh"
#include "string.hh"
#include "symbol.hh"

struct  Symtable : public Assoc<String, Symbol> {
    Symbol lookup(String)const;
};


struct Symtables : private Assoc<String, Symtable*> {
    void read(Text_db&) ;
    Symtable* operator()(String s);

};


#endif


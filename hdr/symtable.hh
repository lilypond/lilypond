/*
  lilypond, (c) 1996,97 Han-Wen Nienhuys
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
    
    Symtable* operator()(String s);
    ~Symtables();
    Symtables();
    Symtables(Symtables const&);
    Assoc<String, Symtable*>::add;
};


#endif


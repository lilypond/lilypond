/*
  symtable.hh -- declare Symtable, Symtables

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SYMTABLE_HH
#define SYMTABLE_HH

#include "dictionary.hh"
#include "string.hh"
#include "atom.hh"

struct  Symtable : public Dictionary<Atom> {
    String id_str;
    
    Atom lookup (String) const;
    void print() const;
};


struct Symtables : private Dictionary<Symtable*>
{
  Symtables();
  Symtables (Symtables const&);
  ~Symtables();

  Symtable* operator()(String s);
  void add (String, Symtable*);
  void print() const;

  String font_;
  String font_path_;  
};


#endif


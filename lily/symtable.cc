/*
  symtable.cc -- implement Symbol_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "misc.hh"
#include "dimen.hh"
#include "debug.hh"
#include "real.hh"
#include "atom.hh"
#include "assoc.hh"
#include "assoc-iter.hh"
#include "symtable.hh"

Symtables::Symtables()
{
}


Symtables::Symtables (Symtables const &s)
  : Dictionary<Symtable*> (s)
{
  for (Assoc_iter<String, Symtable*>  i (s); i.ok(); i++)
    {
      add (i.key(), new Symtable (*i.val ()));
    }
}

Symtables::~Symtables()
{
  for (Assoc_iter<String, Symtable*>  i (*this); i.ok(); i++)
    {
      delete i.val();
    }
}

Atom
Symtable::lookup (String s) const
{
  if (elt_b (s))
    return (*this)[s];
  else
    {
      warning ("Symtable `" + id_str+ _("\': unknown symbol `") +s+"'\n");
      Atom sy;
      return sy;
    }
}

Symtable*
Symtables::operator()(String s)
{
  if (!elt_b (s))
    {
      error ("Symtable `" + s + _("\' unknown"));
      /* 
	 We can't return, because we'll dump core anyway.
       */
      return 0;
    }
  else
    return Dictionary<Symtable*>::operator[](s);
}
void
Symtables::print() const
{
  for (Assoc_iter<String, Symtable*>  i (*this); i.ok(); i++)
    {
      DOUT << "table \'" << i.key() << "\' {\n";
      i.val()->print ();
      DOUT << "}\n";
    }
}
void
Symtable::print() const
{
  for (Assoc_iter<String, Atom>  i (*this); i.ok(); i++)
    {
      DOUT << "\'" << i.key() << "\'->" << i.val ().str () << "\n";
    }
}

void
Symtables::add (String s, Symtable*p)
{
  p-> id_str = s;
  Dictionary<Symtable*>::add (s,p);
}

/*
  symtable.cc -- implement Symbol_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "misc.hh"
#include "dimension.hh"
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
  font_ = s.font_;
  font_path_ = s.font_path_;
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
  if (elem_b (s))
    {
      Atom a (elem(s));
      return a;
    }
  else
    {
      warning (_f ("Symtable `%s\': unknown symbol: `%s\'", id_str, s));
      Atom sy;
      return sy;
    }
}

Symtable*
Symtables::operator()(String s)
{
  if (!elem_b (s))
    {
      error (_f ("Symtable `%s\' unknown", s));
      /* 
	 We can 't return, because we'll dump core anyway.
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
      DOUT << "table \'" << i.key () << "\' {\n";
      i.val()->print ();
      DOUT << "}\n";
    }
}
void
Symtable::print() const
{
  for (Assoc_iter<String, Atom>  i (*this); i.ok(); i++)
    {
      DOUT << "\'" << i.key() << "\'->" << i.val ().str () << '\n';
    }
}

void
Symtables::add (String s, Symtable*p)
{
  p-> id_str = s;
  Dictionary<Symtable*>::add (s,p);
}

/*   
  scope.cc --  implement Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scope.hh"
#include "dictionary-iter.hh"
#include "debug.hh"
#include "identifier.hh"

void
Scope::print () const
{
  bool init_b = false;		// ugh
  for (Scope_iter ai (*this);  ai.ok(); ai++)
    {
      if (ai.val()->init_b_ == init_b)
	{
	  DOUT << ai.key() << "=";
	  ai.val()->print ();
	}
    }
}

Scope::~Scope ()
{
  for (Scope_iter	 ai (*this); ai.ok(); ai++)
    {
      DOUT << "deleting: " << ai.key() << '\n';
      delete ai.val ();
    }
}

Scope::Scope (Scope const&s)
  : Hash_table<Protected_scm,Identifier*> (s)
{
  for (Scope_iter ai (s); ai.ok(); ai++)
    {
      (*this)[ai.scm_key ()] = ai.val ()->clone ();
    }
}

unsigned int scm_hash (Protected_scm s)
{
  return scm_ihashv (s, ~1u);
}

Scope::Scope ()
{
  hash_func_ = scm_hash;
}

bool
Scope::elem_b (String s) const
{
  return elem_b (ly_symbol (s.ch_C()));
}


Identifier *&
Scope::elem (String s) 
{
  return elem (ly_symbol (s.ch_C()));
}


Scope_iter::Scope_iter (Scope const &s)
  : Hash_table_iter<Protected_scm,Identifier*>(s)
{
}

String
Scope_iter::key () const
{
  SCM s= Hash_table_iter<Protected_scm,Identifier*>::key ();
  return symbol_to_string (s);
}

bool
Scope::elem_b (SCM s) const
{
  return Hash_table<Protected_scm,Identifier*> ::elem_b (s);
}

Identifier* &
Scope::elem (SCM s)
{
  return Hash_table<Protected_scm,Identifier*> ::elem (s);
}

SCM
Scope_iter::scm_key () const
{
  return Hash_table_iter<Protected_scm,Identifier*>::key ();
}

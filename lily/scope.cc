/*   
  scope.cc --  implement Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scope.hh"
#include "dictionary-iter.hh"
#include "debug.hh"
#include "identifier.hh"

void
Scope::print () const
{
  bool init_b = false;		// ugh
  for (Dictionary_iter<Identifier*> ai (*this);  ai.ok(); ai++)
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
  for (Dictionary_iter<Identifier*>	 ai (*this); ai.ok(); ai++)
    {
      DOUT << "deleting: " << ai.key() << '\n';
      delete ai.val ();
    }
}

Scope::Scope (Scope const&s)
  : Dictionary<Identifier*> (s)
{
  for (Dictionary_iter<Identifier*> ai (s); ai.ok(); ai++)
    {
      (*this)[ai.key ()] = ai.val ()->clone ();
    }
}

Scope::Scope ()
{}



/*   
  scope.cc --  implement Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#include "scope.hh"
#include "assoc-iter.hh"
#include "debug.hh"
#include "identifier.hh"

void
Scope::print () const
{
  bool init_b = false;		// ugh
  for (Assoc_iter<String,Identifier*> ai (*this);  ai.ok(); ai++)
    {
      if (ai.val()->init_b_ == init_b)
	{
	  DOUT << ai.key() << '=';
	  ai.val()->print ();
	}
    }
}

Scope::~Scope ()
{
  for (Assoc_iter<String,Identifier*>	 ai (*this); ai.ok(); ai++)
    {
      DOUT << "deleting: " << ai.key()<<'\n';
      delete ai.val();
    }
}

Scope::Scope (Scope const&s)
  : Dictionary<Identifier*> (s)
{
  for (Assoc_iter<String,Identifier*> ai (s); ai.ok(); ai++)
    {
      (*this)[ai.key ()] = ai.val ()->clone ();
    }
}

Scope::Scope ()
{}

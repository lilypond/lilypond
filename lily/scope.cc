/*   
  scope.cc --  implement Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scope.hh"
#include "dictionary-iter.hh"
#include "debug.hh"
#include "identifier.hh"
#include "dictionary.hh"
#include "protected-scm.hh"


Scope::~Scope ()
{
  for (Scope_iter ai (*this); ai.ok(); ai++)
    delete ai.val ();
  delete id_dict_;
}

Scope::Scope (Scope const&s)
{
  id_dict_ = new Hash_table<Protected_scm,Identifier*> (*s.id_dict_);
  for (Scope_iter ai (s); ai.ok(); ai++)
    {
      id_dict_->elem (ai.scm_key ()) = ai.val ()->clone ();
    }
}

unsigned int ly_pscm_hash (Protected_scm s)
{
  return ly_scm_hash (s);
}


Scope::Scope ()
{
  id_dict_ = new Hash_table<Protected_scm,Identifier*>;
  id_dict_->hash_func_ = ly_pscm_hash;
}

bool
Scope::elem_b (String s) const
{
  return id_dict_->elem_b (ly_symbol2scm (s.ch_C()));
}


Identifier *&
Scope::elem (String s) 
{
  return id_dict_->elem (ly_symbol2scm (s.ch_C()));
}


Scope_iter::Scope_iter (Scope const &s)
{
  iter_ = new Hash_table_iter<Protected_scm,Identifier*>(*s.id_dict_);
}

String
Scope_iter::key () const
{
  SCM s= iter_->key ();
  return ly_symbol2string (s);
}

bool
Scope::elem_b (SCM s) const
{
  return id_dict_->elem_b (s);
}

Identifier* &
Scope::elem (SCM s)
{
  return id_dict_->elem (s);
}

SCM
Scope_iter::scm_key () const
{
  return iter_->key ();
}

bool
Scope_iter::ok () const
{
  return iter_->ok();
}

void
Scope_iter::operator ++(int)
{
  (*iter_) ++;
}

Identifier*
Scope_iter::val ()const
{
  return iter_->val ();
}

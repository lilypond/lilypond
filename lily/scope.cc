/*   
  scope.cc --  implement Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scope.hh"
#include "identifier.hh"
#include "scm-hash.hh"


Scope::~Scope ()
{
  delete id_dict_;
}

Scope::Scope (Scope const&s)
  : id_dict_ (new Scheme_hash_table (*s.id_dict_))
{
  /*
    cloning not necessary.

  id_dict_ = new Hash_table<Protected_scm,Identifier*> (*s.id_dict_);
  for (Scope_iter ai (s); ai.ok(); ai++)
    {
      id_dict_->elem (ai.scm_key ()) = ai.val ()->clone ();
    }
  */
}

Scope::Scope ()
{
  id_dict_ = new Scheme_hash_table;
}

bool
Scope::elem_b (String s) const
{
  return id_dict_->elem_b (ly_symbol2scm (s.ch_C()));
}


bool
Scope::elem_b (SCM s) const
{
  return id_dict_->elem_b (s);
}
Identifier*
Scope::elem (SCM s)const
{
  return unsmob_identifier  (id_dict_->get (s));
}

SCM
Scope::scm_elem (SCM s)const
{
  return id_dict_->get (s);
}

SCM
Scope::scm_elem (String s) const
{
 return scm_elem (ly_symbol2scm (s.ch_C()));
}

Identifier*
Scope::elem (String s)const
{
  return elem (ly_symbol2scm (s.ch_C()));
}

void
Scope::set (String s, SCM id)
{
  return id_dict_->set (ly_symbol2scm (s.ch_C()), id);
}

void
Scope::set (String s, Identifier * id) 
{
  return id_dict_->set (ly_symbol2scm (s.ch_C()), smobify (id));
}

SCM
Scope::to_alist () const
{
  return id_dict_->to_alist ();
}

/*   
  scope.cc --  implement Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scope.hh"
#include "string.hh"
#include "scm-hash.hh"

Scope::Scope (Scheme_hash_table * st)
{
  assert (st);
  id_dict_ =st;
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


void
Scope::set (String s, SCM id)
{
  return id_dict_->set (ly_symbol2scm (s.ch_C()), id);
}

SCM
Scope::to_alist () const
{
  return id_dict_->to_alist ();
}

bool
Scope::try_retrieve (SCM k , SCM *v)const
{
  return id_dict_->try_retrieve (k, v);
}


/*   
  scope.hh -- declare Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCOPE_HH
#define SCOPE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Scheme_hash_table;

/*
 Junk this almost-void class. 
 */
class Scope {
  Scheme_hash_table *id_dict_;
  Scope (Scope const &);
public:
  SCM to_alist () const; 
  bool elem_b (String ) const;
  bool elem_b (SCM s) const;

  bool try_retrieve (SCM key, SCM *val) const;
  
  SCM scm_elem (String) const;
  SCM scm_elem (SCM) const;


  void set (String, SCM);  
  Scope (Scheme_hash_table*);
  
  friend class Scope_iter;
};
#endif /* SCOPE_HH */


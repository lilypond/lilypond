/*   
  scope.hh -- declare Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCOPE_HH
#define SCOPE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Scheme_hash_table;
class Scope {
  Scheme_hash_table *id_dict_;
public:
  SCM to_alist () const; 
  bool elem_b (String ) const;
  bool elem_b (SCM s) const;
  Identifier *elem (String) const;
  Identifier *elem (SCM) const;

  SCM scm_elem (String) const;
  SCM scm_elem (SCM) const;

  void set (String, Identifier *);
  void set (String, SCM);  
  Scope ();
  
  Scope (Scope const &);
  ~Scope ();
  friend class Scope_iter;
};
#endif /* SCOPE_HH */


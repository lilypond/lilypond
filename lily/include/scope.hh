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
  void set (String, Identifier *);
  Scope ();
  
  Scope (Scope const &);
  ~Scope ();
  friend class Scope_iter;
};
#if 0
class Scope_iter {
  Hash_table_iter<Protected_scm,Identifier*> * iter_;
public:
  void operator ++(int);
  bool ok ()const;
  Scope_iter(Scope const&);
  String key () const;
  Identifier* val () const;
  SCM scm_key () const;
};

#endif
#endif /* SCOPE_HH */


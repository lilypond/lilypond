/*   
  scope.hh -- declare Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCOPE_HH
#define SCOPE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Protected_scm;
class Scope {
  Hash_table<Protected_scm,Identifier*> *id_dict_;
public:
  void print () const;
  bool elem_b (String ) const;
  bool elem_b (SCM s) const;
  Identifier *&elem (String);
  Identifier *&elem (SCM s);  
  Scope ();
  
  Scope (Scope const &);
  ~Scope ();
  friend class Scope_iter;
};

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

#endif /* SCOPE_HH */


/*   
  scope.hh -- declare Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCOPE_HH
#define SCOPE_HH

#include "dictionary.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"
#include "dictionary-iter.hh"
#include "protected-scm.hh"

class Scope : private Hash_table<Protected_scm,Identifier*> {
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

class Scope_iter : public Hash_table_iter<Protected_scm,Identifier*> {
public:
  Scope_iter(Scope const&);
  String key () const;
  SCM scm_key () const;
};

#endif /* SCOPE_HH */


/*   
  scope.hh -- declare Scope
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCOPE_HH
#define SCOPE_HH

#include "dictionary.hh"
#include "lily-proto.hh"

class Scope : public Dictionary<Identifier*> {
public:
  void print () const;
  Scope ();
  Scope (Scope const &);
  ~Scope ();
};

#endif /* SCOPE_HH */


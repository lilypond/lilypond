/*   
  scm-hash.hh -- declare Scheme hasher.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCM_HASH_HH
#define SCM_HASH_HH


#include <map>

#include "lily-guile.hh"
#include "smobs.hh"


struct SCM_less
{
  bool operator  () (SCM s1, SCM s2) const
  {
    return long(s1) < long (s2);
  }
};

typedef map<SCM,SCM, SCM_less> Scm_stl_map;

/**
   auto resizing hash table. This should come from GUILE.

   ALWAYS USE THIS AS VIA A POINTER, i.e.

   class Foo {
    Scheme_hash_table * tab;
   };

   and NOT

   class Foo {
    Scheme_hash_table tab;
   }
 */
class Scheme_hash_table :  private Scm_stl_map
{
public:
  bool try_retrieve (SCM key, SCM *val);
  bool elem_b (SCM key) const;

  /**
     WARNING: putting something in assumes responsibility for cleaning
     up.  */
  void set (SCM k, SCM v);
  SCM get (SCM k) const; 
  
  Scheme_hash_table ();
  void operator = (Scheme_hash_table const &); 
  Scheme_hash_table (Scheme_hash_table const &);
  virtual ~Scheme_hash_table ();
  DECLARE_SMOBS;
  SCM to_alist () const;
};

#endif /* SCM_HASH_HH */


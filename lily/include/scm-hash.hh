/*   
  scm-hash.hh -- declare Scheme hasher.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCM_HASH_HH
#define SCM_HASH_HH


#include <map>

#include "lily-guile.hh"
#include "smobs.hh"


struct SCM_less
{
  bool operator () (SCM s1, SCM s2) const
  {
    return long (s1) < long (s2);
  }
};

typedef std::map<SCM,SCM, SCM_less> Scm_stl_map;

/**
   auto resizing hash table. 

   1. ALWAYS USE THIS AS VIA A POINTER, i.e.

   class Foo {
    Scheme_hash_table * tab;
   };

   and NOT

   class Foo {
    Scheme_hash_table tab;
   }


   2. UPON DESTRUCTION, DO

   scm_unprotect_object (tab->self_scm_);




   TODO:

  - This should come from GUILE. We're typically doing double work,
   because KEY already is a symbol, and is looked up in a symbol
   hashtable.

  - use GUILE hashtables iso STL.
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
  void remove (SCM k);
  Scheme_hash_table ();
  void operator = (Scheme_hash_table const &); 
  Scheme_hash_table (Scheme_hash_table const &);

  SCM to_alist () const;
  DECLARE_SMOBS (Scheme_hash_table,foo);
};

#endif /* SCM_HASH_HH */


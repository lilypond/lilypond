/*   
  scm-hash.hh -- declare Scheme hasher.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCM_HASH_HH
#define SCM_HASH_HH


#include "lily-guile.hh"
#include "smobs.hh"


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

   scm_gc_unprotect_object (tab->self_scm_);




   TODO:

  - This should come from GUILE. We're typically doing double work,
   because KEY already is a symbol, and is looked up in a symbol
   hashtable.

  - use GUILE hashtables iso STL.
 */

class Scheme_hash_table
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
private:
  SCM hash_tab_;
  unsigned elt_count_;
  
  DECLARE_SMOBS (Scheme_hash_table,foo);
};


#endif /* SCM_HASH_HH */


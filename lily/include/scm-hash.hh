/*   
  scm-hash.hh -- declare Scheme hasher.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCM_HASH_HH
#define SCM_HASH_HH

#include "lily-guile.hh"
#include "hash-table.hh"
#include "smobs.hh"

/**
   auto resizing hash table. This should come from GUILE.
 */
class Scheme_hash_table : private Hash_table<SCM,SCM>
{
public:
  //  bool elem_b (SCM k) const;
  Hash_table<SCM,SCM>::try_retrieve;
  Hash_table<SCM,SCM>::elem_b;  
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


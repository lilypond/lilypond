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
class Scheme_hash_table : public Hash_table<SCM,SCM>
{
public:  
  Scheme_hash_table ();
  void operator = (Scheme_hash_table const &); 
  Scheme_hash_table (Scheme_hash_table const &);
  virtual ~Scheme_hash_table ();
  DECLARE_SMOBS;
  SCM to_alist () const;
};

#endif /* SCM_HASH_HH */


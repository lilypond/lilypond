/*
  Implement storage and manipulation of grob properties.
 */

#include <string.h>
#include <math.h>

#include "main.hh"
#include "input-smob.hh"

#include "group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "grob.hh"

#include "spanner.hh"
#include "item.hh"
#include "misc.hh"
#include "item.hh"

/*
  HASHING_FOR_MUTABLE_PROPS:

  
  plain, -O0 compile
  
user	0m12.400s

sz == 13, -O0 compile
  
 xdvi trip

user	0m13.780s
  
sz == 5


user	0m13.000s

sz == 3


user	0m13.080s

Hashing doesn't improve the result of grob property lookup, at least
not with naive hashing. It is possible that the overhead of the
scm_hash* functions take too much time. One way to solve this is by
using vector accesses directly, and precompute the hashvalues, similar
to CACHE_SYMBOLS. That option could only cause slowdowns if the hash
tables produces weird cache-line trashing.

Second option: we could index immutable props in a hash tab as
well. This only takes space, since they are immutable no updates are
needed.  This does take a lot of space, since we must duplicate the
alists (but not the entries).

*/

// #define HASHING_FOR_MUTABLE_PROPS

SCM
Grob::get_property_alist_chain (SCM def) const
{
#ifndef HASHING_FOR_MUTABLE_PROPS
  return  scm_list_n (mutable_property_alist_,
		      immutable_property_alist_,
		      def,
		      SCM_UNDEFINED);
#else
  SCM chain = gh_list (immutable_property_alist_, def, SCM_UNDEFINED);
  SCM * velts = SCM_VELTS (mutable_property_alist_);
  int l = SCM_VECTOR_LENGTH(mutable_property_alist_);
  for (int i = 0; i < l; i++)
    {
      if (gh_pair_p (velts[i]))
	chain = gh_cons ( velts[i], chain);
    }

  return chain;
#endif
}



/*
  This special add_thing routine is slightly more efficient than

    set_prop (name,cons (thing, get_prop (name)))

  since it can reuse the handle returned by scm_assq().
*/
void
Grob::add_to_list_property (SCM sym, SCM thing) 
{
  SCM handle
#ifndef HASHING_FOR_MUTABLE_PROPS
    = scm_sloppy_assq (sym, mutable_property_alist_)
#else
    = scm_hashq_get_handle (mutable_property_alist_, sym);
#endif
    ;
  if (handle != SCM_BOOL_F)
    {
      gh_set_cdr_x (handle, gh_cons (thing, gh_cdr (handle)));
    }
  else
    {
      /*
	There is no mutable prop yet, so create an entry, and put it in front of the
	mutable prop list.
      */
      handle = scm_sloppy_assq (sym, immutable_property_alist_);
      SCM tail = (handle != SCM_BOOL_F) ? gh_cdr(handle) : SCM_EOL;
      SCM val = gh_cons (thing, tail);
#ifndef HASHING_FOR_MUTABLE_PROPS
      mutable_property_alist_ = gh_cons (gh_cons (sym, val),
					 mutable_property_alist_);
#else
      scm_hashq_set_x (mutable_property_alist_, sym, val);
#endif
    }
}


extern void check_interfaces_for_property (Grob const *me, SCM sym);

void
Grob::internal_set_grob_property (SCM s, SCM v)
{
  /*
    Perhaps we simply do the assq_set, but what the heck.
   */
  if (!live())
    return ; 

#ifndef NDEBUG
  if (internal_type_checking_global_b)
    {
      assert (type_check_assignment (s, v, ly_symbol2scm ("backend-type?")));
      check_interfaces_for_property(this, s);
    }
#endif

#ifndef HASHING_FOR_MUTABLE_PROPS
  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
#else
  scm_hashq_set_x (mutable_property_alist_, s, v);
#endif
}


SCM
Grob::internal_get_grob_property (SCM sym) const
{
#ifndef HASHING_FOR_MUTABLE_PROPS
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return ly_cdr (s);
#else
  if (mutable_property_alist_ == SCM_EOL)
    return SCM_EOL;
  
  SCM s = scm_hashq_ref (mutable_property_alist_, sym, SCM_EOL);
  if (s!=SCM_EOL)
    return s;
#endif

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  
#ifndef NDEBUG
  if (internal_type_checking_global_b && gh_pair_p (s))
    {
      assert (type_check_assignment (sym, gh_cdr (s), ly_symbol2scm ("backend-type?")));
      check_interfaces_for_property(this, sym);
    }
#endif

  return (s == SCM_BOOL_F) ? SCM_EOL : ly_cdr (s); 
}

void
Grob::substitute_mutable_properties (SCM crit, SCM orig)
{
  set_break_subsititution(crit);
#ifndef HASHING_FOR_MUTABLE_PROPS
  mutable_property_alist_ = substitute_mutable_property_alist (orig);
#else
  if (orig == SCM_EOL)
    {
      mutable_property_alist_ = SCM_EOL;
      return ;
    }
  
  SCM * src_elts = SCM_VELTS (orig);
  SCM * dest_elts = SCM_VELTS (mutable_property_alist_);  
  unsigned int l = SCM_VECTOR_LENGTH(mutable_property_alist_);
  assert (l == SCM_VECTOR_LENGTH(orig));
  for (unsigned int i = 0; i < l; i++)
    {
      dest_elts[i] = substitute_mutable_property_alist (src_elts[i]);
    }
#endif
}


bool
Grob::live () const
{
  return immutable_property_alist_ != SCM_EOL;
}

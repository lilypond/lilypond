/*   
  smobs.hh -- declare smob related stuff.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SMOBS_HH
#define SMOBS_HH

#include "lily-guile.hh"

/**
   A smob is a C++ class with static member functions to glue it with
   Scheme. Every instance carries SELF_SCM_, a pointer to the Scheme
   smob of itself.  Upon destruction, SELF_SCM_ is set to SCM_EOL.

   smob_free() checks if SELF_SCM_ equals its argument, so we can also
   use a smobbified object on the stack: the destruction will happen
   before GC hits the object.

   This is discouraged, though, because it causes memory leaks, and has
   weird semantics.
   

*/

#define DECLARE_SMOBS					\
	SCM smobify_self ();					\
	static SCM mark_smob (SCM);				\
	static scm_sizet free_smob (SCM s);			\
	static int print_smob (SCM s, SCM p, scm_print_state*);	\
	static long smob_tag_;					\
	static SCM equal_p (SCM a, SCM b);\
	static void init_smobs();				\
	void unsmobify_self ();\
        void do_smobify_self();\
	SCM self_scm_;


/**
   Check if S is of the specified C++ class.
 */
#define SMOB_IS_TYPE_B(TYPE, S)  (SCM_NIMP((S)) && SCM_CAR((S)) == TYPE::smob_tag_)

/// Cast S.  No checks are done.
#define SMOB_TO_TYPE(TYPE, S)  ((TYPE*) SCM_CDR((S)))
#endif /* SMOBS_HH */


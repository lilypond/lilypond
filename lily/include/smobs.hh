/*   
  smobs.hh -- declare smob related stuff.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SMOBS_HH
#define SMOBS_HH

#include "lily-guile.hh"


/*

   Each smobbed C-object may only be interfaced by a single, unique
   smob cell. Therefore NEVER provide a public function that will
   create a smobcell for an existing object pointer.

   There are two ways to reach this goal:

   simple smobs:

   - Data structures that are encapsulated by GUILE. If constructed
   through GUILE, you may only store them as protected SCMs, and may
   not copy the pointer the object itself. Typical interface

   struct Ssmob {
   public:
     SCM make_copy_scm () const {
       Ssmob *sp = new Ssmob (*this);
       return sp->smobbed_self ();
     }
   };

   or

   struct Ssmob {
   public:
     DECLARE_SIMPLE_SMOBS;
     static SCM make_scm (void initdata) {
       Ssmob * sp = new Ssmob (initdata);
       return sp->smobbed_self ();
     }
   private:
     Ssmob (initdata);
   }

   Objets of type Ssmob may live on the stack, or on the heap, or as
   part of other objects.  However, as soon as the object is smobbed,
   by definition (by definition of the constructors, in this example),
   lives on the heap as a separate object
   
   - complex smobs: data structures whose identity is referenced and
   stored both in C++ and in GUILE form. From going from C++ to GUILE,
   you use smob_ptr->self_scm_

   class Csmob {
     DECLARE_SMOBS;
     Csmob () { smobify_self (); }
     Csmob (Csmob const & s) {
       // don't copy self_scm_
       smobify_self ();
     }
   };
   
   A complex smob is a C++ class with static member functions to glue
   it with Scheme. Every instance carries SELF_SCM_, a pointer to the
   unique Scheme smob cell of itself.

   Upon creation, SELF_SCM_ is protected, so if you choose to store it
   in C++ structures, you need to do

   class Bla {
   Csmob *ptr;
   ~Bla () {  scm_unprotect_object (ptr->self_scm_); }
   
   };

   If protection is done via GUILE, don't forget to unprotect AFTER putting
   stuff into the GUILE datastructs


   guile_data = gh_cons (ptr->self_scm_, guile_data);
   ptr->self_scm_

   Since GUILE takes care of the freeing the object, the destructor
   is private.

   DUMMY a thing to make sure compiles only work if this header
   if this file is there.


   WARNING:

   smobify_self () might trigger a GC, so make sure that objects are  
   sane when you do smobify_self ().
*/

#define DECLARE_SIMPLE_SMOBS(CL,dummy) \
protected: \
	friend class Non_existant_class ; \
	SCM smobbed_self () const; \
private:\
	static long smob_tag_;					\
	static SCM mark_smob (SCM);				\
	static scm_sizet free_smob (SCM s);			\
	static int print_smob (SCM s, SCM p, scm_print_state*);	\
public: \
	static SCM equal_p (SCM a, SCM b);\
	static CL * unsmob (SCM);\
	static SCM smob_p (SCM);\
	static void init_smobs ();				\
private:


#define DECLARE_SMOBS(CL,dummy)					\
	DECLARE_SIMPLE_SMOBS (CL,dammy) \
protected:\
	virtual ~CL ();\
	SCM unprotected_smobify_self ();\
private: \
	SCM smobify_self ();					\
	SCM self_scm_; \
public: \
	SCM self_scm () const { return self_scm_; } \
private:


#endif /* SMOBS_HH */


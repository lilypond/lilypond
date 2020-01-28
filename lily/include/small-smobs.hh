#ifndef SMALL_SMOBS_HH
#define SMALL_SMOBS_HH

#include "smobs.hh"

#if GUILEV2
// Fix the APIs of GUILE2.x, broken in 2009--2014
#undef SCM_SMOB_OBJECT_LOC
#undef SCM_SMOB_OBJECT_2_LOC
#undef SCM_SMOB_OBJECT_3_LOC
#define SCM_SMOB_OBJECT_LOC(x) (SCM_SMOB_OBJECT_N_LOC ((x), 1))
#define SCM_SMOB_OBJECT_2_LOC(x) (SCM_SMOB_OBJECT_N_LOC ((x), 2))
#define SCM_SMOB_OBJECT_3_LOC(x) (SCM_SMOB_OBJECT_N_LOC ((x), 3))
#endif

// This is tricky: the small smobs contain all the data in the smob
// itself.  Any derived classes must _not_ contain any data members or
// be polymorphic (contain a virtual table pointer) as there is no
// place to store any of that.  The class itself does not contain a
// data member either.  Pointers to it are basically meaningless and
// are instead reinterpreted directly as an SCM value in order to have
// unsmob and friends behave as customary.

template <class Super> class Smob1 : public Smob_base<Super>
{
  Smob1 () = delete;
  Smob1 (const Smob1 &) = delete;
  Smob1 &operator= (const Smob1 &) = delete;

public:
  SCM self_scm () const { return SCM_PACK (this); }
  SCM &scm1 () const { return *SCM_SMOB_OBJECT_LOC (self_scm ()); }
  static SCM make_smob (SCM arg1 = SCM_UNDEFINED)
  {
    SCM_RETURN_NEWSMOB (Smob_base<Super>::smob_tag (), SCM_UNPACK (arg1));
  }
  SCM mark_smob () const { return scm1 (); };
  static Super *unchecked_unsmob (SCM s)
  {
    return reinterpret_cast<Super *> (SCM_UNPACK (s));
  }
};

template <class Super> class Smob2 : public Smob_base<Super>
{
  Smob2 () = delete;
  Smob2 (const Smob2 &) = delete;
  Smob2 operator= (const Smob2 &) = delete;

public:
  SCM self_scm () const { return SCM_PACK (this); }
  SCM &scm1 () const { return *SCM_SMOB_OBJECT_LOC (self_scm ()); }
  SCM &scm2 () const { return *SCM_SMOB_OBJECT_2_LOC (self_scm ()); }
  static SCM make_smob (SCM arg1 = SCM_UNDEFINED, SCM arg2 = SCM_UNDEFINED)
  {
    SCM_RETURN_NEWSMOB2 (Smob_base<Super>::smob_tag (), SCM_UNPACK (arg1),
                         SCM_UNPACK (arg2));
  }
  SCM mark_smob () const
  {
    scm_gc_mark (scm2 ());
    return scm1 ();
  }
  static Super *unchecked_unsmob (SCM s)
  {
    return reinterpret_cast<Super *> (SCM_UNPACK (s));
  }
};

template <class Super> class Smob3 : public Smob_base<Super>
{
  Smob3 () = delete;
  Smob3 (const Smob3 &) = delete;
  Smob3 operator= (const Smob3 &) = delete;

public:
  SCM self_scm () const { return SCM_PACK (this); }
  SCM &scm1 () const { return *SCM_SMOB_OBJECT_LOC (self_scm ()); }
  SCM &scm2 () const { return *SCM_SMOB_OBJECT_2_LOC (self_scm ()); }
  SCM &scm3 () const { return *SCM_SMOB_OBJECT_3_LOC (self_scm ()); }
  static SCM make_smob (SCM arg1 = SCM_UNDEFINED, SCM arg2 = SCM_UNDEFINED,
                        SCM arg3 = SCM_UNDEFINED)
  {
    SCM_RETURN_NEWSMOB3 (Smob_base<Super>::smob_tag (), SCM_UNPACK (arg1),
                         SCM_UNPACK (arg2), SCM_UNPACK (arg3));
  }
  SCM mark_smob () const
  {
    scm_gc_mark (scm3 ());
    scm_gc_mark (scm2 ());
    return scm1 ();
  }
  static Super *unchecked_unsmob (SCM s)
  {
    return reinterpret_cast<Super *> (SCM_UNPACK (s));
  }
};

#endif

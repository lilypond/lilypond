/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef SMOBS_HH
#define SMOBS_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "warn.hh"
#include <string>

/*
  Smobs are GUILEs mechanism of exporting C(++) objects to the Scheme
  world.  They are documented in the GUILE manual.


  In LilyPond, C++ objects can be placed under the control of GUILE's
  type system and garbage collection mechanism by inheriting from one
  of several Smob base classes.

  There are two types of smob objects.

  1. Simple smobs are intended for simple objects like numbers:
  immutable objects that can be copied without change of meaning.

  To obtain an SCM version of a simple smob, use the member function
  SCM smobbed_copy ().

  Simple smobs are created by deriving from Simple_smob<Classname>.

  A simple smob is only optionally under the reign of the GUILE
  garbage collector: its usual life time is that of a normal C++
  object.  While a smobbed_copy () is fully under control of the
  garbage collector and will have its mark_smob function called during
  garbage collection, an automatic variable of this type will not have
  mark_smob called, but rather have its memory image in the call stack
  scanned for contained non-immediate SCM values.  Anything requiring
  more complex mark_smob behavior is not suitable for a simple smob.

  When you create a smobbed_copy, the _copy_ is fully managed by the
  GUILE memory system.  As a corollary, multiple smobbed_copy calls
  yield multiple GUILE objects generally not eq? to each other.

  2. Complex smobs are objects that have an identity. These objects
  carry this identity in the form of a self_scm () method, which is a
  SCM pointer to the object itself.  Complex smobs are always under
  control of the GUILE memory system.

  The constructor for a complex smob should have 3 steps:

  * initialize all SCM members to an immediate value (like SCM_EOL)

  * call smobify_self ()

  * initialize SCM members

  For example,

  Complex_smob::Complex_smob : public Smob<Complex_smob> () {
  scm_member_ = SCM_EOL;
  smobify_self ();
  scm_member_ = <..what you want to store..>
  }

  after construction, the self_scm () field of a complex smob is
  protected from Garbage Collection.  This protection should be
  removed once the object is put into another (reachable) Scheme data
  structure, i.e.

  Complex_smob *p = new Complex_smob;
  list = scm_cons (p->self_scm (), list);
  p->unprotect ();

  Since unprotect returns the SCM object itself, this particular case
  can be written as

  Complex_smob *p = new Complex_smob;
  list = scm_cons (p->unprotect (), list);

  Complex smobs are created by deriving from Smob<Classname>.

  CALLING INTERFACE

  Common global functions for accessing C++ smob objects:

  - unsmob<T> (SCM x) - unpack X and return a pointer to the C++ object,
    or 0 if it has the wrong type.

  IMPLEMENTATION

  For implementating a class, the following public members can be
  provided in the top class itself:

  - SCM equal_p (SCM a, SCM b) - compare A and B. Returns a Scheme
    boolean.  If the class does not define this function, equal? will
    be equivalent to eq?.  The function will only be called when both
    objects are of the respective type and not eq? to each other.

  - mark_smob () function, that calls scm_gc_mark () on all Scheme
    objects in the class.  If the class does not define this function,
    it must not contain non-immediate Scheme values.

  - a print_smob () function, that displays a representation for
    debugging purposes.  If the class does not define this function,
    the output will be #<Classname> when printing.

  - a static const type_p_name_[] string set to something like
    "ly:grob?".  When provided, an accordingly named function for
    checking for the given smob type will be available in Scheme.

*/

// Initialization class.  Create a variable or static data member of
// this type at global scope (or creation will happen too late for
// Scheme initialization), initialising with a function to be called.
// Reference somewhere (like in the constructor of the containing
// class) to make sure the variable is actually instantiated.

class Scm_init {
  static const Scm_init * list_;
  void (*const fun_)(void);
  Scm_init const * const next_;
  Scm_init ();          // don't use default constructor, don't define
  Scm_init (const Scm_init &);  // don't define copy constructor
public:
  Scm_init (void (*fun) (void)) : fun_ (fun), next_ (list_)
  { list_ = this; }
  static void init ();
};

template <class Super>
class Smob_base
{
  static scm_t_bits smob_tag_;
  static Scm_init scm_init_;
  static void init (void);
  static string smob_name_;
protected:
  static Super *unchecked_unsmob (SCM s)
  {
    return reinterpret_cast<Super *> (SCM_SMOB_DATA (s));
  }
  // reference scm_init_ in smob_tag which is sure to be called.  The
  // constructor, in contrast, may not be called at all in classes
  // like Smob1.
  static scm_t_bits smob_tag () { (void) scm_init_; return smob_tag_; }
  Smob_base () { }
  static SCM register_ptr (Super *p);
  static Super *unregister_ptr (SCM obj);
private:
  // Those fallbacks are _only_ for internal use by Smob_base.  They
  // are characterized by no knowledge about the implemented type
  // apart from the type's name.  Overriding them as a template
  // specialization is _not_ intended since a type-dependent
  // implementation will in general need access to possibly private
  // parts of the Super class.  So any class-dependent override should
  // be done by redefining the respective function in the Super class
  // (where it will mask the private template member) rather than
  // specializing a different template function/pointer.
  //
  // Most default functions are do-nothings.  void init() will
  // recognize their address when not overriden and will then refrain
  // altogether from passing the the respective callbacks to GUILE.
  SCM mark_smob (void) const;
  static SCM mark_trampoline (SCM); // Used for calling mark_smob
  static size_t free_smob (SCM obj);
  static SCM equal_p (SCM, SCM);
  int print_smob (SCM, scm_print_state *) const;
  static int print_trampoline (SCM, SCM, scm_print_state *);
  static void smob_proc_init (scm_t_bits) { };

  // type_p_name_ has to be defined in the Super class, either with a
  // static const char [] string or as a null pointer of type const
  // char *.  We used to provide a default here for convenience, but
  // battling the various conflicting C++ standards was too much of a
  // hassle.

  // LY_DECLARE_SMOB_PROC is used in the Super class definition for
  // making a smob callable like a function.  Its first argument is a
  // function member pointer constant, to a function taking the
  // correct number of SCM arguments and returning SCM.  The function
  // itself has to be defined separately.

#define LY_DECLARE_SMOB_PROC(PMF, REQ, OPT, VAR)                        \
  static void smob_proc_init (scm_t_bits smob_tag)                      \
  {                                                                     \
    scm_set_smob_apply (smob_tag,                                       \
                        (scm_t_subr)smob_trampoline<PMF>,               \
                        REQ, OPT, VAR);                                 \
  }

  // Well, function template argument packs are a C++11 feature.  So
  // we just define a bunch of trampolines manually.  It turns out
  // that GUILE 1.8.8 cannot actually make callable structures with
  // more than 3 arguments anyway.  That's surprising, to say the
  // least, but in emergency situations one can always use a "rest"
  // argument and take it apart manually.

  template <SCM (Super::*pmf)(void)>
  static SCM smob_trampoline (SCM self)
  {
    return (Super::unchecked_unsmob (self)->*pmf)();
  }
  template <SCM (Super::*pmf)(SCM)>
  static SCM smob_trampoline (SCM self, SCM arg1)
  {
    return (Super::unchecked_unsmob (self)->*pmf)(arg1);
  }
  template <SCM (Super::*pmf)(SCM, SCM)>
  static SCM smob_trampoline (SCM self, SCM arg1, SCM arg2)
  {
    return (Super::unchecked_unsmob (self)->*pmf)(arg1, arg2);
  }
  template <SCM (Super::*pmf)(SCM, SCM, SCM)>
  static SCM smob_trampoline (SCM self, SCM arg1, SCM arg2, SCM arg3)
  {
    return (Super::unchecked_unsmob (self)->*pmf)(arg1, arg2, arg3);
  }

  static bool is_smob (SCM s)
  {
    return SCM_SMOB_PREDICATE (smob_tag (), s);
  }
  static SCM smob_p (SCM s)
  {
    return is_smob (s) ? SCM_BOOL_T : SCM_BOOL_F;
  }

  template <class T>
  friend T *unsmob (SCM s);

  template <class T>
  friend T *ly_assert_smob (SCM s, int number, const char *fun);
};

template <class T>
inline T *unsmob (SCM s)
{
  return T::is_smob (s) ? dynamic_cast<T *> (T::unchecked_unsmob (s)) : 0;
}

// Simple smobs
template <class Super>
class Simple_smob : public Smob_base<Super> {
public:
  static size_t free_smob (SCM obj)
  {
    delete Smob_base<Super>::unregister_ptr (obj);
    return 0;
  }
  SCM smobbed_copy () const
  {
    Super *p = new Super(*static_cast<const Super *> (this));
    return Smob_base<Super>::register_ptr (p);
  }
};

void protect_smob (SCM smob, SCM *prot_cons);
void unprotect_smob (SCM smob, SCM *prot_cons);

// The Smob_core class is not templated and contains material not
// depending on the Super class.

class Smob_core {
protected:
  SCM self_scm_;
  Smob_core () : self_scm_ (SCM_UNDEFINED) { };
public:
  SCM self_scm () const { return self_scm_; }
  Listener get_listener (SCM callback);
};

template <class Super>
class Smob : public Smob_core, public Smob_base<Super> {
private:
  SCM protection_cons_;
  Smob (const Smob<Super> &); // Do not define!  Not copyable!
protected:
  Smob () : protection_cons_ (SCM_EOL) { };
public:
  static size_t free_smob (SCM obj)
  {
    delete Smob_base<Super>::unregister_ptr (obj);
    return 0;
  }
  SCM unprotected_smobify_self ()
  {
    SCM s = Smob_base<Super>::register_ptr (static_cast<Super *> (this));
    self_scm_ = s;
    return s;
  }
  void protect ()
  {
    protect_smob (self_scm_, &protection_cons_);
  }
  void smobify_self () {
    protect_smob (unprotected_smobify_self (), &protection_cons_);
  }
  SCM unprotect ()
  {
    SCM s = self_scm_;
    unprotect_smob (s, &protection_cons_);
    return s;
  }
};

extern bool parsed_objects_should_be_dead;
class parsed_dead
{
  static vector<parsed_dead *> elements;
  SCM data;
  SCM readout_one ()
  {
    SCM res = data;
    data = SCM_UNDEFINED;
    return res;
  }
public:
  parsed_dead () : data (SCM_UNDEFINED)
  {
    elements.push_back (this);
  }
  void checkin (SCM arg) { data = arg; }
  static SCM readout ();
};

// This does not appear to work with GUILEv2's garbage collector:
// Objects are found in the GC phase but printing them will crash at
// least some, so they are apparently not protected in spite of being
// included in the GC scans.  So it would appear that scanning smobs
// is not equivalent to marking them.  Ugh.
#if defined(DEBUG) && !GUILEV2
#define ASSERT_LIVE_IS_ALLOWED(arg)                                     \
  do {                                                                  \
    static parsed_dead pass_here;                                       \
    if (parsed_objects_should_be_dead)                                  \
      pass_here.checkin (arg);                                          \
  } while (0)
#else
#define ASSERT_LIVE_IS_ALLOWED(arg) do { (void)(arg); }  \
  while (0)
#endif

#include "smobs.tcc"
#endif /* SMOBS_HH */

/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2014 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

  Common public methods to C++ smob objects:

  - unsmob (SCM x) - unpacks X and returns pointer to the C++ object,
    or 0 if it has the wrong type.  This can be used as a boolean
    condition at C++ level.
  - smob_p (SCM x) returns #t or #f at Scheme level.

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
public:
  Scm_init () { }
  Scm_init (void (*fun) (void))
  {
    add_scm_init_func (fun);
  }
};

template <class Super>
class Smob_base
{
  static scm_t_bits smob_tag_;
  static Scm_init scm_init_;
  static void init (void);
  static string smob_name_;
  static Super *unchecked_unsmob (SCM s)
  {
    return reinterpret_cast<Super *> (SCM_SMOB_DATA (s));
  }
protected:
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
  SCM mark_smob (void);
  static SCM mark_trampoline (SCM); // Used for calling mark_smob
  static size_t free_smob (SCM obj);
  static SCM equal_p (SCM, SCM);
  int print_smob (SCM, scm_print_state *);
  static int print_trampoline (SCM, SCM, scm_print_state *);

  // type_p_name_ can be overriden in the Super class with a static
  // const char [] string.  This requires both a declaration in the
  // class as well as a single instantiation outside.  Using a
  // template specialization for supplying a different string name
  // right in Smob_base<Super> itself seems tempting, but the C++
  // rules would then require a specialization declaration at the
  // class definition site as well as a specialization instantiation
  // in a single compilation unit.  That requires just as much source
  // code maintenance while being harder to understand and quite
  // trickier in its failure symptoms when things go wrong.  So we
  // just use a static zero as "not here" indication.
  static const int type_p_name_ = 0;

  // LY_DECLARE_SMOB_PROC is used in the Super class definition for
  // making a smob callable like a function.  Declaration has to be
  // public.  It may be either be completed with a semicolon in which
  // case a definition of the member function smob_proc has to be done
  // outside of the class body, or the semicolon is left off and an
  // inline function body is added immediately below.  It would be
  // nice if this were a non-static member function but it would seem
  // tricky to do the required trampolining for unsmobbing the first
  // argument of the callback and using it as a this pointer.
#define LY_DECLARE_SMOB_PROC(REQ, OPT, VAR, ARGLIST)                    \
  static const int smob_proc_signature_ = ((REQ)<<8)|((OPT)<<4)|(VAR);  \
  static SCM smob_proc ARGLIST

  // a separate LY_DEFINE_SMOB_PROC seems sort of pointless as it
  // would just result in SCM CLASS::smob_proc ARGLIST
  //
  // The default case without function functionality is recognized by
  // smob_proc_signature being -1.
  static const int smob_proc = 0;
  static const int smob_proc_signature_ = -1;

public:
  static bool is_smob (SCM s)
  {
    return SCM_SMOB_PREDICATE (smob_tag (), s);
  }
  static SCM smob_p (SCM s)
  {
    return is_smob (s) ? SCM_BOOL_T : SCM_BOOL_F;
  }
  static Super *unsmob (SCM s)
  {
    return is_smob (s) ? Super::unchecked_unsmob (s) : 0;
  }
};

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

template <class Super>
class Smob : public Smob_base<Super> {
private:
  SCM self_scm_;
  SCM protection_cons_;
  Smob (const Smob<Super> &); // Do not define!  Not copyable!
protected:
  Smob () : self_scm_ (SCM_UNDEFINED), protection_cons_ (SCM_EOL) { };
public:
  static size_t free_smob (SCM obj)
  {
    delete Smob_base<Super>::unregister_ptr (obj);
    return 0;
  }
  SCM unprotected_smobify_self ()
  {
    self_scm_ = Smob_base<Super>::register_ptr (static_cast<Super *> (this));
    return self_scm_;
  }
  void protect ()
  {
    protect_smob (self_scm_, &protection_cons_);
  }
  SCM unprotect ()
  {
    unprotect_smob (self_scm_, &protection_cons_);
    return self_scm_;
  }
  void smobify_self () {
    self_scm_ = unprotected_smobify_self ();
    protect ();
  }
  SCM self_scm () const { return self_scm_; }
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
#if !defined(NDEBUG) && !GUILEV2
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

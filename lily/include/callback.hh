/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016--2020 David Kastrup <dak@gnu.org>

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

#ifndef CALLBACK_HH
#define CALLBACK_HH

// A callback wrapper creates a Scheme-callable version of a fixed C++
// function.  It is generally used for calling template-generated
// trampoline functions leading to calling a particular member
// function on a given Smob.
//
// The class itself is not templated in order not to explode the
// number of smob types: each class can support a particular call
// signature.
//
// Check the GET_LISTENER call for a typical use case.

#include "smobs.hh"

class Callback_wrapper : public Simple_smob<Callback_wrapper>
{
  // We use an ordinary function pointer pointing to a trampoline
  // function (templated on the callback in question) instead of
  // storing a member function pointer to a common base class like
  // Smob_core.  The additional code for the trampolines is negligible
  // and the performance implications of using member function
  // pointers in connection with inheritance are somewhat opaque as
  // this involves an adjustment of the this pointer from Smob_core to
  // the scope containing the callback.
  SCM (*trampoline_) (SCM, SCM);
  Callback_wrapper (SCM (*trampoline) (SCM, SCM))
      : trampoline_ (trampoline)
  {} // Private constructor, use only in make_smob
public:
  LY_DECLARE_SMOB_PROC (&Callback_wrapper::call, 2, 0, 0)
  SCM call (SCM target, SCM arg) { return trampoline_ (target, arg); }
  // Callback wrappers are for an unchanging entity, so we do the Lisp
  // creation just once on the first call of make_smob.  So we only
  // get a single Callback_wrapper instance for each differently
  // templated make_smob call.
  template <SCM (*trampoline) (SCM, SCM)> static SCM make_smob ()
  {
    static SCM res
        = scm_permanent_object (Callback_wrapper (trampoline).smobbed_copy ());
    return res;
  }
};

class Callback2_wrapper : public Simple_smob<Callback2_wrapper>
{
  // See Callback_wrapper for the details.  Callback2_wrapper just
  // supports an additional SCM argument as compared to
  // Callback_wrapper but is otherwise identical.
  SCM (*trampoline_) (SCM, SCM, SCM);
  Callback2_wrapper (SCM (*trampoline) (SCM, SCM, SCM))
      : trampoline_ (trampoline)
  {
  } // Private constructor, use only in make_smob
public:
  LY_DECLARE_SMOB_PROC (&Callback2_wrapper::call, 3, 0, 0)
  SCM call (SCM target, SCM arg1, SCM arg2)
  {
    return trampoline_ (target, arg1, arg2);
  }

  template <SCM (*trampoline) (SCM, SCM, SCM)> static SCM make_smob ()
  {
    static SCM res
        = scm_permanent_object (Callback2_wrapper (trampoline).smobbed_copy ());
    return res;
  }
};

class Callback0_wrapper : public Simple_smob<Callback0_wrapper>
{
  // See Callback_wrapper for the details.  Callback0_wrapper does not
  // pass arguments but is otherwise identical to Callback_wrapper.
  SCM (*trampoline_) (SCM);
  Callback0_wrapper (SCM (*trampoline) (SCM))
      : trampoline_ (trampoline)
  {} // Private constructor, use only in make_smob
public:
  LY_DECLARE_SMOB_PROC (&Callback0_wrapper::call, 1, 0, 0)
  SCM call (SCM target) { return trampoline_ (target); }

  template <SCM (*trampoline) (SCM)> static SCM make_smob ()
  {
    static SCM res
        = scm_permanent_object (Callback0_wrapper (trampoline).smobbed_copy ());
    return res;
  }
  // Since there are no arguments at all, we might as well provide
  // default trampolines
  template <class T, SCM (T::*p) ()> static SCM trampoline (SCM target)
  {
    T *t = LY_ASSERT_SMOB (T, target, 1);
    return (t->*p) ();
  }

  template <class T, void (T::*p) ()> static SCM trampoline (SCM target)
  {
    T *t = LY_ASSERT_SMOB (T, target, 1);
    (t->*p) ();
    return SCM_UNSPECIFIED;
  }

  template <class T, SCM (T::*p) ()> static SCM make_smob ()
  {
    return make_smob<trampoline<T, p>> ();
  }

  template <class T, void (T::*p) ()> static SCM make_smob ()
  {
    return make_smob<trampoline<T, p>> ();
  }
};

// The following will usually be used unsmobbified, relying on its
// constituents being protected independently.

class Method_instance : public Simple_smob<Method_instance>
{
  SCM method_, instance_;

public:
  LY_DECLARE_SMOB_PROC (&Method_instance::call, 0, 0, 1)
  SCM call (SCM rest) { return scm_apply_1 (method_, instance_, rest); }

  Method_instance (SCM method, SCM instance)
      : method_ (method), instance_ (instance)
  {
  }
  Method_instance (SCM method, Smob_core *instance)
      : method_ (method), instance_ (instance->self_scm ())
  {
  }
  SCM method () const { return method_; }
  SCM instance () const { return instance_; }
  SCM operator() () const { return scm_call_1 (method_, instance_); }
  SCM operator() (SCM arg) const
  {
    return scm_call_2 (method_, instance_, arg);
  }
  SCM operator() (SCM arg1, SCM arg2) const
  {
    return scm_call_3 (method_, instance_, arg1, arg2);
  }
  SCM mark_smob () const
  {
    scm_gc_mark (method_);
    return instance_;
  }
};

#endif

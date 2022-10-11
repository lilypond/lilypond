/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016--2022 David Kastrup <dak@gnu.org>

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

#include "grob-info.hh"
#include "lily-proto.hh"
#include "smobs.hh"

#include <utility>

// Templated work class to Callbacks

template <typename... Args>
class Callback_wrapper : public Simple_smob<Callback_wrapper<Args...>>
{
  // We use an ordinary function pointer pointing to a trampoline
  // function (templated on the callback in question) instead of
  // storing a member function pointer to a common base class like
  // Smob_core.  The additional code for the trampolines is negligible
  // and the performance implications of using member function
  // pointers in connection with inheritance are somewhat opaque as
  // this involves an adjustment of the this pointer from Smob_core to
  // the scope containing the callback.
  SCM (*trampoline_) (SCM, Args...);
  Callback_wrapper (SCM (*trampoline) (SCM, Args...))
    : trampoline_ (trampoline)
  {
  } // Private constructor, use only in make_smob
public:
  LY_DECLARE_STATIC_SMOB_PROC (call, sizeof...(Args) + 1, 0, 0)
  static SCM call (SCM self, SCM target, Args... args)
  {
    return (Callback_wrapper::unchecked_unsmob (self)->trampoline_) (target,
                                                                     args...);
  }

  // Callback wrappers are for an unchanging entity, so we do the Lisp
  // creation just once on the first call of make_smob.  So we only
  // get a single Callback_wrapper instance for each differently
  // templated make_smob call.
  template <SCM (*trampoline) (SCM, Args...)>
  static SCM make_smob ()
  {
    static SCM res
      = scm_permanent_object (Callback_wrapper (trampoline).smobbed_copy ());
    return res;
  }
};

class Callbacks
{
public:
  template <SCM (*trampoline) (SCM)>
  static SCM make_smob ()
  {
    return Callback_wrapper<>::make_smob<trampoline> ();
  }
  template <SCM (*trampoline) (SCM, SCM)>
  static SCM make_smob ()
  {
    return Callback_wrapper<SCM>::make_smob<trampoline> ();
  }
  template <SCM (*trampoline) (SCM, SCM, SCM)>
  static SCM make_smob ()
  {
    return Callback_wrapper<SCM, SCM>::make_smob<trampoline> ();
  }

  // Provide a number of trampolines, first basic no-argument ones
  template <class T, SCM (T::*p) ()>
  static SCM trampoline (SCM target)
  {
    auto *const t = LY_ASSERT_SMOB (T, target, 1);
    return (t->*p) ();
  }

  // This version with a return value could sensibly be const
  template <class T, SCM (T::*p) () const>
  static SCM trampoline (SCM target)
  {
    auto *const t = LY_ASSERT_SMOB (T, target, 1);
    return (t->*p) ();
  }

  template <class T, void (T::*p) ()>
  static SCM trampoline (SCM target)
  {
    auto *const t = LY_ASSERT_SMOB (T, target, 1);
    (t->*p) ();
    return SCM_UNSPECIFIED;
  }

  // Single SCM argument, like used in Listener
  template <class T, void (T::*callback) (SCM)>
  static SCM trampoline (SCM target, SCM ev)
  {
    auto *const t = LY_ASSERT_SMOB (T, target, 1);

    (t->*callback) (ev);
    return SCM_UNSPECIFIED;
  }

  // The more complex trampolines are defined near their use cases.

  // Stream_event argument is used in Translators

  template <class T, void (T::*p) (Stream_event *)>
  static SCM trampoline (SCM target, SCM ev);

  template <class Target, class Owner, class Delegate,
            Delegate Owner::*delegate,
            void (Delegate::*method) (Stream_event *) = &Delegate::operator()>
  static SCM trampoline (SCM owner, SCM ev);

  // Acknowledger trampolines
  template <class T, void (T::*callback) (Grob_info_t<Grob>)>
  static SCM trampoline (SCM target, SCM grob, SCM source_engraver);

  template <class T, void (T::*callback) (Grob_info_t<Item>)>
  static SCM trampoline (SCM target, SCM item, SCM source_engraver);

  template <class T, void (T::*callback) (Grob_info_t<Spanner>)>
  static SCM trampoline (SCM target, SCM spanner, SCM source_engraver);
};

// This duplicates std::remove_pointer (apart from erroring out if
// there is no pointer to be removed) but it's simple enough that we
// don't want to pull in all of <type_traits> for a header as
// frequently included as this one.
template <typename U>
struct ly_remove_pointer; // Template, no default
template <typename U>
struct ly_remove_pointer<U *>
{
  using type = U;
};
template <typename U>
struct ly_remove_pointer<const U *>
{
  using type = U;
};

// Tool class for member function pointer base class identification in
// spirit akin to the <type_traits> include file classes.
//
// If T is a member function pointer type, mfp_baseclass<T>::type is
// the type of its underlying base class.

template <typename T>
class mfp_baseclass
{
  // We cannot make the return type U since it can be an abstract base class
  template <typename U, typename V, typename... W>
  static U *strip_mfp (V (U::*) (W...));
  template <typename U, typename V, typename... W>
  static U *strip_mfp (V (U::*) (W...) const);

public:
  using type = typename ly_remove_pointer<decltype (strip_mfp (
    static_cast<T> (nullptr)))>::type;
};

// Tool class for member object pointer base class identification in
// spirit akin to the <type_traits> include file classes.
//
// If T is a member object pointer type, mop_baseclass<T>::type is
// the type of its underlying base class.

template <typename T>
class mop_baseclass
{
  // We cannot make the return type U since it can be an abstract base class
  template <typename U, typename V>
  static U *strip_mop (V U::*);

public:
  using type = typename ly_remove_pointer<decltype (strip_mop (
    static_cast<T> (nullptr)))>::type;
};

// Build a member function pointer given a pointer to the class and
// the unqualified name of a member function
#define MFP_CREATE(ptr, proc) (&ly_remove_pointer<decltype (ptr)>::type::proc)

// Split a constant member function pointer into a pair of template
// arguments, the first being the underlying base class type, the
// second being the member function pointer itself: this is frequently
// needed for template resolution

#define MFP_ARGS(mfp) typename mfp_baseclass<decltype (mfp)>::type, mfp

// Wrapper macro for member function pointers

#define MFP_WRAP(mfp)                                                          \
  Callbacks::make_smob<Callbacks::trampoline<MFP_ARGS (mfp)>> ()

// Wrap a member object's member function.  The resulting procedure expects to
// be called with the owning object as the first argument, and it will forward
// the remaining arguments to the member object's method.
//
// The member object pointed to by `&target::delegate` may be inherited from a
// base class, which we call the "owner" in this context.
//
// The function to be called is `delegate.method ()`.
#define MOMF_WRAP(target, delegate, method)                                    \
  [] () {                                                                      \
    using Delegate = decltype (std::declval<target> ().delegate);              \
    using Owner = typename mop_baseclass<decltype (&target::delegate)>::type;  \
    return Callbacks::make_smob<Callbacks::trampoline<                         \
      target, Owner, Delegate, &target::delegate, &Delegate::method>> ();      \
  }()

// The following will usually be used unsmobbified, relying on its
// constituents being protected independently.

class Method_instance : public Simple_smob<Method_instance>
{
  SCM method_, instance_;

public:
  LY_DECLARE_SMOB_PROC (&Method_instance::call, 0, 0, 1)
  SCM call (SCM rest) { return scm_apply_1 (method_, instance_, rest); }

  Method_instance (SCM method, SCM instance)
    : method_ (method),
      instance_ (instance)
  {
  }
  Method_instance (SCM method, Smob_core *instance)
    : method_ (method),
      instance_ (instance->self_scm ())
  {
  }
  SCM method () const { return method_; }
  SCM instance () const { return instance_; }
  template <typename... Args>
  SCM operator() (Args &&...args) const
  {
    return ly_call (method_, instance_, std::forward<Args> (args)...);
  }
  SCM mark_smob () const
  {
    scm_gc_mark (method_);
    return instance_;
  }
};

#endif

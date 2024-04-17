/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016--2023 David Kastrup <dak@gnu.org>

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

#include <type_traits>
#include <utility>

// Templated work class to Callbacks

template <typename Result, typename... Args>
class Callback_wrapper : public Simple_smob<Callback_wrapper<Result, Args...>>
{
  // force_scm_t<Args>... converts Args... to the same number of SCM
  template <typename>
  using force_scm_t = SCM;

  // We use an ordinary function pointer pointing to a trampoline
  // function (templated on the callback in question) instead of
  // storing a member function pointer to a common base class like
  // Smob_core.  The additional code for the trampolines is negligible
  // and the performance implications of using member function
  // pointers in connection with inheritance are somewhat opaque as
  // this involves an adjustment of the this pointer from Smob_core to
  // the scope containing the callback.
  Result (*trampoline_) (force_scm_t<Args>...);

  // Writing force_scm_t<Args>... here would interfere with class template
  // argument deduction; however, all arguments must be SCM for the
  // initialization of trampoline_ to compile; therefore, we have lost nothing.
  Callback_wrapper (Result (*trampoline) (Args...))
    : trampoline_ (trampoline)
  {
  } // Private constructor, use only in get_callback_wrapper_smob
public:
  LY_DECLARE_STATIC_SMOB_PROC (call, sizeof...(Args), 0, 0)
  static SCM call (SCM self, force_scm_t<Args>... args)
  {
    if constexpr (std::is_same_v<Result, void>)
      {
        Callback_wrapper::unchecked_unsmob (self)->trampoline_ (args...);
        return SCM_UNSPECIFIED;
      }
    else
      {
        return Callback_wrapper::unchecked_unsmob (self)->trampoline_ (args...);
      }
  }

  template <auto>
  friend SCM get_callback_wrapper_smob ();
};

// Callback wrappers are for an unchanging entity, so we do the Lisp creation
// just once on the first call of get_callback_wrapper_smob.  So we get a single
// Callback_wrapper instance per trampoline function.
template <auto trampoline>
inline SCM
get_callback_wrapper_smob ()
{
  static SCM res
    = scm_permanent_object (Callback_wrapper (trampoline).smobbed_copy ());
  return res;
}

class Callbacks
{
public:
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
  static void trampoline (SCM target)
  {
    auto *const t = LY_ASSERT_SMOB (T, target, 1);
    (t->*p) ();
  }

  // Single SCM argument, like used in Listener
  template <class T, void (T::*callback) (SCM)>
  static void trampoline (SCM target, SCM ev)
  {
    auto *const t = LY_ASSERT_SMOB (T, target, 1);

    (t->*callback) (ev);
  }

  // The more complex trampolines are defined near their use cases.

  // Stream_event argument is used in Translators

  template <class T, void (T::*p) (Stream_event *)>
  static void trampoline (SCM target, SCM ev);

  template <class Target, class Owner, class Delegate,
            Delegate Owner::*delegate,
            void (Delegate::*method) (Stream_event *) = &Delegate::operator()>
  static void trampoline (SCM owner, SCM ev);

  // Acknowledger trampolines
  template <class T, void (T::*callback) (Grob_info_t<Grob>)>
  static void trampoline (SCM target, SCM grob, SCM source_engraver);

  template <class T, void (T::*callback) (Grob_info_t<Item>)>
  static void trampoline (SCM target, SCM item, SCM source_engraver);

  template <class T, void (T::*callback) (Grob_info_t<Spanner>)>
  static void trampoline (SCM target, SCM spanner, SCM source_engraver);
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
  using type = typename std::remove_pointer_t<decltype (strip_mfp (
    static_cast<T> (nullptr)))>;
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
  using type = typename std::remove_pointer_t<decltype (strip_mop (
    static_cast<T> (nullptr)))>;
};

// Build a member function pointer given a pointer to the class and
// the unqualified name of a member function
#define MFP_CREATE(ptr, proc) (&std::remove_pointer_t<decltype (ptr)>::proc)

// Split a constant member function pointer into a pair of template
// arguments, the first being the underlying base class type, the
// second being the member function pointer itself: this is frequently
// needed for template resolution

#define MFP_ARGS(mfp) typename mfp_baseclass<decltype (mfp)>::type, mfp

// Wrapper macro for member function pointers

#define MFP_WRAP(mfp)                                                          \
  get_callback_wrapper_smob<Callbacks::trampoline<MFP_ARGS (mfp)>> ()

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
    return get_callback_wrapper_smob<Callbacks::trampoline<                    \
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

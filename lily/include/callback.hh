/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016  David Kastrup <dak@gnu.org>

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
  Callback_wrapper (SCM (*trampoline) (SCM, SCM)) : trampoline_ (trampoline)
  { } // Private constructor, use only in make_smob
public:
  static const char * const type_p_name_; // = 0
  LY_DECLARE_SMOB_PROC (&Callback_wrapper::call, 2, 0, 0)
  SCM call (SCM target, SCM arg)
  {
    return trampoline_ (target, arg);
  }
  // Callback wrappers are for an unchanging entity, so we do the Lisp
  // creation just once on the first call of make_smob.  So we only
  // get a single Callback_wrapper instance for each differently
  // templated make_smob call.
  template <SCM (*trampoline) (SCM, SCM)>
  static SCM make_smob ()
  {
    static SCM res =
      scm_permanent_object (Callback_wrapper (trampoline).smobbed_copy ());
    return res;
  }
};


#endif

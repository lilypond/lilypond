/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2014--2022 David Kastrup <dak@gnu.org>

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

#ifndef SMALL_SMOBS_HH
#define SMALL_SMOBS_HH

#include "smobs.hh"

// This is tricky: the small smobs contain all the data in the smob
// itself.  Any derived classes must _not_ contain any data members or
// be polymorphic (contain a virtual table pointer) as there is no
// place to store any of that.  The class itself does not contain a
// data member either.  Pointers to it are basically meaningless and
// are instead reinterpreted directly as an SCM value in order to have
// unsmob and friends behave as customary.

template <class Super>
class Smob1 : public Smob_base<Super>
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

template <class Super>
class Smob2 : public Smob_base<Super>
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

template <class Super>
class Smob3 : public Smob_base<Super>
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

// This is the smallest smob of all: none.  It's just a wrapper around
// native SCM.  You need to define a proper is_smob function for it as
// a type checker.

template <class Super>
class Smob0
{
  Smob0 () = delete;
  Smob0 (const Smob0 &) = delete;
  Smob0 &operator= (const Smob0 &) = delete;

public:
  SCM self_scm () const { return SCM_PACK (this); }
  static Super *unchecked_unsmob (SCM s)
  {
    return reinterpret_cast<Super *> (SCM_UNPACK (s));
  }
};

#endif

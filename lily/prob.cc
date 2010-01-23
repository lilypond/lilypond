/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "prob.hh"
#include "main.hh"
#include "item.hh"
#include "input.hh"
#include "profile.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Prob);
IMPLEMENT_TYPE_P (Prob, "ly:prob?");

SCM
Prob::equal_p (SCM sa, SCM sb)
{
  /* This comparison function is only designed to make the copy
     constructor preserve equality.

     Perhaps it would be better to use a more strict definition of
     equality; e.g., that that two probs are equal iff they can be
     distinguished by calls to ly:prob-property.
  */
  Prob *probs[2] = {unsmob_prob (sa), unsmob_prob (sb)};
  SCM props[2][2];
  int i;

  for (i = 0; i < 2; i++)
    {
      props[i][0] = probs[i]->immutable_property_alist_;
      props[i][1] = probs[i]->mutable_property_alist_;
    }

  if (strcmp (probs[0]->class_name (), probs[1]->class_name ()))
    return SCM_BOOL_F;

  /* Compare mutable and immutable lists, element by element. */
  for (i = 0; i < 2; i++)
    {
      SCM aprop = props[0][i];
      SCM bprop = props[1][i];

      for (;
	   scm_is_pair (aprop) && scm_is_pair (bprop);
	   aprop = scm_cdr (aprop), bprop = scm_cdr (bprop))
	{
	  SCM aval = scm_cdar (aprop);
	  SCM bval = scm_cdar (bprop);
	  if (scm_caar (aprop) != scm_caar (bprop) ||
	      (
	       !(unsmob_input (aval) && unsmob_input (bval))
	       &&		 
	       !to_boolean (scm_equal_p (aval, bval))))
	    return SCM_BOOL_F;
	}

      /* is one list shorter? */
      if (aprop != SCM_EOL || bprop != SCM_EOL)
	return SCM_BOOL_F;
    }

  return SCM_BOOL_T;
}

Prob::Prob (SCM type, SCM immutable_init)
{
  self_scm_ = SCM_EOL;
  mutable_property_alist_ = SCM_EOL;
  immutable_property_alist_ = immutable_init;
  type_ = type;
  smobify_self ();
}


Prob::~Prob ()
{
}

Prob::Prob (Prob const &src)
{
  immutable_property_alist_ = src.immutable_property_alist_;
  mutable_property_alist_ = SCM_EOL;
  self_scm_ = SCM_EOL;
  type_ = src.type_;

  /* First we smobify_self, then we copy over the stuff.  If we don't,
     stack vars that hold the copy might be optimized away, meaning
     that they won't be protected from GC. */
  smobify_self ();
  mutable_property_alist_ = src.copy_mutable_properties ();
}


SCM
Prob::copy_mutable_properties () const
{
  return ly_deep_copy (mutable_property_alist_);
}

void
Prob::derived_mark () const
{
}

SCM
Prob::mark_smob (SCM smob)
{
  ASSERT_LIVE_IS_ALLOWED ();
  
  Prob *system = (Prob *) SCM_CELL_WORD_1 (smob);
  scm_gc_mark (system->mutable_property_alist_);
  system->derived_mark ();
  
  return system->immutable_property_alist_;
}

int
Prob::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Prob *p = (Prob *) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<", port);
  scm_puts ("Prob: ", port);
  scm_display (p->type_, port);
  scm_puts (" C++: ", port);
  scm_puts (p->class_name (), port);
  scm_display (p->mutable_property_alist_, port);
  scm_display (p->immutable_property_alist_, port);
  
  scm_puts (" >\n", port);
  return 1;
}



SCM
Prob::internal_get_property (SCM sym) const
{
#ifndef NDEBUG
  if (profile_property_accesses)
    note_property_access (&prob_property_lookup_table, sym);
#endif

  /*
    TODO: type checking
   */
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return scm_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : scm_cdr (s);
}

/* We don't (yet) instrument probs */
void
Prob::instrumented_set_property (SCM sym, SCM val, const char*, int, const char*)
{
  internal_set_property (sym, val);
}

void
Prob::internal_set_property (SCM sym, SCM val) 
{
  if (do_internal_type_checking_global)
    type_check_assignment (sym, val);
  
  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, sym, val);
}

void
Prob::type_check_assignment (SCM, SCM) const
{
  /* empty */
}

SCM
Prob::get_property_alist (bool m) const
{
  return (m) ? mutable_property_alist_ : immutable_property_alist_;
}

string
Prob::name () const
{
  SCM nm = get_property ("name");
  if (scm_is_symbol (nm))
    return ly_symbol2string (nm);
  else
    return this->class_name ();
}


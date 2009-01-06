/*
  grob-array.cc -- implement Grob_array

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob-array.hh"
#include "item.hh"
#include "spanner.hh"

#include "ly-smobs.icc"

Item *
Grob_array::item (vsize i)
{
  return dynamic_cast<Item *> (grobs_.at (i));
}

Spanner *
Grob_array::spanner (vsize i)
{
  return dynamic_cast<Spanner *> (grobs_.at (i));
}

Grob_array::Grob_array ()
{
  ordered_ = true;
}

vector<Grob*> &
Grob_array::array_reference ()
{
  return grobs_;
}

vector<Grob*> const &
Grob_array::array () const
{
  return grobs_;
}

SCM
Grob_array::mark_smob (SCM s)
{
  (void) s;

#if 0  /* see System::derived_mark () const */
  Grob_array *ga = unsmob_grob_array (s);
  for (vsize i = 0; i < ga->grobs_.size (); i++)
    scm_gc_mark (ga->grobs_[i]->self_scm ());
#endif
  return SCM_UNDEFINED;
}

int
Grob_array::print_smob (SCM arr, SCM port, scm_print_state*)
{
  scm_puts ("#<Grob_array", port);

  Grob_array *grob_arr = unsmob (arr);
  for (vsize i = 0; i < grob_arr->size (); i++)
    {
      scm_display (grob_arr->grob (i)->self_scm (), port);
      scm_puts (" ", port);
    }
  scm_puts (">", port);
  return 1;
}

SCM
Grob_array::make_array ()
{
  Grob_array ga;
  return ga.smobbed_copy ();
}

void
Grob_array::clear ()
{
  grobs_.clear ();
}

void
Grob_array::remove_duplicates ()
{
  assert (!ordered_);
  
  vector_sort (grobs_, less<Grob*> ());
  ::uniq (grobs_);
}

bool
Grob_array::empty () const
{
  return grobs_.empty ();
}

void
Grob_array::set_array (vector<Grob*> const &src)
{
  grobs_ = src;
}

IMPLEMENT_SIMPLE_SMOBS (Grob_array);
IMPLEMENT_TYPE_P (Grob_array, "ly:grob-array?");

IMPLEMENT_DEFAULT_EQUAL_P (Grob_array);

SCM
grob_list_to_grob_array (SCM lst)
{
  SCM arr_scm = Grob_array::make_array ();
  Grob_array *ga = unsmob_grob_array (arr_scm);
  for (SCM s = lst; scm_is_pair (s); s = scm_cdr (s))
    ga->add (unsmob_grob (scm_car (s)));
  return arr_scm;
}


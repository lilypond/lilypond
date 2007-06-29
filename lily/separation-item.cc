/*
  separation-item.cc -- implement Separation_item

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "separation-item.hh"

#include "axis-group-interface.hh"
#include "lookup.hh"
#include "stencil.hh"
#include "skyline.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"
#include "accidental-placement.hh"

void
Separation_item::add_item (Grob *s, Item *i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"), i);
}

void
Separation_item::add_conditional_item (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("conditional-elements"), e);
}

void
Separation_item::set_distance (Drul_array<Item *> items,
				       Real padding)
{
  Drul_array<Skyline_pair*> lines (Skyline_pair::unsmob (items[LEFT]->get_property ("horizontal-skylines")),
				   Skyline_pair::unsmob (items[RIGHT]->get_property ("horizontal-skylines")));
  Skyline right = conditional_skyline (items[RIGHT], items[LEFT]);
  right.merge ((*lines[RIGHT])[LEFT]);
  
  Real dist = padding + (*lines[LEFT])[RIGHT].distance (right);
  if (dist > 0)
    {
      Rod rod;

      rod.item_drul_ = items;

      rod.distance_ = dist;
      rod.add_to_cols ();
    }  
}

bool
Separation_item::is_empty (Grob *me)
{
  Skyline_pair *sky = Skyline_pair::unsmob (me->get_property ("horizontal-skylines"));
  return (!sky || sky->is_empty ());
}

/*
  Return the width of ME given that we are considering the object on
  the LEFT.
*/
Skyline
Separation_item::conditional_skyline (Grob *me, Grob *left)
{
  vector<Box> bs = boxes (me, left);
  return Skyline (bs, 0.1, Y_AXIS, LEFT);
}


MAKE_SCHEME_CALLBACK (Separation_item, calc_skylines,1);
SCM
Separation_item::calc_skylines (SCM smob)
{
  Item *me = unsmob_item (smob);
  vector<Box> bs = boxes (me, 0);
  /* todo: the horizon_padding is somewhat arbitrary */
  return Skyline_pair (bs, 0.1, Y_AXIS).smobbed_copy ();
}

/* if left is non-NULL, get the boxes corresponding to the
   conditional-elements (conditioned on the grob LEFT). This
   sounds more general than it is: conditional-elements are
   always accidentals attached to a tied note.
*/
vector<Box>
Separation_item::boxes (Grob *me, Grob *left)
{
  Item *item = dynamic_cast<Item *> (me);

  int very_large = INT_MAX;
  Paper_column *pc = item->get_column ();
  vector<Box> out;
  extract_grob_set (me, left ? "conditional-elements" : "elements", read_only_elts);
  vector<Grob*> elts;

  if (left)
    elts = Accidental_placement::get_relevant_accidentals (read_only_elts, left);
  else
    elts = read_only_elts;

  Grob *ycommon = common_refpoint_of_array (elts, me, Y_AXIS);
  
  for (vsize i = 0; i < elts.size (); i++)
    {
      Item *il = dynamic_cast<Item *> (elts[i]);
      if (pc != il->get_column ())
	continue;
      if (Axis_group_interface::has_interface (il))
	continue;

      Interval y (il->pure_height (ycommon, 0, very_large));
      Interval x (il->extent (pc, X_AXIS));

      Interval extra = robust_scm2interval (elts[i]->get_property ("extra-spacing-width"),
					    Interval (-0.1, 0.1));
      x[LEFT] += extra[LEFT];
      x[RIGHT] += extra[RIGHT];
      if (to_boolean (elts[i]->get_property ("infinite-spacing-height")))
	y = Interval (-infinity_f, infinity_f);
 
      if (!x.is_empty () && !y.is_empty ())
      out.push_back (Box (x, y));
    }

  return out;      
}

/*
  Try to find the break-aligned symbol in SEPARATION_ITEM that is
  sticking out at direction D. The x size is put in LAST_EXT
*/
Grob *
Separation_item::extremal_break_aligned_grob (Grob *me,
					      Direction d,
					      Interval *last_ext)
{
  Grob *col = dynamic_cast<Item *> (me)->get_column ();
  last_ext->set_empty ();
  Grob *last_grob = 0;

  extract_grob_set (me, "elements", elts);
  for (vsize i = elts.size (); i--;)
    {
      Grob *break_item = elts[i];
      if (!scm_is_symbol (break_item->get_property ("break-align-symbol")))
	continue;

      if (!scm_is_pair (break_item->get_property ("space-alist")))
	continue;

      Interval ext = break_item->extent (col, X_AXIS);

      if (ext.is_empty ())
	continue;

      if (!last_grob
	  || (last_grob && d * (ext[d]- (*last_ext)[d]) > 0))
	{
	  *last_ext = ext;
	  last_grob = break_item;
	}
    }

  return last_grob;
}

extern bool debug_skylines;
MAKE_SCHEME_CALLBACK (Separation_item, print, 1)
SCM
Separation_item::print (SCM smob)
{
  if (!debug_skylines)
    return SCM_BOOL_F;

  Grob *me = unsmob_grob (smob);
  Stencil ret;
  if (Skyline_pair *s = Skyline_pair::unsmob (me->get_property ("horizontal-skylines")))
    {
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[LEFT].to_points (Y_AXIS)).in_color (255, 255, 0));
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[RIGHT].to_points (Y_AXIS)).in_color (0, 255, 255));
    }
  return ret.smobbed_copy ();
}

ADD_INTERFACE (Separation_item,
	       "Item that computes widths to generate spacing rods. "
	       ,

	       "X-extent "
	       "conditional-elements "
	       "elements "
	       "padding "
	       "horizontal-skylines "
	       );

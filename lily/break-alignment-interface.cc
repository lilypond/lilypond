/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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


#include "break-align-interface.hh"

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "dimensions.hh"
#include "international.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "self-alignment-interface.hh"
#include "side-position-interface.hh"
#include "warn.hh"



/*
  This is tricky: we cannot modify 'elements, since callers are
  iterating the same list. Reordering the list in-place, or resetting
  'elements will skip elements in the loops of callers.

  So we return the correct order as an array.
*/
SCM
Break_alignment_interface::break_align_order (Item *me)
{
  SCM order_vec = me->get_property ("break-align-orders");
  if (!scm_is_vector (order_vec)
      || scm_c_vector_length (order_vec) < 3)
    return SCM_BOOL_F;

  SCM order = scm_vector_ref (order_vec,
			      scm_from_int (me->break_status_dir () + 1));


  return order;
}

  
vector<Grob*>
Break_alignment_interface::ordered_elements (Grob *grob)
{
  Item *me = dynamic_cast<Item *> (grob);
  extract_grob_set (me, "elements", elts);


  SCM order = break_align_order (me);

  if (order == SCM_BOOL_F)
    return elts;
  
  vector<Grob*> writable_elts (elts);
   /*
    Copy in order specified in BREAK-ALIGN-ORDER.
  */
  vector<Grob*> new_elts;
  for (; scm_is_pair (order); order = scm_cdr (order))
    {
      SCM sym = scm_car (order);

      for (vsize i = writable_elts.size (); i--;)
	{
	  Grob *g = writable_elts[i];
	  if (g && sym == g->get_property ("break-align-symbol"))
	    {
	      new_elts.push_back (g);
	      writable_elts.erase (writable_elts.begin () + i);
	    }
	}
    }

  return new_elts;
}

void
Break_alignment_interface::add_element (Grob *me, Grob *toadd)
{
  Align_interface::add_element (me, toadd);
}

MAKE_SCHEME_CALLBACK (Break_alignment_interface, calc_positioning_done, 1)
SCM
Break_alignment_interface::calc_positioning_done (SCM smob)
{
  Grob *grob = unsmob_grob (smob);  
  Item *me = dynamic_cast<Item *> (grob);


  me->set_property ("positioning-done", SCM_BOOL_T);

  vector<Grob*> elems = ordered_elements (me);
  vector<Interval> extents;

  int last_nonempty = -1;
  for (vsize i = 0; i < elems.size (); i++)
    {
      Interval y = elems[i]->extent (elems[i], X_AXIS);
      extents.push_back (y);
      if (!y.is_empty ())
	last_nonempty = i;
    }

  vsize idx = 0;
  while (idx < extents.size () && extents[idx].is_empty ())
    idx++;

  vector<Real> offsets;
  offsets.resize (elems.size ());
  for (vsize i = 0; i < offsets.size ();i++)
    offsets[i] = 0.0;

  Real extra_right_space = 0.0;
  vsize edge_idx = VPOS;
  while (idx < elems.size ())
    {
      vsize next_idx = idx + 1;
      while (next_idx < elems.size ()
	     && extents[next_idx].is_empty ())
	next_idx++;

      Grob *l = elems[idx];
      Grob *r = 0;

      if (next_idx < elems.size ())
	r = elems[next_idx];

      SCM alist = SCM_EOL;

      /*
	Find the first grob with a space-alist entry.
      */
      extract_grob_set (l, "elements", elts);

      for (vsize i = elts.size (); i--;)
	{
	  Grob *elt = elts[i];

	  if (edge_idx == VPOS
	      && (elt->get_property ("break-align-symbol")
		  == ly_symbol2scm ("left-edge")))
	    edge_idx = idx;

	  SCM l = elt->get_property ("space-alist");
	  if (scm_is_pair (l))
	    {
	      alist = l;
	      break;
	    }
	}

      SCM rsym = r ? SCM_EOL : ly_symbol2scm ("right-edge");

      /*
	We used to use #'cause to find out the symbol and the spacing
	table, but that gets icky when that grob is suicided for some
	reason.
      */
      if (r)
	{
	  extract_grob_set (r, "elements", elts);
	  for (vsize i = elts.size ();
	       !scm_is_symbol (rsym) && i--;)
	    {
	      Grob *elt = elts[i];
	      rsym = elt->get_property ("break-align-symbol");
	    }
	}

      if (rsym == ly_symbol2scm ("left-edge"))
	edge_idx = next_idx;

      SCM entry = SCM_EOL;
      if (scm_is_symbol (rsym))
	entry = scm_assq (rsym, alist);

      bool entry_found = scm_is_pair (entry);
      if (!entry_found)
	{
	  string sym_string;
	  if (scm_is_symbol (rsym))
	    sym_string = ly_symbol2string (rsym);

	  string orig_string;
	  if (unsmob_grob (l->get_property ("cause")))
	    orig_string = unsmob_grob (l->get_property ("cause"))->name ();

	  programming_error (_f ("No spacing entry from %s to `%s'",
				 orig_string.c_str (),
				 sym_string.c_str ()));
	}

      Real distance = 1.0;
      SCM type = ly_symbol2scm ("extra-space");

      if (entry_found)
	{
	  entry = scm_cdr (entry);

	  distance = scm_to_double (scm_cdr (entry));
	  type = scm_car (entry);
	}

      if (r)
	{
	  if (type == ly_symbol2scm ("extra-space"))
	    offsets[next_idx] = extents[idx][RIGHT] + distance
	      - extents[next_idx][LEFT];
	  /* should probably junk minimum-space */
	  else if (type == ly_symbol2scm ("minimum-space"))
	    offsets[next_idx] = max (extents[idx][RIGHT], distance);
	}
      else
	{
	  extra_right_space = distance;
	  if (idx + 1 < offsets.size ())
	    offsets[idx+1] = extents[idx][RIGHT] + distance;
	}

      idx = next_idx;
    }

  Real here = 0.0;
  Interval total_extent;

  Real alignment_off = 0.0;
  for (vsize i = 0; i < offsets.size (); i++)
    {
      here += offsets[i];
      if (i == edge_idx)
	alignment_off = -here;
      total_extent.unite (extents[i] + here);
    }

  if (total_extent.is_empty ())
    return SCM_BOOL_T;

  if (me->break_status_dir () == LEFT)
    alignment_off = -total_extent[RIGHT] - extra_right_space;
  else if (edge_idx == VPOS)
    alignment_off = -total_extent[LEFT];

  here = alignment_off;
  for (vsize i = 0; i < offsets.size (); i++)
    {
      here += offsets[i];
      elems[i]->translate_axis (here, X_AXIS);
    }

  return SCM_BOOL_T;
}



MAKE_SCHEME_CALLBACK (Break_alignable_interface, self_align_callback, 1)
SCM
Break_alignable_interface::self_align_callback (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  Item *alignment = dynamic_cast<Item*> (me->get_parent (X_AXIS));
  if (!Break_alignment_interface::has_interface (alignment))
    return scm_from_int (0);

  SCM symbol_list = me->get_property ("break-align-symbols");
  vector<Grob*> elements = Break_alignment_interface::ordered_elements (alignment);
  if (elements.size () == 0)
    return scm_from_int (0);
  
  int break_aligned_grob = -1;
  for (; scm_is_pair (symbol_list); symbol_list = scm_cdr (symbol_list))
    {
      SCM sym = scm_car (symbol_list);
      for (vsize i = 0; i < elements.size (); i++)
	{
	  if (elements[i]->get_property ("break-align-symbol") == sym)
	    {
	      if (Item::break_visible (elements[i])
		  && !elements[i]->extent (elements[i], X_AXIS).is_empty ())
		{
		  break_aligned_grob = i;
		  goto found_break_aligned_grob; /* ugh. need to break out of 2 loops */
		}
	      else if (break_aligned_grob == -1)
		break_aligned_grob = i;
	    }
	}
    }

found_break_aligned_grob:
  if (break_aligned_grob == -1)
    return scm_from_int (0);

  Grob *alignment_parent = elements[break_aligned_grob];
  Grob *common = me->common_refpoint (alignment_parent, X_AXIS);
  Real anchor = robust_scm2double (alignment_parent->get_property ("break-align-anchor"), 0);

  return scm_from_double (alignment_parent->relative_coordinate (common, X_AXIS)
			  - me->relative_coordinate (common, X_AXIS)
			  + anchor);
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_average_anchor, 1)
SCM
Break_aligned_interface::calc_average_anchor (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  Real avg = 0.0;
  int count = 0;

  /* average the anchors of those children that have it set */
  extract_grob_set (me, "elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    {
      SCM anchor = elts[i]->get_property ("break-align-anchor");
      if (scm_is_number (anchor))
	{
	  count++;
	  avg += scm_to_double (anchor);
	}
    }

  return scm_from_double (count > 0 ? avg / count : 0);
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_extent_aligned_anchor, 1)
SCM
Break_aligned_interface::calc_extent_aligned_anchor (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Real alignment = robust_scm2double (me->get_property ("break-align-anchor-alignment"), 0.0);
  Interval iv = me->extent (me, X_AXIS);

  if (isinf (iv[LEFT]) && isinf (iv[RIGHT])) /* avoid NaN */
    return scm_from_double (0.0);

  return scm_from_double (iv.linear_combination (alignment));
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_break_visibility, 1)
SCM
Break_aligned_interface::calc_break_visibility (SCM smob)
{
  /* a BreakAlignGroup is break-visible if it has one element that is break-visible */
  Grob *me = unsmob_grob (smob);
  SCM ret = scm_c_make_vector (3, SCM_EOL);
  extract_grob_set (me, "elements", elts);
  for (int dir = 0; dir <= 2; dir++)
    {
      bool visible = false;
      for (vsize i = 0; i < elts.size (); i++)
	{
	  SCM vis = elts[i]->get_property ("break-visibility");
	  if (scm_is_vector (vis) && to_boolean (scm_c_vector_ref (vis, dir)))
	    visible = true;
	}
      scm_c_vector_set_x (ret, dir, scm_from_bool (visible));
    }
  return ret;
}

ADD_INTERFACE (Break_alignable_interface,
	       "Object that is aligned on a break alignment.",

	       /* properties */
	       "break-align-symbols "
	       );

ADD_INTERFACE (Break_aligned_interface,
	       "Items that are aligned in prefatory matter.\n"
	       "\n"
	       "The spacing of these items is controlled by the"
	       " @code{space-alist} property.  It contains a list"
	       " @code{break-align-symbol}s with a specification of the"
	       " associated space.  The space specification can be\n"
	       "\n"
	       "@table @code\n"
	       "@item (minimum-space . @var{spc}))\n"
	       "Pad space until the distance is @var{spc}.\n"
	       "@item (fixed-space . @var{spc})\n"
	       "Set a fixed space.\n"
	       "@item (semi-fixed-space . @var{spc})\n"
	       "Set a space.  Half of it is fixed and half is stretchable."
	       " (does not work at start of line. fixme)\n"
	       "@item (extra-space . @var{spc})\n"
	       "Add @var{spc} amount of space.\n"
	       "@end table\n"
	       "\n"
	       "Special keys for the alist are @code{first-note} and"
	       " @code{next-note}, signifying the first note on a line, and"
	       " the next note halfway a line.\n"
	       "\n"
	       "Rules for this spacing are much more complicated than this."
	       "  See [Wanske] page 126--134, [Ross] page 143--147.",

	       /* properties */ 
	       "break-align-anchor "
	       "break-align-anchor-alignment "
	       "break-align-symbol "
	       "space-alist "
	       );

ADD_INTERFACE (Break_alignment_interface,
	       "The object that performs break alignment.  See"
	       " @ref{break-aligned-interface}.",

	       /* properties */
	       "positioning-done "
	       "break-align-orders "
	       );

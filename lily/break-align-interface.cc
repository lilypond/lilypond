/*
  break-align-interface.cc -- implement Break_align_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


MAKE_SCHEME_CALLBACK (Break_align_interface, self_align_callback, 1);
SCM
Break_align_interface::self_align_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Item *item = dynamic_cast<Item *> (me);
  Direction bsd = item->break_status_dir ();
  if (bsd == LEFT)
    me->set_property ("self-alignment-X", scm_from_int (RIGHT));

  /*
    Force break alignment itself to be done first, in the case
  */
  return Self_alignment_interface::aligned_on_self (me, X_AXIS);
}

/*
  This is tricky: we cannot modify 'elements, since callers are
  iterating the same list. Reordering the list in-place, or resetting
  'elements will skip elements in the loops of callers.

  So we return the correct order as an array.
*/
Link_array<Grob>
Break_align_interface::ordered_elements (Grob *grob)
{
  Item *me = dynamic_cast<Item *> (grob);
  extract_grob_set (me, "elements", elts);

  SCM order_vec = me->get_property ("break-align-orders");
  if (!scm_is_vector (order_vec)
      || scm_c_vector_length (order_vec) < 3)
    return elts;

  Link_array<Grob> writable_elts (elts);
  SCM order = scm_vector_ref (order_vec,
			      scm_from_int (me->break_status_dir () + 1));

  /*
    Copy in order specified in BREAK-ALIGN-ORDER.
  */
  Link_array<Grob> new_elts;
  for (; scm_is_pair (order); order = scm_cdr (order))
    {
      SCM sym = scm_car (order);

      for (vsize i = writable_elts.size (); i--;)
	{
	  Grob *g = writable_elts[i];
	  if (g && sym == g->get_property ("break-align-symbol"))
	    {
	      new_elts.push_back (g);
	      writable_elts.del (i);
	    }
	}
    }

  return new_elts;
}

void
Break_align_interface::add_element (Grob *me, Grob *toadd)
{
  Align_interface::add_element (me, toadd);
}

MAKE_SCHEME_CALLBACK(Break_align_interface, calc_positioning_done, 1)
SCM
Break_align_interface::calc_positioning_done (SCM smob)
{
  Grob *grob = unsmob_grob (smob);  
  Item *me = dynamic_cast<Item *> (grob);

  Link_array<Grob> elems = ordered_elements (me);
  std::vector<Interval> extents;

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

  std::vector<Real> offsets;
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
	      && elt->get_property ("break-align-symbol")
	      == ly_symbol2scm ("left-edge"))
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
	  std::string sym_string;
	  if (scm_is_symbol (rsym))
	    sym_string = ly_symbol2string (rsym);

	  std::string orig_string;
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
	extra_right_space = distance;

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

ADD_INTERFACE (Break_aligned_interface, "break-aligned-interface",
	       "Items that are aligned in prefatory matter.\n"
	       "\n"
	       "The spacing of these items is controlled by the @code{space-alist}\n"
	       "property. It contains a list @code{break-align-symbol}s with a specification\n"
	       "of the associated space. The space specification can be "
	       "@table @code\n"
	       "@item (minimum-space . @var{spc}))\n"
	       "  Pad space until the distance is @var{spc}\n"
	       "@item (fixed-space . @var{spc})\n"
	       "  Set a fixed space\n"
	       "@item (semi-fixed-space . @var{spc})\n"
	       "  Set a space. Half of it is fixed and half is stretchable. \n"
	       "(does not work at start of line. fixme)\n"
	       "@item (extra-space . @var{spc})\n"
	       "  Add @var{spc} amount of space.\n"
	       "@end table\n"
	       "\n"
	       "Special keys for the alist are @code{first-note} and @code{next-note}, signifying\n"
	       "the first note on a line, and the next note halfway a line.\n"
	       "\n"
	       "Rules for this spacing are much more complicated than this. \n"
	       "See [Wanske] page 126 -- 134, [Ross] pg 143 -- 147\n",

	       /* properties */ 
	       "break-align-symbol "
	       "space-alist "
	       );

ADD_INTERFACE (Break_align_interface, "break-alignment-interface",
	       "The object that performs break aligment. See @ref{break-aligned-interface}.",

	       /* properties */
	       "positioning-done "
	       "break-align-orders");


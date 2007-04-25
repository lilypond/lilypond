/*
  break-align-interface.cc -- implement Break_alignment_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

  SCM my_align = me->get_property ("break-align-symbol");
  SCM order = Break_alignment_interface::break_align_order (alignment);

  vector<Grob*> elements = Break_alignment_interface::ordered_elements (alignment);
  if (elements.size () == 0)
    return scm_from_int (0);
  
  int last_idx_found = -1;
  vsize i = 0;
  for (SCM s = order; scm_is_pair (s); s = scm_cdr (s))  
    {
      if (i < elements.size ()
	  && elements[i]->get_property ("break-align-symbol") == scm_car (s))
	{
	  last_idx_found = i;
	  i ++;
	}

      if (scm_car (s) == my_align)
	break ;
    }	

  Direction which_edge = LEFT;
  if (vsize (last_idx_found + 1) < elements.size ())
    last_idx_found ++;
  else
    which_edge = RIGHT;
  
  Grob *alignment_parent = elements[last_idx_found];
  Grob *common = me->common_refpoint (alignment_parent, X_AXIS);
  Real anchor = robust_scm2double (alignment_parent->get_property ("break-align-anchor"), 0);

  return scm_from_double (alignment_parent->relative_coordinate (common, X_AXIS)
			  - me->relative_coordinate (common, X_AXIS)
			  + anchor);
}

MAKE_SCHEME_CALLBACK (Break_aligned_interface, calc_anchor, 1)
SCM
Break_aligned_interface::calc_anchor (SCM grob)
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

ADD_INTERFACE (Break_alignable_interface,
	       "Object that is aligned on a break aligment. ",

	       /* properties */
	       "break-align-symbol "
	       )



ADD_INTERFACE (Break_aligned_interface,
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
	       "break-align-anchor "
	       "break-align-symbol "
	       "space-alist "
	       );

ADD_INTERFACE (Break_alignment_interface,
	       "The object that performs break aligment. See @ref{break-aligned-interface}.",

	       /* properties */
	       "positioning-done "
	       "break-align-orders");

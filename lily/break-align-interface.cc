/*
  break-align-interface.cc -- implement Break_align_interface

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <math.h>
#include <libc-extension.hh>	// isinf

#include "side-position-interface.hh"
#include "axis-group-interface.hh"
#include "warn.hh"
#include "lily-guile.hh"
#include "break-align-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "paper-column.hh"
#include "group-interface.hh"
#include "align-interface.hh"

MAKE_SCHEME_CALLBACK (Break_align_interface,alignment_callback,2);

SCM
Break_align_interface::alignment_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);

  assert (a == X_AXIS);
  Grob *par = me->get_parent (a);
  if (par && !to_boolean (par->get_grob_property ("break-alignment-done")))
    {
      par->set_grob_property ("break-alignment-done", SCM_BOOL_T);
      Break_align_interface::do_alignment (par);
    }
    
  return gh_double2scm (0);
}

MAKE_SCHEME_CALLBACK (Break_align_interface,self_align_callback,2);
SCM
Break_align_interface::self_align_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == X_AXIS);
  
  Item* item = dynamic_cast<Item*> (me);
  Direction bsd = item->break_status_dir ();
  if (bsd == LEFT)
    {
      me->set_grob_property ("self-alignment-X", scm_int2num (RIGHT));
    }

  /*
    Force break alignment itself to be done first, in the case
   */
  return Self_alignment_interface::aligned_on_self (element_smob, axis);  
}

void
Break_align_interface::add_element (Grob*me, Grob *toadd)
{
  Axis_group_interface::add_element (me, toadd);
}

void
Break_align_interface::do_alignment (Grob *me)
{
  Item * item = dynamic_cast<Item*> (me);

  Link_array<Grob> elems
    = Pointer_group_interface__extract_grobs (me, (Grob*)0,
						 "elements");
  Array<Interval> extents;
  
  for (int i=0; i < elems.size (); i++) 
    {
      Interval y = elems[i]->extent (elems[i], X_AXIS);
      extents.push (y);
    }


  int idx  = 0;
  while (idx < extents.size  () && extents[idx].empty_b ())
    idx++;
  
  Array<Real> offsets;
  offsets.set_size (elems.size());
  for (int i= 0; i < offsets.size();i ++)
    offsets[i] = 0.0;


  int edge_idx = -1;
  while (idx < elems.size())
    {
      int next_idx = idx+1;
      while ( next_idx < elems.size() && extents[next_idx].empty_b())
	next_idx++;

      if (next_idx == elems.size())
	break;
      
      Grob *l = elems[idx];
      Grob *r = elems[next_idx];

      SCM alist = SCM_EOL;

      for (SCM s= l->get_grob_property ("elements");
	   gh_pair_p (s) ; s = gh_cdr (s))
	  {
	    Grob *elt = unsmob_grob (gh_car (s));

	    if (edge_idx < 0
		&& elt->get_grob_property ("break-align-symbol") == ly_symbol2scm( "left-edge"))
	      edge_idx = idx;
	    
	    SCM l =elt->get_grob_property ("space-alist");
	    if (gh_pair_p(l))
	      {
		alist= l;
		break;
	      }
	  }

      SCM rsym = SCM_EOL;

      /*
	We used to use #'cause to find out the symbol and the spacing
	table, but that gets icky when that grob is suicided for some
	reason.
      */
      for (SCM s = r->get_grob_property ("elements");
	   gh_pair_p (s); s = gh_cdr (s))
	{
	  Grob * elt =unsmob_grob(gh_car (s));

	  SCM sym = elt->get_grob_property ("break-align-symbol");
	  if (gh_symbol_p (sym))
	    {
	      rsym = sym;
	      break;
	    }
	}
      if (rsym  == ly_symbol2scm("left-edge"))
	edge_idx = next_idx;

      SCM entry = SCM_EOL;
      if (gh_symbol_p (rsym))
	entry = scm_assq (rsym, alist);

      bool entry_found = gh_pair_p (entry);
      if (!entry_found)
	{
	  String sym_string;
	  if(gh_symbol_p (rsym))
	    sym_string = ly_symbol2string (rsym);

	  String orig_string ;
	  if (unsmob_grob (l->get_grob_property ("cause")))
	    orig_string = unsmob_grob (l->get_grob_property ("cause"))->name ();
	  
	  programming_error (_f("No spacing entry from %s to `%s'",
				orig_string.to_str0 (),
				sym_string.to_str0 ()));
	}

      Real distance = 1.0;
      SCM type = ly_symbol2scm ("extra-space");
      
      if (entry_found)
	{
	  entry = gh_cdr (entry);
	  
	  distance = gh_scm2double (gh_cdr (entry));
	  type = gh_car (entry) ;
	}

      if (type == ly_symbol2scm ("extra-space"))
	offsets[next_idx] = extents[idx][RIGHT] + distance;
      else if (type == ly_symbol2scm("minimum-space"))
	offsets[next_idx] = extents[idx][RIGHT] >? distance;

      idx = next_idx;
    }


  Real here = 0.0;
  Interval total_extent;

  Real alignment_off =0.0;  
  for (int i =0 ; i < offsets.size(); i++)
    {
      here += offsets[i];
      if (i == edge_idx)
	alignment_off = -here; 
      total_extent.unite (extents[i] + here);
    }


  if (item->break_status_dir () == LEFT)
    alignment_off = -total_extent[RIGHT];
  else if (edge_idx < 0)
    alignment_off = -total_extent[LEFT];

  here = alignment_off;
  for (int i =0 ; i < offsets.size(); i++)
    {
      here += offsets[i];
      elems[i]->translate_axis (here, X_AXIS);
    }
}


ADD_INTERFACE (Break_aligned_interface, "break-aligned-interface",
  "Items that are aligned in prefatory matter.

The spacing of these items is controlled by the space-alist
property. It contains a list break-align-symbols with a specification
of the associated space. The space definition is either (extra-space
. @var{number}), which adds space after the symbol, (minimum-space
. @var{ms}), which pads the space until it it is @var{ms}.


Special keys for the alist are 'first-note and 'next-note, signifyign
the first note on a line, and the next note halfway a line.

Rules for this spacing are much more complicated than this. 
See [Wanske] page 126 -- 134, [Ross] pg 143 -- 147


",
  "break-align-symbol break-alignment-done space-alist");

ADD_INTERFACE (Break_align_interface, "break-alignment-interface",
  "See @ref{break-aligned-interface}.",
  "break-alignment-done");




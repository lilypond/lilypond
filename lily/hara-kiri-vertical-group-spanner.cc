/*
  hara-kiri-vertical-group-spanner.cc 
    -- implement Hara_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998,1999 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "hara-kiri-vertical-group-spanner.hh"
#include "debug.hh"
#include "item.hh"

Hara_kiri_group_spanner::Hara_kiri_group_spanner()
{
  set_elt_property ("items-worth-living", SCM_EOL);
  set_axes(Y_AXIS,Y_AXIS);
}

void 
Hara_kiri_group_spanner::add_interesting_item (Item* n)
{
  add_dependency (n);
  set_elt_property ("items-worth-living",
		    gh_cons (n->self_scm_,
			     get_elt_property ("items-worth-living")));
}

void 
Hara_kiri_group_spanner::do_post_processing ()
{
  SCM worth = get_elt_property ("items-worth-living");
  if (gh_pair_p (worth))
    return;

  Link_array<Score_element> childs = get_children ();
  for (int i = 0; i < childs.size (); i++)
    {
      Score_element* s = childs[i];

      if ( line_l () != s->line_l ())
	programming_error ("Killing other children too");
      s->set_elt_property ("transparent", SCM_BOOL_T);
      s->set_empty (X_AXIS);
      s->set_empty (Y_AXIS);

    }
  set_empty (X_AXIS);
  set_empty (Y_AXIS);  
}




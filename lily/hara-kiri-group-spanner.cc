/*
  hara-kiri-vertical-group-spanner.cc -- implement Hara_kiri_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-interface.hh"
#include "hara-kiri-group-spanner.hh"
#include "debug.hh"
#include "item.hh"

/*
 */
Hara_kiri_group_spanner::Hara_kiri_group_spanner(SCM s)
  : Spanner (s)
{
  set_elt_pointer ("items-worth-living", SCM_EOL);
}

void 
Hara_kiri_group_spanner::add_interesting_item (Item* n)
{
  add_dependency (n);
  Pointer_group_interface (this, "items-worth-living").add_element (n);

}

GLUE_SCORE_ELEMENT(Hara_kiri_group_spanner,after_line_breaking);
SCM 
Hara_kiri_group_spanner::member_after_line_breaking ()
{
  SCM worth = get_elt_pointer ("items-worth-living");
  /*
    worth == self_scm  is a stupid way to signal that we're done.
   */
  if (gh_pair_p (worth))
    return SCM_UNDEFINED;

  Link_array<Score_element> childs = Axis_group_interface (this).get_children ();
  for (int i = 0; i < childs.size (); i++)
    {
      Score_element* s = childs[i];

      if ( line_l () != s->line_l ())
	programming_error ("Killing other children too");
      s->suicide ();
    }

  /*
    very appropriate name here :-)
   */
  suicide ();
  return SCM_UNDEFINED;
}



/*
  We can't rely on offsets and dimensions of elements in a hara-kiri
  group. Use a callback to make sure that hara-kiri has been done
  before asking for offsets.  */
Real
Hara_kiri_group_spanner::force_hara_kiri_callback (Score_element const  *elt, Axis a)
{
  while (elt && !dynamic_cast<Hara_kiri_group_spanner const*> (elt))
    elt = elt->parent_l(a);

  if (elt)
    {
      Hara_kiri_group_spanner const  * seppuku = dynamic_cast<Hara_kiri_group_spanner const*> (elt);

      ((Hara_kiri_group_spanner*)seppuku)->member_after_line_breaking ();
    }
  

  return 0.0;
}

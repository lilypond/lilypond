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

Hara_kiri_vertical_group_spanner::Hara_kiri_vertical_group_spanner()
{
}

void 
Hara_kiri_vertical_group_spanner::add_interesting_item (Item* n)
{
  add_dependency (n);
  interesting_items_.push (n);
}

void 
Hara_kiri_vertical_group_spanner::do_post_processing ()
{
  if (!interesting_items_.empty ())
    return;

  Link_array<Score_element> childs = get_children ();
  for (int i = 0; i < childs.size (); i++)
    {
      childs[i]->transparent_b_ = true;
      childs[i]->set_empty (true);
    }
  set_empty (true);
}

void
Hara_kiri_vertical_group_spanner::do_substitute_dependency (Score_element*o, Score_element*n)
{
  if (Item *it = dynamic_cast<Item *> (o))
    interesting_items_.substitute (it, dynamic_cast<Item *> (n));
}


void
Hara_kiri_vertical_group_spanner::do_print () const
{
  Axis_group_spanner::do_print ();
}

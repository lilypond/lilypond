/*
  hara-kiri-vertical-group-spanner.cc 
    -- implement Hara_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "hara-kiri-vertical-group-spanner.hh"
#include "debug.hh"
#include "note-head.hh"
#include "bar.hh"

IMPLEMENT_IS_TYPE_B1 (Hara_kiri_vertical_group_spanner, Vertical_group_spanner);

Hara_kiri_vertical_group_spanner::Hara_kiri_vertical_group_spanner()
{
}

void 
Hara_kiri_vertical_group_spanner::add_note (Note_head* n)
{
  add_dependency (n);
  head_l_arr_.push (n);
}

void 
Hara_kiri_vertical_group_spanner::do_post_processing ()
{
  if (!head_l_arr_.empty ())
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
  if (o->is_type_b (Note_head::static_name ()))
    head_l_arr_.substitute ((Note_head*)o->access_Item (), 
      (n)? (Note_head*)n->access_Item () : 0);
}


void
Hara_kiri_vertical_group_spanner::do_print () const
{
  Axis_group_spanner::do_print ();
}

/*
  hara-kiri-vertical-group-spanner.cc 
    -- implement Hara_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "hara-kiri-vertical-group-spanner.hh"
#include "item.hh"
#include "note-head.hh"
#include "p-col.hh"
#include "molecule.hh"

IMPLEMENT_IS_TYPE_B1 (Hara_kiri_vertical_group_spanner, Vertical_group_spanner);

void 
Hara_kiri_vertical_group_spanner::add_element (Graphical_element* e)
{
  if (e->is_type_b (Note_head::static_name ()))
    add_dependency ((Score_elem*)e);
  Vertical_group_spanner::add_element (e);
}

// we never get here, or indeed in axis-group-spanner::do_break_processing?
void 
Hara_kiri_vertical_group_spanner::do_break_processing ()
{
  Vertical_group_spanner::do_break_processing ();
  if (dependency_size ())
    return;
  transparent_b_ = true;
}

// too late ?
void 
Hara_kiri_vertical_group_spanner::do_post_processing ()
{
  if (dependency_size ())
    return;
  transparent_b_ = true;
}

Molecule*
Hara_kiri_vertical_group_spanner::brew_molecule_p() const
{
  // aaarg: go away
  if (1) // transparent_b_)
    return new Molecule;
  return Vertical_group_spanner::brew_molecule_p ();
}



/*
  encompass-info.cc -- implement Encompass_info

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>

*/
#include "dimension-cache.hh"
#include "proto.hh"
#include "stem.hh"
#include "note-column.hh"
#include "paper-def.hh"
#include "encompass-info.hh"
#include "slur.hh"
#include "staff-symbol.hh"
#include "note-head.hh"
#include "debug.hh"
#include "align-element.hh"

Encompass_info::Encompass_info ()
{
}

Encompass_info::Encompass_info (Note_column const* note_column, Direction dir, Slur const* slur_l)
{
  interstaff_f_ = 0;
  
  Stem* stem_l = note_column->stem_l_;
  if (!stem_l)
    {
      warning (_ ("Slur over rest?"));
      o_[X_AXIS] = note_column->hpos_f ();
      o_[Y_AXIS] = note_column->extent (Y_AXIS)[dir];
      return; 
    }
  
  o_[X_AXIS] = stem_l->hpos_f ();

  /*
    Simply set x to middle of notehead
   */

  o_[X_AXIS] -= 0.5 * stem_l->dir_ * note_column->extent (X_AXIS).length ();

  if ((stem_l->dir_ == dir)
      && !stem_l->extent (Y_AXIS).empty_b ())
    {
      o_[Y_AXIS] = stem_l->extent (Y_AXIS)[dir];
    }
  else
    {
      o_[Y_AXIS] = note_column->extent (Y_AXIS)[dir];
    }

  /*
   leave a gap: slur mustn't touch head/stem
   */
  o_[Y_AXIS] += dir * slur_l->paper_l ()->get_var ("slur_y_free");

  Graphical_element *common = stem_l->common_refpoint (slur_l, Y_AXIS);
  Align_element * align = dynamic_cast<Align_element*> (common);
  if (align && align->axis() == Y_AXIS)
    {
      if (align->threshold_interval_[MIN] != 
	  align->threshold_interval_[MAX])
	warning (_ ("minVerticalAlign != maxVerticalAlign: interstaff beams/slurs may be broken"));

      interstaff_f_ = align->threshold_interval_[MIN];

      Graphical_element const * slur_refpoint = slur_l;
      while (slur_refpoint->parent_l  (Y_AXIS) != common)
	slur_refpoint = slur_refpoint->parent_l (Y_AXIS);

      Graphical_element const * note_refpoint = note_column;
      while (note_refpoint->parent_l (Y_AXIS) != common)
	note_refpoint = note_refpoint->parent_l (Y_AXIS);

      int slur_prio =
	align->get_priority ((Score_element*) dynamic_cast<Score_element const*> (slur_refpoint));
      int stem_prio =
	align->get_priority ((Score_element*) dynamic_cast<Score_element  const *> (note_refpoint));

      /*
	our staff is lower -> interstaff_f_ *= -1
       */

      if (slur_prio < stem_prio)
	interstaff_f_ *= -1;
      o_[Y_AXIS] += interstaff_f_;
    }
}

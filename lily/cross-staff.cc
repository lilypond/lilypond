#include "cross-staff.hh"
#include "item.hh"
#include "align-element.hh"
#include "spanner.hh"
#include "warn.hh"

Real
calc_interstaff_dist (Item const *item, Spanner const *span)
{
  Real interstaff = 0.0; 
  Score_element *common = item->common_refpoint (span, Y_AXIS);
  Align_element * align = dynamic_cast<Align_element*> (common);
  if (align && align->axis() == Y_AXIS)
    {
      if (align->threshold_interval_[MIN] != 
	  align->threshold_interval_[MAX])
	warning (_ ("minVerticalAlign != maxVerticalAlign: cross staff spanners may be broken"));

      interstaff = align->threshold_interval_[MIN];

      Score_element const * span_refpoint = span;
      while (span_refpoint->parent_l  (Y_AXIS) != common)
	span_refpoint = span_refpoint->parent_l (Y_AXIS);

      Score_element const * note_refpoint = item;
      while (note_refpoint->parent_l (Y_AXIS) != common)
	note_refpoint = note_refpoint->parent_l (Y_AXIS);

      int span_prio =
	align->get_count ((Score_element*) dynamic_cast<Score_element const*> (span_refpoint));
      int item_prio =
	align->get_count ((Score_element*) dynamic_cast<Score_element  const *> (note_refpoint));

      /*
	our staff is lower -> interstaff *= -1
       */

      if (span_prio < item_prio)
	interstaff *= -1;
      return interstaff;
    }
  else return 0.0;
}


#include "cross-staff.hh"
#include "item.hh"
#include "align-interface.hh"
#include "spanner.hh"
#include "warn.hh"
#include "paper-def.hh"


/*
  JUNKME
 */
Real
calc_interstaff_dist (Item  *item, Spanner  *span)
{
  Real interstaff = 0.0; 
  Grob *common = item->common_refpoint (span, Y_AXIS);


  if (Align_interface::has_interface (common) && Align_interface::axis(common) == Y_AXIS)
    {
      SCM threshold = common->get_grob_property ("forced-distance");
      interstaff = 1.0;
      if (!gh_number_p (threshold))
	warning (_ ("not a forced distance; cross-staff spanners may be broken"));
      else
	interstaff *= gh_scm2double (threshold);

      Grob  * span_refpoint = span;
      while (span_refpoint->parent_l  (Y_AXIS) != common)
	span_refpoint = span_refpoint->parent_l (Y_AXIS);

      Grob  * note_refpoint = item;
      while (note_refpoint->parent_l (Y_AXIS) != common)
	note_refpoint = note_refpoint->parent_l (Y_AXIS);

      int span_prio =
	Align_interface::get_count (common,(Grob*) dynamic_cast<Grob *> (span_refpoint));
      int item_prio =
	Align_interface::get_count (common,(Grob*) dynamic_cast<Grob   *> (note_refpoint));

      /*
	our staff is lower -> interstaff *= -1
       */

      if (span_prio < item_prio)
	interstaff *= -1;
      return interstaff;
    }
  else return 0.0;
}


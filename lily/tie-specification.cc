#include "tie-formatting-problem.hh"
#include "grob.hh"
#include "tie.hh"
#include "libc-extension.hh"
#include "tie-specification.hh"

Tie_specification::Tie_specification ()
{
  tie_grob_ = 0;
  has_manual_position_ = false;
  has_manual_dir_ = false;
  has_manual_delta_y_ = false;
  position_ = 0;
  manual_position_ = 0;
  manual_dir_ = CENTER;
  note_head_drul_[LEFT] =
    note_head_drul_[RIGHT] = 0;
  column_ranks_[RIGHT] =
    column_ranks_[LEFT] = 0;
}


void
Tie_specification::from_grob (Grob *tie)
{
  tie_grob_ = tie;
  if (scm_is_number (tie->get_property_data ("direction")))
    {
      manual_dir_ = to_dir (tie->get_property ("direction"));
      has_manual_dir_ = true;
    }
  
  position_ = Tie::get_position (tie);
  SCM pos_scm = tie->get_property ("staff-position");
  if (scm_is_number (pos_scm))
    {
      has_manual_delta_y_ = (scm_inexact_p (pos_scm) == SCM_BOOL_T);
      manual_position_ = scm_to_double (tie->get_property ("staff-position"));
      has_manual_position_ = true;
    }
}

int
Tie_specification::column_span () const
{
  return column_ranks_[RIGHT] - column_ranks_[LEFT];
}

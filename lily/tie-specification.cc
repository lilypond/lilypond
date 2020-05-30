#include "tie-formatting-problem.hh"
#include "grob.hh"
#include "item.hh"
#include "semi-tie.hh"
#include "spanner.hh"
#include "tie.hh"
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
  note_head_drul_[LEFT]
    = note_head_drul_[RIGHT] = 0;
  column_ranks_[RIGHT]
    = column_ranks_[LEFT] = 0;
}

void
Tie_specification::from_grob (Grob *tie)
{
  // In this method, Tie and Semi_tie require the same logic with different
  // types.  It might be clearer to use a template.
  tie_grob_ = tie;
  if (scm_is_number (get_property_data (tie, "direction")))
    {
      manual_dir_ = from_scm<Direction> (get_property (tie, "direction"));
      has_manual_dir_ = true;
    }

  if (Spanner *spanner = dynamic_cast<Spanner *> (tie))
    position_ = Tie::get_position (spanner);
  else if (Item *item = dynamic_cast<Item *> (tie))
    position_ = Semi_tie::get_position (item);
  else
    {
      programming_error ("grob is neither a tie nor a semi-tie");
      position_ = 0;
    }

  SCM pos_scm = get_property (tie, "staff-position");
  if (scm_is_number (pos_scm))
    {
      has_manual_delta_y_ = !is_scm<Rational> (pos_scm);
      manual_position_ = scm_to_double (get_property (tie, "staff-position"));
      has_manual_position_ = true;
    }
}

int
Tie_specification::column_span () const
{
  return column_ranks_[RIGHT] - column_ranks_[LEFT];
}

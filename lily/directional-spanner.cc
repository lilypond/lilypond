#include "directional-spanner.hh"

Direction
Directional_spanner::get_default_dir() const
{
  return DOWN;
}

void
Directional_spanner::do_pre_processing()
{
  if (!get_direction ())
    set_direction (get_default_dir());
}

Directional_spanner::Directional_spanner()
{
  set_direction (CENTER);
}

Offset
Directional_spanner::center () const
{
  Real w= extent (X_AXIS).length ();
  Offset o (w/2, 0);  
  return o;
}

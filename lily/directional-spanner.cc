#include "directional-spanner.hh"

void
Directional_spanner::set_default_dir()
{
  dir_ = DOWN;
}

void
Directional_spanner::do_pre_processing()
{
  if (!dir_)
    set_default_dir();
}

Directional_spanner::Directional_spanner()
{
  dir_ = CENTER;
}

Offset
Directional_spanner::center () const
{
  Real w= extent (X_AXIS).length ();
  Offset o (w/2, 0);  
  return o;
}

#include "directional-spanner.hh"

void
Directional_spanner::set_default_dir()
{
    dir_i_ = -1;
}

void
Directional_spanner::do_pre_processing()
{
    if (!dir_i_)
	set_default_dir();
}

Directional_spanner::Directional_spanner()
{
    dir_i_ = 0;
}

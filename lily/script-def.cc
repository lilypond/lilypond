#include "debug.hh"
#include "script-def.hh"

Script_def::Script_def(String idx,  bool follow, int stem, int staff ,bool invert)
{
    inside_staff_b_ = follow;
    symidx = idx ;
    rel_stem_dir_i_ =stem;
    staff_dir_i_ = staff;
    invertsym_b_ = invert;
    priority_i_ =0;
}

void
Script_def::print() const
{
    mtor << "Script_def{ idx: " << symidx 
	 << " direction, stem: " << rel_stem_dir_i_ << " staff : " << staff_dir_i_ << "}\n";
}
int
Script_def::compare(Script_def const & c)
{
    return !(symidx == c.symidx &&
	rel_stem_dir_i_ == c.rel_stem_dir_i_&&
	staff_dir_i_ == c.staff_dir_i_&&
	invertsym_b_ == c.invertsym_b_);
}

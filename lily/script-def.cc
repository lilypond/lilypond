/*
  script-def.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "script-def.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "lookup.hh"

Script_def::Script_def()
{
    inside_staff_b_ = false;
    symidx = "unknown" ;
    rel_stem_dir_i_ =0;
    staff_dir_i_ = -1;
    invertsym_b_ = 0;
    priority_i_ =0;
}

void
Script_def::set_from_input(String idx,  bool follow, int stem, int staff ,bool invert)
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

bool
Script_def::do_equal_b(General_script_def const &g)const 
{
    Script_def const & c = (Script_def const&) g;
    return !(symidx == c.symidx &&
	     rel_stem_dir_i_ == c.rel_stem_dir_i_&&
	     staff_dir_i_ == c.staff_dir_i_&&
	     invertsym_b_ == c.invertsym_b_);
}

int
Script_def::staff_dir_i() const
{
    return staff_dir_i_; 
}

int
Script_def::rel_stem_dir_i() const
{
    return rel_stem_dir_i_; 
}

bool
Script_def::inside_b() const
{
    return inside_staff_b_; 
}

Atom
Script_def::get_atom(Paper_def *p , int d)const
{
    String preidx_str ="";
    if (invertsym_b_&& d < 0) 
	preidx_str = "-";

    return p->lookup_l()->script(preidx_str + symidx);
}

IMPLEMENT_STATIC_NAME(Script_def);

int
Script_def::priority_i()const
{
    return priority_i_;
}
	
    
    

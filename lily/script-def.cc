/*
  script-def.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "script-def.hh"

#include "paper-def.hh"
#include "lookup.hh"

Script_def::Script_def()
{
  inside_staff_b_ = false;
  symidx_str_ = "unknown" ;
  rel_stem_dir_ =CENTER;
  staff_dir_ = DOWN;
  invertsym_b_ = 0;
  priority_i_ =0;
}

void
Script_def::set_from_input (String idx,  bool follow, int stem, int staff ,bool invert, int priority_i)
{
  inside_staff_b_ = follow;
  symidx_str_ = idx ;
  rel_stem_dir_ =Direction(stem);
  staff_dir_ = Direction(staff);
  invertsym_b_ = invert;
  priority_i_ =priority_i;
}


void
Script_def::do_print() const
{
#ifndef NPRINT
  DOUT << "Script_def{ idx: " << symidx_str_ 
       << " direction, stem: " << rel_stem_dir_ << " staff : " << staff_dir_ << "}\n";
#endif
}

bool
Script_def::do_equal_b (General_script_def const *g) const 
{
  Script_def const * c = dynamic_cast<Script_def const*> (g);
  return c&& (symidx_str_ == c->symidx_str_ &&
	  rel_stem_dir_ == c->rel_stem_dir_&&
	  staff_dir_ == c->staff_dir_&&
	  invertsym_b_ == c->invertsym_b_);
}

Direction
Script_def::staff_dir() const
{
  return staff_dir_; 
}

Direction
Script_def::rel_stem_dir() const
{
  return rel_stem_dir_; 
}

bool
Script_def::inside_b() const
{
  return inside_staff_b_; 
}

Molecule
Script_def::get_molecule (Paper_def *p , Direction d) const
{
  String preidx_str ="";
  if (invertsym_b_)
    preidx_str = (d < 0)? "d" : "u";

  return p->lookup_l(0)->script (preidx_str + symidx_str_);
}




int
Script_def::priority_i() const
{
  return priority_i_;
}
	
  
  

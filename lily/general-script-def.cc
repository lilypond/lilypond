/*
  general-script-def.cc -- implement General_script_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "general-script-def.hh"
#include "debug.hh"
#include "symbol.hh"
int
General_script_def::staff_dir_i()const
{
    return -1;
}
int
General_script_def::rel_stem_dir_i()const
{
    return 0;
}
int
General_script_def::priority_i()const
{
    return 1000;
}

bool
General_script_def::inside_b()const
{
    return false;
}

bool
General_script_def::equal_b (General_script_def const&g)const
{
    if (name() != g.name ())
	return false;

    return do_equal_b (&g);
}

bool
General_script_def::do_equal_b (General_script_def const*)const
{
    return true;
}


void
General_script_def::print() const
{
    DOUT << name() << "{";
    do_print();
    DOUT << "}";
}

void
General_script_def::do_print() const
{
}

Atom
General_script_def::get_atom (Paper_def*, int)const
{
    Symbol s;
    return Atom (s);
}

IMPLEMENT_IS_TYPE_B(General_script_def);

/*
  general-script-def.cc -- implement General_script_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "general-script-def.hh"
#include "debug.hh"
#include "atom.hh"

Direction
General_script_def::staff_dir() const
{
  return DOWN;
}

Direction
General_script_def::rel_stem_dir() const
{
  return CENTER;
}
int
General_script_def::priority_i() const
{
  return 1000;
}

bool
General_script_def::inside_b() const
{
  return false;
}

bool
General_script_def::equal_b (General_script_def const&g) const
{
  if (name() != g.name ())
    return false;

  return do_equal_b (&g);
}

bool
General_script_def::do_equal_b (General_script_def const*) const
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
General_script_def::get_atom (Paper_def*, Direction) const
{
  Atom s;
  return Atom (s);
}

IMPLEMENT_IS_TYPE_B(General_script_def);


Interval
General_script_def::width (Paper_def*) const
{
  Interval t;
  return t;
}

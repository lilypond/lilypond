/*
  bar.cc -- implement Bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "debug.hh"


Bar::Bar ()
{
  set_elt_property (breakable_scm_sym, SCM_BOOL_T);
  type_str_ = "|";
}

void
Bar::do_print () const
{
#ifndef NPRINT
  String s = type_str_;
  if (s  == "{")
    s = "brace";
  if (s == "[")
    s = "bracket";
  DOUT << "type = " << s;
#endif
}

Real
Bar::get_bar_size () const
{
  return paper_l ()->get_var ("barsize");
}


Molecule*
Bar::do_brew_molecule_p () const
{    
  Molecule *output = new Molecule (lookup_l ()->bar (type_str_, get_bar_size (), paper_l ()));
  
  return output;
}

/**
  Prescriptions for splitting bars.
  TODO: put this in SCM.
 */
static char const *bar_breaks[][3] ={
  {":|", ":|:", "|:"},
  {"|", "|", ""},
  {"", "|s", "|"},
  {"|", "|:", "|:"},
  {"|.", "|.", ""},
  {":|", ":|", ""},
  {"||", "||", ""},
  {".|.", ".|.", ""},
  {"", "scorebar", "scorepostbreak"},
  {"", "{", "{"},
  {"", "[", "["},  
  {0,0,0}
};

void
Bar::do_pre_processing ()
{
  for (int i=0; bar_breaks[i][0]; i++) 
    {
      if (bar_breaks[i][1] == type_str_)
	{
	  type_str_ = bar_breaks[i][break_status_dir ()+1];
	  break;
	}
    }
  if (remove_elt_property (at_line_start_scm_sym)!= SCM_BOOL_F
      && (break_status_dir () == RIGHT) && (type_str_ == ""))
    {
      type_str_ = "|";
    }

  if (type_str_ =="")
    dim_cache_[X_AXIS]->set_empty (true);
}
  

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
  breakable_b_ = true;
  type_str_ = "|";
  at_line_start_b_ = false;
}

void
Bar::do_print () const
{
#ifndef NPRINT
    //  DOUT << type_str_; "{[" confuse  indenter.
#endif
}

Molecule*
Bar::do_brew_molecule_p () const
{    
  Paper_def *p = paper ();
  Molecule *output = new Molecule (lookup_l ()->bar (type_str_, p->get_var ("barsize")));
  
  return output;
}

/**
  Prescriptions for splitting bars.
  TODO: parametrise this (input-settable)
 */
static char const *bar_breaks[][3] ={
  {":|", ":|:", "|:"},
  {"|", "|", ""},
  {"", "|s", "|"},
  {"", "|:", "|:"},
  {"|.", "|.", ""},
  {":|", ":|", ""},
  {"||", "||", ""},
  {".|.", ".|.", ""},
  {"", "scorebar", "|"},
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
	  if (at_line_start_b_ && (break_status_dir_ == 1) && (type_str_ == ""))
	    {
	      type_str_ = "|";
	    }
	}
    }
  
  /*
    span_score_bar needs dims, so don't do
  
    transparent_b_ = empty_b_ = (!type_str_);
    
    */
}
  

/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>

#include "all-font-metrics.hh"
#include "string.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "lookup.hh"
#include "score-engraver.hh"
#include "paper-score.hh"
#include "identifier.hh"
#include "main.hh"
#include "scope.hh"
#include "file-results.hh" // urg? header_global_p
#include "paper-outputter.hh"
#include "paper-stream.hh"


Paper_def::Paper_def ()
{
  lookup_p_tab_p_ = new map<int, Lookup*>;
}


Paper_def::~Paper_def ()
{
  for (map<int,Lookup*>::const_iterator ai = lookup_p_tab_p_->begin();
       ai != lookup_p_tab_p_->end (); ai++)
    {
      delete (*ai).second;
    }
  
  delete lookup_p_tab_p_;
}

Paper_def::Paper_def (Paper_def const&s)
  : Music_output_def (s)
{
  shape_int_a_ = s.shape_int_a_;
  lookup_p_tab_p_ = new map<int, Lookup*>;
  
  for (map<int,Lookup*>::const_iterator ai = s.lookup_p_tab_p_->begin();
       ai != s.lookup_p_tab_p_->end (); ai++)
    {
      Lookup * l = new Lookup (* (*ai).second);
      set_lookup ((*ai).first, l);      
    }
}


Real
Paper_def::get_var (String s) const
{
  return get_realvar (ly_symbol2scm (s.ch_C()));
}

Real
Paper_def::get_realvar (SCM s) const
{
  if (!scope_p_->elem_b (s))
    error (_f ("unknown paper variable: `%s'", ly_symbol2string (s)));
  Real * p = scope_p_->elem (s)->access_content_Real (false);
  if (!p)
    {
      error (_("not a real variable"));
      return 0.0;
    }

  return *p;
}

/*
  FIXME. This is broken until we have a generic way of
  putting lists inside the \paper block.
 */
Interval
Paper_def::line_dimensions_int (int n) const
{
  Real lw =  get_var ("linewidth");
  Real ind = n? 0.0:get_var ("indent");

  return Interval (ind, lw);
}

void
Paper_def::set_lookup (int i, Lookup*l)
{
  map<int,Lookup*> :: const_iterator it (lookup_p_tab_p_->find (i));
  if (it != lookup_p_tab_p_->end ())
    {
      delete (*it).second;
    }
  (*lookup_p_tab_p_)[i] = l;
}


/*
  junkme.
 */
Real
Paper_def::interbeam_f (int multiplicity_i) const
{
  if (multiplicity_i <= 3)
    return get_var ("interbeam");
  else
    return get_var ("interbeam4");
}


void
Paper_def::print () const
{
#ifndef NPRINT
  Music_output_def::print ();
  DEBUG_OUT << "Paper {";
  for (map<int,Lookup*>::const_iterator ai = lookup_p_tab_p_->begin();
       ai != lookup_p_tab_p_->end (); ai++)
    {
      DEBUG_OUT << "Lookup: " << (*ai).first
		<< " = " << (*ai).second->font_name_ << '\n';
    }
  DEBUG_OUT << "}\n";
#endif
}

Lookup const *
Paper_def::lookup_l (int i) const
{
  return (*lookup_p_tab_p_)[i];
}



int Paper_def::default_count_i_ = 0;

int
Paper_def::get_next_default_count () const
{
  return default_count_i_ ++;
}

void
Paper_def::reset_default_count()
{
  default_count_i_ = 0;
}


Paper_stream*
Paper_def::paper_stream_p () const
{
  String outname = base_output_str ();

  if (outname != "-")
    outname += String (".") + output_global_ch;
  progress_indication (_f ("paper output to %s...",
			   outname == "-" ? String ("<stdout>") : outname));
		       

  target_str_global_array.push (outname);
  return new Paper_stream (outname);
}


String
Paper_def::base_output_str () const
{
  String str = get_default_output ();

  if (str.empty_b ())
    {
      str = default_outname_base_global;
      int def = get_next_default_count ();
      if (def)
	str += "-" + to_str (def);
    }
  return str;
}



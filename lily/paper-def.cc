/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>

#include "string.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "lookup.hh"
#include "main.hh"
#include "scope.hh"
#include "file-results.hh" // urg? header_global_p
#include "paper-stream.hh"

Paper_def::Paper_def ()
{
  lookup_alist_ = SCM_EOL;
}


Paper_def::~Paper_def ()
{
}

Paper_def::Paper_def (Paper_def const&src)
  : Music_output_def (src)
{
  SCM n  = SCM_EOL;
  for (SCM s = src.lookup_alist_; gh_pair_p(s); s = gh_cdr (s))
    {
      n = scm_acons (gh_caar(s), gh_cdar (s), n);
    }

  lookup_alist_  = n;
}


Real
Paper_def::get_var (String s) const
{
  return get_realvar (ly_symbol2scm (s.ch_C()));
}

SCM
Paper_def::get_scmvar (String s) const
{
  return  scope_p_->scm_elem (ly_symbol2scm (s.ch_C()));
}

Real
Paper_def::get_realvar (SCM s) const
{
  SCM val ;
  if (!scope_p_->try_retrieve (s, &val))
    {
      programming_error ("unknown paper variable: " +  ly_symbol2string (s));
      return 0.0;
    }

  if (gh_number_p (val))
    {
      return gh_scm2double (val);
    }
  else
    {
      programming_error ("not a real variable");
      return 0.0;
    }
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
Paper_def::set_lookup (int i, SCM l)
{
  assert (unsmob_lookup (l));
  lookup_alist_ = scm_assq_set_x(lookup_alist_, gh_int2scm (i), l);
}

Lookup const *
Paper_def::lookup_l (int i) const
{
  SCM l = scm_assq (gh_int2scm(i), lookup_alist_);
  return l == SCM_BOOL_F ? 0 :  unsmob_lookup (gh_cdr (l));
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



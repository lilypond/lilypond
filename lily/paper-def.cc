/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <math.h>

#include "virtual-font-metric.hh"
#include "all-font-metrics.hh"
#include "string.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "scaled-font-metric.hh"
#include "main.hh"
#include "scm-hash.hh"
#include "input-file-results.hh" // urg? header_global
#include "paper-outputter.hh"
#include "ly-modules.hh"

/*
  This is an almost empty thing. The only substantial thing this class
  handles, is scaling up and down to real-world dimensions (internally
  dimensions are against global staff-space.)
  
 */
Paper_def::Paper_def ()
{
}

Paper_def::~Paper_def ()
{
}

Paper_def::Paper_def (Paper_def const&src)
  : Music_output_def (src)
{
}



Real
Paper_def::get_realvar (SCM s) const
{
  SCM val = lookup_variable (s);
  SCM scale = lookup_variable (ly_symbol2scm ("outputscale"));
  
  Real sc = gh_scm2double (scale);
  return gh_scm2double (val) / sc;
}

/*
  FIXME. This is broken until we have a generic way of
  putting lists inside the \paper block.
 */
Interval
Paper_def::line_dimensions_int (int n) const
{
  Real lw =  get_realvar (ly_symbol2scm ("linewidth"));
  Real ind = n? 0.0:get_realvar (ly_symbol2scm ("indent"));

  return Interval (ind, lw);
}




Paper_outputter*
Paper_def::get_paper_outputter (String outname)  const
{
  progress_indication (_f ("paper output to `%s'...",
			   outname == "-" ? String ("<stdout>") : outname));

  global_input_file->target_strings_.push (outname);
  Paper_outputter * po = new Paper_outputter (outname);
  Path p = split_path (outname);
  p.ext = "";
  po->basename_ = p.to_string ();
  return po;
}


/*
  Todo: use symbols and hashtable idx?
*/
Font_metric *
Paper_def::find_font (SCM fn, Real m)
{
  SCM key = gh_cons (fn, gh_double2scm (m));
  SCM met = scm_assoc (key, scaled_fonts_);

  if (gh_pair_p (met))
    return unsmob_metrics (ly_cdr (met));

  /*
    Hmm. We're chaining font - metrics. Should consider wether to merge
    virtual-font and scaled_font.
   */
  Font_metric*  f=0;
  if (gh_list_p (fn))
    {
      f = new Virtual_font_metric (fn, m, this); // TODO: GC protection.
      
      scaled_fonts_ = scm_acons (key, f->self_scm (), scaled_fonts_);
      scm_gc_unprotect_object (f->self_scm ());
    }
  else
    {
      SCM scale_var = ly_module_lookup (scope_, ly_symbol2scm ("outputscale"));

      m /= gh_scm2double (scm_variable_ref (scale_var));

      f = all_fonts_global->find_font (ly_scm2string (fn));
      SCM val = Scaled_font_metric::make_scaled_font_metric (f, m);
      scaled_fonts_ = scm_acons (key, val, scaled_fonts_);
      scm_gc_unprotect_object (val);
      
      f = unsmob_metrics (val);
    }

  return f;
}


/*
  Return alist to translate internally used fonts back to real-world
  coordinates.  */
SCM
Paper_def::font_descriptions ()const
{
  SCM l = SCM_EOL;
  for (SCM s = scaled_fonts_; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM desc = ly_caar (s);
      if (!gh_string_p (gh_car (desc)))
	continue ;

      SCM mdesc = unsmob_metrics (ly_cdar (s))->description_;

      l = gh_cons (gh_cons (mdesc, desc), l);
    }
  return l;
}

Paper_def* 
unsmob_paper (SCM x)
{
  return dynamic_cast<Paper_def*> (unsmob_music_output_def (x));
}

  

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
#include "paper-outputter.hh"
#include "ly-module.hh"

/*
  This is an almost empty thing. The only substantial thing this class
  handles is scaling up and down to real-world dimensions (internally
  dimensions are against global staff-space.)
 */

Paper_def::Paper_def ()
{
  /* Do not remove this statement, scm_make_hash_table may trigger GC.  */
  scaled_fonts_ = SCM_EOL;
  scaled_fonts_ = scm_c_make_hash_table (11);
}

Paper_def::Paper_def (Paper_def const&src)
  : Music_output_def (src)
{
  /* Do not remove this statement, scm_make_hash_table may trigger GC.  */
  scaled_fonts_ = SCM_EOL;
  scaled_fonts_ = scm_c_make_hash_table (11);
}

Paper_def::~Paper_def ()
{
}

void
Paper_def::derived_mark ()
{
  scm_gc_mark (scaled_fonts_);
}

Real
Paper_def::get_dimension (SCM s) const
{
  SCM val = lookup_variable (s);
  SCM scale = lookup_variable (ly_symbol2scm ("outputscale"));
  
  Real sc = ly_scm2double (scale);
  return ly_scm2double (val) / sc;
}

/* FIXME.  This is broken until we have a generic way of
   putting lists inside the \paper block.  */
Interval
Paper_def::line_dimensions_int (int n) const
{
  Real lw =  get_dimension (ly_symbol2scm ("linewidth"));
  Real ind = n? 0.0:get_dimension (ly_symbol2scm ("indent"));

  return Interval (ind, lw);
}

Paper_outputter*
Paper_def::get_paper_outputter (String outname) const
{
  progress_indication (_f ("paper output to `%s'...",
			   outname == "-" ? String ("<stdout>") : outname));

  Paper_outputter * po = new Paper_outputter (outname);
  Path p = split_path (outname);
  p.ext = "";
  po->basename_ = p.to_string ();
  return po;
}

Font_metric*
Paper_def::find_scaled_font (Font_metric *f, Real m, SCM input_enc_name)
{
  SCM sizes = scm_hashq_ref (scaled_fonts_, f->self_scm (), SCM_BOOL_F);
  if (sizes != SCM_BOOL_F)
    {
      SCM met = scm_assoc (scm_make_real (m), sizes);
      if (ly_c_pair_p (met))
	return unsmob_metrics (ly_cdr (met));
    }
  else
    sizes = SCM_EOL;
  
  /* Hmm. We're chaining font - metrics.  Should consider whether to
     merge virtual-font and scaled_font.  */
  SCM val = SCM_EOL;
  if (Virtual_font_metric * vf = dynamic_cast<Virtual_font_metric*> (f))
    {
      /* For fontify_atom (), the magnification and name must be known
	 at the same time. That's impossible for

	  Scaled (Virtual_font (Font1,Font2))

	so we replace by

	  Virtual_font (Scaled (Font1), Scaled (Font2))  */
      SCM lst = SCM_EOL;
      SCM *t = &lst;
      for (SCM s = vf->get_font_list (); ly_c_pair_p (s); s = ly_cdr (s))
	{
	  Font_metric *scaled = find_scaled_font (unsmob_metrics (ly_car (s)),
						  m, input_enc_name);
	  *t = scm_cons (scaled->self_scm (), SCM_EOL);
	  t = SCM_CDRLOC(*t);
	}

      vf = new Virtual_font_metric (lst);
      val = vf->self_scm ();
    }
  else
    {
      SCM scale_var = ly_module_lookup (scope_, ly_symbol2scm ("outputscale"));

      if (!is_symbol (input_enc_name))
	{
	  SCM var = ly_module_lookup (scope_, ly_symbol2scm ("inputencoding"));
	  input_enc_name = scm_variable_ref (var);
	}
      m /= ly_scm2double (scm_variable_ref (scale_var));
      val = Modified_font_metric::make_scaled_font_metric (input_enc_name,
							   f, m);
    }

  sizes = scm_acons (scm_make_real (m), val, sizes);
  scm_gc_unprotect_object (val);
  scm_hashq_set_x (scaled_fonts_, f->self_scm (), sizes);
  return unsmob_metrics (val);
}


/* Return alist to translate internally used fonts back to real-world
   coordinates.  */
SCM
Paper_def::font_descriptions () const
{
  SCM func = ly_scheme_function ("hash-table->alist");

  SCM l = SCM_EOL;
  for (SCM s = scm_call_1 (func, scaled_fonts_); ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM entry = ly_car (s);
      for (SCM t = ly_cdr (entry); ly_c_pair_p (t); t  = ly_cdr (t))
	{
	  Font_metric *fm= unsmob_metrics (ly_cdar (t));

	  if (dynamic_cast<Modified_font_metric*> (fm))
	    l = scm_cons (fm->self_scm (), l);
	}
    }
  return l;
}

Paper_def* 
unsmob_paper (SCM x)
{
  return dynamic_cast<Paper_def*> (unsmob_music_output_def (x));
}
  

LY_DEFINE (ly_paper_def_p, "ly:paper-def?",
	   1, 0, 0, (SCM def),
	   "Is @var{def} a paper definition?")
{
  Paper_def *op = dynamic_cast<Paper_def*> (unsmob_music_output_def (def));

  bool pap = op;
  return ly_bool2scm (pap);
}

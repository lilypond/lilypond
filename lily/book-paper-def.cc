/* 
  book-paper-def.cc -- implement Output_def

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dimensions.hh"
#include "font-metric.hh"
#include "ly-module.hh"
#include "output-def.hh"
#include "modified-font-metric.hh"
#include "virtual-font-metric.hh"

Real
output_scale (Output_def *od)
{
  return scm_to_double (od->lookup_variable (ly_symbol2scm ("outputscale")));
}

/* TODO: should add nesting for Output_def here too. */
Font_metric *
find_scaled_font (Output_def *mod,
		  Font_metric *f, Real m, SCM input_enc_name)
{
  if (mod->parent_)
    return find_scaled_font (mod->parent_, f, m, input_enc_name);
  
  Real lookup_mag = m;
  if (!dynamic_cast<Virtual_font_metric*> (f))
    lookup_mag /= output_scale (mod);

  SCM font_table = mod->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  if (scm_hash_table_p (font_table) != SCM_BOOL_T)
    {
      font_table = scm_c_make_hash_table (11);
      mod->set_variable (ly_symbol2scm ("scaled-fonts"), font_table);
    }

  
  SCM sizes = scm_hashq_ref (font_table, f->self_scm (), SCM_BOOL_F);
  if (sizes != SCM_BOOL_F)
    {
      SCM met = scm_assoc (scm_make_real (lookup_mag), sizes);
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
	  Font_metric *scaled = find_scaled_font (mod,
						  unsmob_metrics (ly_car (s)),
						  m, input_enc_name);
	  *t = scm_cons (scaled->self_scm (), SCM_EOL);
	  t = SCM_CDRLOC (*t);
	}

      vf = new Virtual_font_metric (lst);
      val = vf->self_scm ();
    }
  else
    {
      val = Modified_font_metric::make_scaled_font_metric (input_enc_name,
							   f, lookup_mag);
    }

  sizes = scm_acons (scm_make_real (lookup_mag), val, sizes);
  scm_gc_unprotect_object (val);
  scm_hashq_set_x (font_table, f->self_scm (), sizes);
  return unsmob_metrics (val);
}

/* TODO: this is a nasty interface. During formatting,
   the Output_def should be scaled to the output_scale_
   specified in the toplevel Output_def.  */
Output_def * 
scale_output_def (Output_def *o, Real amount)
{
  SCM proc = ly_scheme_function ("scale-paper");
  SCM new_pap = scm_call_2 (proc, o->self_scm (), scm_double2num (amount));
  scm_gc_protect_object (new_pap);

  return unsmob_output_def (new_pap);
}

LY_DEFINE (ly_bookpaper_fonts, "ly:bookpaper-fonts",
	   1, 0, 0,
	   (SCM bp),
	   "Return fonts scaled up BP")
{
  Output_def *b = unsmob_output_def (bp);

  SCM font_table = b->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  
  SCM_ASSERT_TYPE (b, bp, SCM_ARG1, __FUNCTION__, "bookpaper");

  SCM ell = SCM_EOL;
  if (scm_hash_table_p (font_table) == SCM_BOOL_T)
    {
      SCM func = ly_scheme_function ("hash-table->alist");

      for (SCM s = scm_call_1 (func, font_table); ly_c_pair_p (s);
	   s = ly_cdr (s))
	{
	  SCM entry = ly_car (s);
	  for (SCM t = ly_cdr (entry); ly_c_pair_p (t); t = ly_cdr (t))
	    {
	      Font_metric *fm = unsmob_metrics (ly_cdar (t));

	      if (dynamic_cast<Modified_font_metric*> (fm))
		ell = scm_cons (fm->self_scm (), ell);
	    }
	}
    }
  return ell;
}

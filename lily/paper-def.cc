/* 
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dimensions.hh"
#include "output-def.hh"
#include "modified-font-metric.hh"
#include "pango-font.hh"

Real
output_scale (Output_def *od)
{
  return scm_to_double (od->lookup_variable (ly_symbol2scm ("outputscale")));
}


SCM
get_font_table (Output_def *def)
{
  SCM font_table = def->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  if (scm_hash_table_p (font_table) != SCM_BOOL_T)
    {
      font_table = scm_c_make_hash_table (11);
      def->set_variable (ly_symbol2scm ("scaled-fonts"), font_table);
    }
  return font_table;
}
  


/* TODO: should add nesting for Output_def here too. */
Font_metric *
find_scaled_font (Output_def *mod, Font_metric *f, Real m)
{
  if (mod->parent_)
    return find_scaled_font (mod->parent_, f, m);
  
  Real lookup_mag = m / output_scale (mod);
  
  SCM font_table = get_font_table (mod);
  SCM sizes = scm_hashq_ref (font_table, f->self_scm (), SCM_BOOL_F);
  if (sizes != SCM_BOOL_F)
    {
      SCM met = scm_assoc (scm_make_real (lookup_mag), sizes);
      if (scm_is_pair (met))
	return unsmob_metrics (scm_cdr (met));
    }
  else
    sizes = SCM_EOL;
  
  SCM val = Modified_font_metric::make_scaled_font_metric (f, lookup_mag);
  
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
  SCM proc = ly_lily_module_constant ("scale-layout");
  SCM new_pap = scm_call_2 (proc, o->self_scm (), scm_double2num (amount));
  scm_gc_protect_object (new_pap);

  return unsmob_output_def (new_pap);
}


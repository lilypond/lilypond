/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dimensions.hh"
#include "output-def.hh"
#include "modified-font-metric.hh"
#include "pango-font.hh"
#include "all-font-metrics.hh"

Real
output_scale (Output_def *od)
{
  return scm_to_double (od->lookup_variable (ly_symbol2scm ("output-scale")));
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

SCM
get_pango_font_table (Output_def *def)
{
  SCM font_table = def->lookup_variable (ly_symbol2scm ("pango-fonts"));
  if (scm_hash_table_p (font_table) != SCM_BOOL_T)
    {
      font_table = scm_c_make_hash_table (11);
      def->set_variable (ly_symbol2scm ("pango-fonts"), font_table);
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
  SCM sizes = scm_hashq_ref (font_table, f->self_scm (), SCM_EOL);
  SCM handle = scm_assoc (scm_from_double (lookup_mag), sizes);
  if (scm_is_pair (handle))
    return unsmob_metrics (scm_cdr (handle));

  SCM val = Modified_font_metric::make_scaled_font_metric (f, lookup_mag);

  sizes = scm_acons (scm_from_double (lookup_mag), val, sizes);
  unsmob_metrics (val)->unprotect ();
  scm_hashq_set_x (font_table, f->self_scm (), sizes);
  return unsmob_metrics (val);
}

Font_metric *
find_pango_font (Output_def *layout, SCM descr, Real factor)
{
  if (layout->parent_)
    return find_pango_font (layout->parent_, descr, factor);

  SCM table = get_pango_font_table (layout);
  SCM sizes = scm_hash_ref (table, descr, SCM_EOL);
  SCM size_key = scm_from_double (factor);
  SCM handle = scm_assoc (size_key, sizes);
  if (scm_is_pair (handle))
    return unsmob_metrics (scm_cdr (handle));

  PangoFontDescription *description
    = pango_font_description_from_string (scm_i_string_chars (descr));

  pango_font_description_set_size (description,
				   gint (factor *
					 pango_font_description_get_size (description)));

  
  Font_metric *fm = all_fonts_global->find_pango_font (description,
						       output_scale (layout));

  pango_font_description_free (description);
 
  sizes = scm_acons (size_key, fm->self_scm (), sizes);
  scm_hash_set_x (table, descr, sizes);

  return fm;
}

/* TODO: this is a nasty interface. During formatting,
   the Output_def should be scaled to the output_scale_
   specified in the toplevel Output_def.  */
Output_def *
scale_output_def (Output_def *o, Real amount)
{
  SCM proc = ly_lily_module_constant ("scale-layout");
  SCM new_pap = scm_call_2 (proc, o->self_scm (), scm_double2num (amount));

  o = unsmob_output_def (new_pap);
  o->protect ();
  return o;
}


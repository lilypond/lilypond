/*
  font-select.cc -- implement property -> font_metric routines. 

  source file of the GNU LilyPond music typesetter

  (c) 2003--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <math.h>

#include "dimensions.hh"
#include "all-font-metrics.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "warn.hh"

LY_DEFINE (ly_paper_get_font, "ly:paper-get-font", 2, 0, 0,
	   (SCM paper_smob, SCM chain),

	   "Return a font metric satisfying the font-qualifiers "
	   "in the alist chain @var{chain}.\n"
	   "(An alist chain is a list of alists, "
	   "containing grob properties).\n")
{
  Output_def *paper = unsmob_output_def (paper_smob);
  SCM_ASSERT_TYPE (paper, paper_smob, SCM_ARG1,
		   __FUNCTION__, "paper definition");
  
  Font_metric *fm = select_font (paper, chain);
  return fm->self_scm ();
}

LY_DEFINE (ly_paper_get_number, "ly:paper-get-number", 2, 0, 0,
	   (SCM layout_smob, SCM name),
	   "Return the layout variable @var{name}.")
{
  Output_def *layout = unsmob_output_def (layout_smob);
  SCM_ASSERT_TYPE (layout, layout_smob, SCM_ARG1,
		   __FUNCTION__, "layout definition");
  return scm_make_real (layout->get_dimension (name));
}

bool
wild_compare (SCM field_val, SCM val)
{
  return (val == SCM_BOOL_F
	  || field_val == ly_symbol2scm ("*")
	  || field_val == val);
}

Font_metric *
get_font_by_design_size (Output_def *layout, Real requested,
			 SCM font_vector)
{
  int n = SCM_VECTOR_LENGTH (font_vector);
  Real size = 1e6;
  Real last_size = -1e6;
  int i = 0;

  for (; i < n; i++)
    {
      SCM entry = SCM_VECTOR_REF (font_vector, i);
      Font_metric *fm = unsmob_metrics (scm_force (entry));
      size = fm->design_size ();

      if (size > requested)
	break;
      last_size = size;
    }

  if (i == n)
    i = n - 1;
  else if (i > 0)
    {
      if ((requested / last_size) < (size / requested))
	{
	  i--;
	  size = last_size;
	}
    }

  Font_metric *fm = unsmob_metrics (scm_force (SCM_VECTOR_REF (font_vector,
							       i)));
  return find_scaled_font (layout, fm, requested / size);
}

Font_metric *
get_font_by_mag_step (Output_def *layout, Real requested_step,
		      SCM font_vector, Real default_size)
{
  return get_font_by_design_size (layout, default_size
				  * pow (2.0, requested_step / 6.0),
				  font_vector);
}

SCM
properties_to_font_size_family (SCM fonts, SCM alist_chain)
{
  return scm_call_2 (ly_lily_module_constant ("lookup-font"), fonts, alist_chain);
}

Font_metric *
select_encoded_font (Output_def *layout, SCM chain)
{
  SCM name = ly_chain_assoc (ly_symbol2scm ("font-name"), chain);

  if (!scm_is_pair (name) || !scm_is_string (scm_cdr (name)))
    {
      SCM fonts = layout->lookup_variable (ly_symbol2scm ("fonts"));
      name = properties_to_font_size_family (fonts, chain);
    }
  else
    name = scm_cdr (name);

  if (scm_is_string (name))
    {
      SCM mag = ly_chain_assoc (ly_symbol2scm ("font-magnification"), chain);
      Real rmag = (scm_is_pair (mag)
		   ? robust_scm2double (scm_cdr (mag), 1.0)
		   : 1);
      Font_metric *fm = all_fonts_global->find_font (ly_scm2string (name));
		
      return find_scaled_font (layout, fm, rmag);
    }
  else if (scm_instance_p (name))
    {
      SCM base_size  = scm_slot_ref (name, ly_symbol2scm ("default-size"));
      SCM vec = scm_slot_ref (name, ly_symbol2scm ("size-vector"));

      SCM font_size = ly_chain_assoc (ly_symbol2scm ("font-size"), chain);
      Real req = 0;
      if (scm_is_pair (font_size))
	req = scm_to_double (scm_cdr (font_size));

      return get_font_by_mag_step (layout, req, vec,
				   scm_to_double (base_size));
    }

  assert (0);
  return 0;
}

Font_metric *
select_font (Output_def *layout, SCM chain)
{
  return select_encoded_font (layout, chain);
}

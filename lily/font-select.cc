/*   
  font-select.cc -- implement property -> font_metric routines. 

  source file of the GNU LilyPond music typesetter

  (c) 2003--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include <math.h>

#include "book-paper-def.hh"
#include "all-font-metrics.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "warn.hh"

LY_DEFINE (ly_paper_get_font, "ly:paper-get-font", 2, 0, 0,
	   (SCM paper, SCM chain),

	   "Return a font metric satisfying the font-qualifiers "
	   "in the alist chain @var{chain}.\n"
	   "(An alist chain is a list of alists, containing grob properties).\n")
{
  Output_def *pap = unsmob_output_def (paper);
  SCM_ASSERT_TYPE (pap, paper, SCM_ARG1, __FUNCTION__, "paper definition");
  
  Font_metric *fm = select_font (pap, chain);
  return fm->self_scm ();
}

LY_DEFINE (ly_paper_get_number, "ly:paper-get-number", 2, 0, 0,
	   (SCM paper, SCM name),
	   "Return the paper variable @var{name}.")
{
  Output_def *pap = unsmob_output_def (paper);
  SCM_ASSERT_TYPE (pap, paper, SCM_ARG1, __FUNCTION__, "paper definition");
  return scm_make_real (pap->get_dimension (name));
}

bool
wild_compare (SCM field_val, SCM val)
{
  return (val == SCM_BOOL_F || field_val == ly_symbol2scm ("*") || field_val == val);
}


/*
  TODO: this triggers a great number of font-loads (feta11 upto
  parmesan23). We could make a Delayed_load_font_metric for which the
  design size is specced in advance.
 */
Font_metric*
get_font_by_design_size (Output_def* paper, Real requested,
			 SCM font_vector, SCM input_encoding_name)
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
	break ;
      last_size = size; 
    }

  if (i == n)
    {
      i = n-1;
    }
  else if (i > 0)
    {
      if ((requested / last_size) < (size / requested))
	{
	  i -- ;
	  size = last_size;
	}
    }

  Font_metric *fm = unsmob_metrics (scm_force (SCM_VECTOR_REF (font_vector, i)));
  return dynamic_cast<Book_output_def*> (paper->parent_)
    // ugh.
    ->find_scaled_font (fm, requested / size, input_encoding_name);

}


Font_metric*
get_font_by_mag_step (Output_def* paper, Real requested_step,
		      SCM font_vector, Real default_size, SCM input_encoding_name)
{
  return get_font_by_design_size (paper,
				  default_size * pow (2.0, requested_step / 6.0),
				  font_vector, input_encoding_name);
}



SCM
properties_to_font_size_family (SCM fonts, SCM alist_chain)
{
  return scm_call_2 (ly_scheme_function ("lookup-font"), fonts, alist_chain);
}


Font_metric *
select_encoded_font (Output_def *paper, SCM chain, SCM encoding_name)
{
  SCM name = ly_assoc_chain (ly_symbol2scm  ("font-name"), chain);
  
  if (!ly_c_pair_p (name) || !ly_c_string_p (ly_cdr (name)))
    {
      SCM fonts = paper->lookup_variable (ly_symbol2scm ("fonts"));
      name = properties_to_font_size_family (fonts, chain);
    }
  else
    name  = ly_cdr (name);

  if (ly_c_string_p (name))
    {
      SCM mag = ly_assoc_chain (ly_symbol2scm ("font-magnification"), chain);
  
      Real rmag = ly_c_pair_p (mag) ? robust_scm2double (ly_cdr (mag), 1.0) : 1;

      Font_metric * fm = all_fonts_global->find_font (ly_scm2string (name));
      
      
      return dynamic_cast<Book_output_def*> (paper->parent_)
	->find_scaled_font (fm, rmag, encoding_name);
    }
  else if (scm_instance_p (name))
    {
      SCM base_size  = scm_slot_ref (name, ly_symbol2scm ("default-size"));
      SCM vec = scm_slot_ref (name, ly_symbol2scm ("size-vector"));
      
      SCM font_size = ly_assoc_chain (ly_symbol2scm ("font-size"), chain);
      Real req = 0.0;
      if (ly_c_pair_p (font_size))
	req = ly_scm2double (ly_cdr (font_size));

      return get_font_by_mag_step (paper, req,
				   vec, ly_scm2double (base_size), encoding_name);
    }

  assert (0);

  return 0;
}

Font_metric *
select_font (Output_def *paper, SCM chain)
{
  return select_encoded_font (paper, chain, SCM_EOL);
}

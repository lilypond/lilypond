/*   
  font-select.cc -- implement property -> font_metric routines. 

  source file of the GNU LilyPond music typesetter

  (c) 2003--2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include <math.h>

#include "paper-def.hh"
#include "font-interface.hh"
#include "warn.hh"

LY_DEFINE (ly_paper_get_font, "ly:paper-get-font", 2, 0, 0,
	   (SCM paper, SCM chain),

	   "Return a font metric satisfying the font-qualifiers "
	   "in the alist chain @var{chain}.\n"
	   "(An alist chain is a list of alists, containing grob properties).\n")
{
  Paper_def *pap = unsmob_paper (paper);
  SCM_ASSERT_TYPE (pap, paper, SCM_ARG1, __FUNCTION__, "paper definition");
  
  Font_metric *fm = select_font (pap, chain);
  return fm->self_scm ();
}

LY_DEFINE (ly_paper_get_number, "ly:paper-get-number", 2, 0, 0,
	   (SCM paper, SCM name),
	   "Return the paper variable @var{name}.")
{
  Paper_def *pap = unsmob_paper (paper);
  SCM_ASSERT_TYPE (pap, paper, SCM_ARG1, __FUNCTION__, "paper definition");
  return gh_double2scm (pap->get_realvar (name));
}

bool
wild_compare (SCM field_val, SCM val)
{
  return (val == SCM_BOOL_F || field_val == ly_symbol2scm ("*") || field_val == val);
}

Font_metric*
get_font_by_design_size (Paper_def* paper, Real requested,
			 SCM font_vector)
{
  int n = SCM_VECTOR_LENGTH (font_vector);
  Real size = 1e6;
  Real last_size = -1e6;
  int i = 0;
  
  for (; i < n; i++)
    {
      size = gh_scm2double (gh_car (SCM_VECTOR_REF (font_vector, i)));
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
  
  return paper->find_font (gh_cdr (SCM_VECTOR_REF (font_vector, i)),
			   requested / size);
}


Font_metric*
get_font_by_mag_step (Paper_def* paper, Real requested_step,
		      SCM font_vector, Real default_size)
{
  return get_font_by_design_size (paper,
				  default_size * pow (2.0, requested_step / 6.0),
				  font_vector);
}



SCM
properties_to_font_size_family (SCM fonts, SCM alist_chain)
{
  static SCM proc;
  if (!proc )
    proc = scm_c_eval_string ("lookup-font");

  return scm_call_2 (proc, fonts, alist_chain);
}


Font_metric *
select_font (Paper_def *paper, SCM chain)
{
  SCM name = ly_assoc_chain (ly_symbol2scm  ("font-name"), chain);
  
  if (!gh_pair_p (name) || !gh_string_p (gh_cdr (name)))
    {
      SCM fonts = paper->lookup_variable (ly_symbol2scm ("fonts"));
      name = properties_to_font_size_family (fonts, chain);
    }
  else
    name  = gh_cdr (name);


  if (gh_string_p (name))
    {
      SCM mag = ly_assoc_chain (ly_symbol2scm ("font-magnification"), chain);
  
      Real rmag = gh_pair_p (mag) ? robust_scm2double (gh_cdr (mag), 1.0) : 1;
  
      return paper->find_font (name, rmag);
    }
  else if (gh_pair_p (name)) // (DEFAULT . FONT-VEC) pair
    {
      SCM vec = gh_cdr (name);
      SCM base_size = gh_car (name);
      
      SCM font_size = ly_assoc_chain (ly_symbol2scm ("font-size"), chain);
      Real req = 0.0;
      if (gh_pair_p (font_size))
	req = gh_scm2double (ly_cdr (font_size));

      return get_font_by_mag_step (paper, req,
				   vec, gh_scm2double (base_size));
    }

  assert (0);

  return 0;
}



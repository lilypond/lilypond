#include "paper-def.hh"
#include "font-interface.hh"
#include "warn.hh"


/*
  TODO revise font handling.


* relative sizes should relate to staff-space, eg.  font-staff-space
= 1.2 ^ relative-size

* If a relative size is given, lily should magnify the closest
design size font to match that. (ie. fonts should have variable
scaling)

(this requires that fonts are stored as (filename , designsize))


  
 */




LY_DEFINE(ly_paper_get_font,"ly:paper-get-font", 2, 0, 0,
	  (SCM paper, SCM chain),
	  "Return a font metric satisfying the font-qualifiers in the alist chain @var{chain}.\n"
"\n"
"The font object represents the metric information of a font. Every font\n"
"that is loaded into LilyPond can be accessed via Scheme. \n"
"\n"
"LilyPond only needs to know the dimension of glyph to be able to process\n"
"them. This information is stored in font metric files. LilyPond can read\n"
"two types of font-metrics: @TeX{} Font Metric files (TFM files) and\n"
"Adobe Font Metric files (AFM files).  LilyPond will always try to load\n"
"AFM files first since they are more versatile.\n"
"\n"
"An alist chain is a list of alists, containing grob properties.\n")
{
  Paper_def *pap = unsmob_paper (paper);
  SCM_ASSERT_TYPE(pap, paper, SCM_ARG1, __FUNCTION__, "paper definition");
  
  Font_metric*fm = select_font (pap, chain);
  return fm->self_scm();
}


bool
wild_compare (SCM field_val, SCM val)
{
  return (val == SCM_BOOL_F || field_val == ly_symbol2scm ("*") || field_val == val);
}

/*
  We can probably get more efficiency points if we preprocess FONTS
  to make lookup easier.
 */
SCM
properties_to_font_name (SCM fonts, SCM alist_chain)
{
  SCM shape = SCM_BOOL_F;
  SCM family = SCM_BOOL_F;
  SCM series = SCM_BOOL_F;

  
  SCM point_sz = ly_assoc_chain (ly_symbol2scm ("font-design-size"), alist_chain);
  SCM rel_sz = SCM_BOOL_F;

  shape = ly_assoc_chain (ly_symbol2scm ("font-shape"), alist_chain);
  family = ly_assoc_chain (ly_symbol2scm ("font-family"), alist_chain);
  series = ly_assoc_chain (ly_symbol2scm ("font-series"), alist_chain);

  if (gh_pair_p (shape))
    shape = ly_cdr (shape);
  if (gh_pair_p (family))
    family = ly_cdr (family);
  if (gh_pair_p (series))
    series = ly_cdr (series);


  if (gh_pair_p (point_sz))
    point_sz = ly_cdr (point_sz);
  else
    {
      rel_sz = ly_assoc_chain (ly_symbol2scm ("font-relative-size"), alist_chain);
      if (gh_pair_p (rel_sz))
	rel_sz = ly_cdr (rel_sz);
    }

  for (SCM s = fonts ; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM qlist = ly_caar (s);

      if (!wild_compare (scm_list_ref (qlist, gh_int2scm (1)), series))
	continue;
      if (!wild_compare (scm_list_ref (qlist, gh_int2scm (2)), shape))
	continue;
      if (!wild_compare (scm_list_ref (qlist, gh_int2scm (3)), family))
	continue;
  
      if (point_sz == SCM_BOOL_F && !wild_compare (ly_car (qlist), rel_sz))
	continue;
          
      SCM qname = ly_cdar (s);
      return qname;
    }

  warning (_ ("couldn't find any font satisfying "));
  scm_write (scm_list_n (point_sz, shape, series , family, rel_sz,
			 SCM_UNDEFINED), scm_current_error_port ());
  scm_flush (scm_current_error_port ());
 
  return scm_makfrom0str ("cmr10");
  
}


Font_metric *
select_font (Paper_def *paper, SCM chain)
{
  SCM name = ly_assoc_chain (ly_symbol2scm  ("font-name"), chain);
  
  if (!gh_pair_p (name) || !gh_string_p (gh_cdr (name)))
    {
      SCM fonts = paper->lookup_variable (ly_symbol2scm ("fonts"));
      name = properties_to_font_name (fonts, chain);
    }
  else
    name  = gh_cdr (name);
  
  SCM mag = ly_assoc_chain (ly_symbol2scm ("font-magnification"), chain);
  
  Real rmag = gh_pair_p (mag) && gh_number_p (gh_cdr (mag))
    ? gh_scm2double (gh_cdr (mag)) : 1.0;
  
  Font_metric *fm = paper->find_font (name, rmag);
  return fm;
}

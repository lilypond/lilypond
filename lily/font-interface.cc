/*   
  font-interface.cc --  implement Font_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "all-font-metrics.hh"
#include "font-metric.hh"
#include "font-interface.hh"
#include "score-element.hh"
#include "paper-def.hh"
#include "warn.hh"


SCM
Font_interface::font_alist_chain (Score_element *me)
{
  SCM defaults = gh_cdr (scm_assoc (ly_symbol2scm ("font-defaults"),
				    me->paper_l ()->style_sheet_ ));

  SCM ch = gh_list (me->mutable_property_alist_,
		    me->immutable_property_alist_,
		    defaults,
		    SCM_UNDEFINED);

  return ch;
}

/*
  todo: split up this func, reuse in text_item? 
 */
Font_metric *
Font_interface::get_default_font (Score_element*me)
{
  Font_metric * fm =  unsmob_metrics (me->get_elt_property ("font"));
  if (fm)
    return fm;

  fm = get_font (me,  font_alist_chain (me));
  me->set_elt_property ("font", fm->self_scm ());
  return fm;
}

Font_metric *
Font_interface::get_font (Score_element *me, SCM chain)
{
  
  SCM ss = me->paper_l ()->style_sheet_;

  SCM proc = gh_cdr (scm_assoc (ly_symbol2scm ("properties-to-font"),
				ss));

  SCM fonts = gh_cdr (scm_assoc (ly_symbol2scm ("fonts"), ss));

  assert (gh_procedure_p (proc));
  SCM font_name = gh_call2 (proc, fonts, chain);

  Font_metric *fm = me->paper_l ()->find_font (font_name, 1.0);

  return fm;
}

SCM
Font_interface::add_style (Score_element* me, SCM style, SCM chain)
{
  assert (gh_symbol_p (style));
  
  SCM sheet = me->paper_l ()->style_sheet_;
      
  SCM style_alist = gh_cdr (scm_assoc (ly_symbol2scm ("style-alist"), sheet));
  SCM entry = scm_assoc (style, style_alist);
  if (gh_pair_p (entry))
    {
      chain = gh_cons (gh_cdr (entry), chain);
    }
  return chain;
}

/*
SCM routines:  

Interpreting music...
MIDI output to wtk1-fugue2.midi...
Track ... 

real	0m31.862s
user	0m29.110s
sys	0m0.260s

real	0m26.964s
user	0m24.850s
sys	0m0.280s


so a 14% speedup.

*/

static SCM name_sym, shape_sym, family_sym, series_sym, rel_sz_sym, pt_sz_sym;


static void
init_syms ()
{
  name_sym = scm_permanent_object (ly_symbol2scm ("font-name"));
  shape_sym  = scm_permanent_object (ly_symbol2scm ("font-shape"));
  family_sym = scm_permanent_object (ly_symbol2scm ("font-family"));
  series_sym = scm_permanent_object (ly_symbol2scm ("font-series"));
  rel_sz_sym = scm_permanent_object (ly_symbol2scm ("font-relative-size"));
  pt_sz_sym = scm_permanent_object (ly_symbol2scm ("font-point-size"));
}


ADD_SCM_INIT_FUNC(Font_interface_syms,init_syms);


MAKE_SCHEME_CALLBACK(Font_interface,properties_to_font_name,2);
SCM
Font_interface::properties_to_font_name (SCM fonts, SCM alist_chain)
{
  SCM name = ly_assoc_chain (name_sym, alist_chain);

  SCM shape = SCM_BOOL_F;
  SCM family = SCM_BOOL_F;
  SCM series = SCM_BOOL_F;

  
  SCM point_sz = ly_assoc_chain (pt_sz_sym, alist_chain);
  SCM rel_sz = SCM_BOOL_F;

  if (!gh_pair_p (name))
    {
       shape = ly_assoc_chain (shape_sym, alist_chain);
       family = ly_assoc_chain (family_sym, alist_chain);
       series = ly_assoc_chain (series_sym, alist_chain);

       if (gh_pair_p (shape))
	 shape = gh_cdr (shape);
       if (gh_pair_p (family))
	 family = gh_cdr (family);
       if (gh_pair_p (series))
	 series = gh_cdr (series);
    }
  else
    name = gh_cdr (name);


  if (gh_pair_p (point_sz))
    point_sz = gh_cdr (point_sz);
  else
    {
      rel_sz = ly_assoc_chain (rel_sz_sym, alist_chain);
      if (gh_pair_p (rel_sz))
	rel_sz = gh_cdr (rel_sz);
    }

  for (SCM s = fonts ; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM qlist = gh_caar (s);

      if (name != SCM_BOOL_F)
	{
	  if (scm_list_ref (qlist, gh_int2scm (4)) != name)
	    continue;
	}
      else
	{
	  if (series != SCM_BOOL_F
	      && scm_list_ref (qlist, gh_int2scm (1)) != series)
	    continue;
	  if (shape != SCM_BOOL_F
	      && scm_list_ref (qlist, gh_int2scm (2)) != shape)
	    continue;
	  if (family != SCM_BOOL_F
	      && scm_list_ref (qlist, gh_int2scm (3)) != family)
	    continue;
	}
  
      if (point_sz != SCM_BOOL_F)
	{
	  if (scm_list_ref (qlist, gh_int2scm (4)) != name)
	    continue;
	}
      else
	{
	  if (rel_sz != SCM_BOOL_F
	      && gh_car (qlist) != rel_sz)
	    continue;
	}

      
      SCM qname = gh_cdar (s);
      return qname;
    }

  warning (_("couldn't find any font satisfying ") );
  scm_write (gh_list (name, point_sz, shape, series , family, rel_sz, SCM_UNDEFINED), scm_current_error_port ());

  return gh_str02scm ("cmr10");
  
}

/*   
  all-font-metrics.cc --  implement All_font_metrics
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "main.hh"
#include "all-font-metrics.hh"
#include "debug.hh"
#include "warn.hh"
#include "afm.hh"
#include "tfm.hh"
#include "dictionary-iter.hh"
#include "lily-guile.hh"
#include "tfm-reader.hh"

const char * default_font_sz_ = "cmr10";

All_font_metrics::All_font_metrics (String path)
{
  search_path_.parse_path (path);
}

Adobe_font_metric *
All_font_metrics::find_afm (String name)
{
  if (!afm_p_dict_.elem_b (name))
    {
      String path = name  + ".afm";
      path = search_path_.find (path);
      if (path.empty_b ())
	return 0;
      
      *mlog << "[" << path;
      Adobe_font_metric
	* afm_p = new Adobe_font_metric (read_afm_file (path));

      afm_p->name_str_ = name;
      
      *mlog << "]" << flush ;

      afm_p_dict_[name] = afm_p;
    }
  return afm_p_dict_[name];  
}

Scaled_font_metric * 
All_font_metrics::find_scaled (String nm, int m)
{
  Scaled_font_metric * s=0;
  String index =  nm + "@" + to_str (m);
  if (!scaled_p_dict_.elem_b (index))
    {
      Font_metric *f = find_font (nm);
      s = new Scaled_font_metric (f, m);
      scaled_p_dict_[index] = s;
      return s;  
    }
  else
    return scaled_p_dict_[index];
}

Tex_font_metric *
All_font_metrics::find_tfm (String name)
{
  if (!tfm_p_dict_.elem_b (name))
    {
      String path = name  + ".tfm";
      path = search_path_.find (path);
      if (path.empty_b ())
	return 0;

      *mlog << "[" << path;
      Tex_font_metric	* tfm_p = Tex_font_metric_reader::read_file (path);
      tfm_p->name_str_ = name;

      *mlog << "]" << flush ;

      tfm_p_dict_[name] = tfm_p;
    }
  return tfm_p_dict_[name];  
}


Font_metric *
All_font_metrics::find_font (String name)
{
  Font_metric * f=0;
  f = find_tfm (name);
  if (f)
    return f;

  f= find_afm (name);
  if (f)
    return f;

  warning (_f ("Can't find font: `%s'", name.ch_C ()));
  warning (_ ("Loading default font"));
  
  f =  find_tfm (default_font_sz_);
  if (f)
    return f;
  error (_f ("Can't find default font: `%s'", default_font_sz_));
  error (_f ("(search path: `%s'", search_path_.str ()));
  error (_ ("Giving up"));
}

SCM
All_font_metrics::font_descriptions () const
{
  SCM l = SCM_EOL;
  for (Dictionary_iter<Adobe_font_metric*> ai(afm_p_dict_); ai.ok (); ai++)
    l = gh_cons (ai.val ()->description (), l);
  for (Dictionary_iter<Tex_font_metric*> ai(tfm_p_dict_); ai.ok (); ai++)
    l = gh_cons(ai.val ()->description (), l);

  for (Dictionary_iter<Scaled_font_metric*> ai(scaled_p_dict_); ai.ok (); ai++)
    l = gh_cons (ai.val ()->description (),l);
  
  return l;
}

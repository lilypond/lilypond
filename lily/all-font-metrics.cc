
#include "main.hh"
#include "all-fonts.hh"
#include "debug.hh"
#include "warn.hh"
#include "afm.hh"
#include "tfm.hh"

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
      *mlog << "]" << flush ;

      afm_p_dict_[name] = afm_p;
    }
  return afm_p_dict_[name];  
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
      Tex_font_metric	* tfm_p = new Tex_font_metric;
      tfm_p->read_file (path);
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

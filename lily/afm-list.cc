#include "pointer.hh"
#include "main.hh"
#include "all-fonts.hh"
#include "debug.hh"
#include "warn.hh"
#include "afm.hh"

const char * default_font_sz_ = "cmr10";



All_font_metrics::All_font_metrics (String path)
{
  search_path_.parse_path (path);
  
  String f = default_font_sz_ + String (".afm");
  f = search_path_.find (f);
  if (f.empty_b ())
    error (_f("Can't find default font (PATH = %s)", path));
  

  afm_p_dict_[default_font_sz_] = new Adobe_font_metric (read_afm_file (f));
}

Adobe_font_metric *
All_font_metrics::find_font (String name)
{
  if (!afm_p_dict_.elem_b (name))
    {
      String path = name  + ".afm";
      path = search_path_.find (path);
      if (path.empty_b ())
	{
	  warning (_f ("Can't find `%s'", name));
	  return afm_p_dict_[default_font_sz_];
	}
      
      *mlog << "[" << path;
      Adobe_font_metric
	* afm_p = new Adobe_font_metric (read_afm_file (path));
      *mlog << "]" << flush ;

      afm_p_dict_[name] = afm_p;
    }

  return afm_p_dict_[name];
}



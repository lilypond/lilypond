/*   
  afm2.cc --  implement Adobe_font_metric
  
  source file of the Flower Library
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "afm.hh"
#include "warn.hh"

Adobe_font_metric::Adobe_font_metric (AFM_Font_info * fi)
{
  font_inf_ = fi;

  for (int i= 256; i--;)
    ascii_to_metric_idx_.push (-1);
  
  for (int i=0; i < fi->numOfChars; i++)
    {
      AFM_CharMetricInfo * c = fi->cmi + i;

      ascii_to_metric_idx_[c->code] = i;
      name_to_metric_dict_[c->name] = i;
    }
}
  

AFM_CharMetricInfo const *
Adobe_font_metric::find_ascii_metric (int a , bool warn) const
{
  if (ascii_to_metric_idx_[a] >=0)
    {
      int code = ascii_to_metric_idx_[a];
      if (code>=0)
	{
	  return font_inf_->cmi + code;
	}
    }
  else if (warn )
    {
      warning (_f ("can't find character number: %d", a));
    }

  return 0;
}

AFM_CharMetricInfo const *
Adobe_font_metric::find_char_metric (String nm, bool warn) const
{
  map<String,int>::const_iterator ai = name_to_metric_dict_.find (nm);
  
  if (ai == name_to_metric_dict_.end ())
    {
      if (warn)
	{
	  warning (_f ("can't find character called: `%s'", nm.ch_C()));
	}
      return 0;
    }
  else
    return font_inf_->cmi + (*ai).second;
}


Box
Adobe_font_metric::get_char (int code, bool warn) const
{
  AFM_CharMetricInfo const
    * c =  find_ascii_metric (code,warn);
  if (c)
    return afm_bbox_to_box (c->charBBox);			
  else
    return Box (Interval (0,0),Interval(0,0));
}

Adobe_font_metric*
read_afm_file (String nm)
{
  FILE *f = fopen (nm.ch_C() , "r");

  AFM_Font_info * fi;
  int ok = AFM_parseFile (f, &fi, ~1);

  if (ok)
    {
      error (_("Error parsing AFM file"));
      exit (2);
    }
  fclose (f);

  return new Adobe_font_metric (fi);
}

  
Box
afm_bbox_to_box (AFM_BBox bb)
{
  return Box (Interval (bb.llx, bb.urx)* (1/1000.0),
	      Interval (bb.lly, bb.ury)* (1/1000.0));

}
  

Adobe_font_metric::~Adobe_font_metric ()
{
  AFM_free (font_inf_);
}

/*   
  afm.cc --  implement Adobe_font_metric
  
  source file of the Flower Library
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "afm.hh"
#include "warn.hh"
#include "molecule.hh"

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


SCM
Adobe_font_metric::make_afm (AFM_Font_info *fi)
{
  Adobe_font_metric * fm = new Adobe_font_metric (fi);

  return fm->self_scm();    
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
Adobe_font_metric::get_char (int code) const
{
  AFM_CharMetricInfo const
    * c =  find_ascii_metric (code,false);
  Box b (Interval (0,0),Interval(0,0));
  if (c)
    b = afm_bbox_to_box (c->charBBox);			

  return b;
}

SCM
read_afm_file (String nm)
{
  FILE *f = fopen (nm.ch_C() , "r");

  AFM_Font_info * fi;
  int ok = AFM_parseFile (f, &fi, ~1);

  if (ok)
    {
      error (_f ("Error parsing AFM file: %s", nm.ch_C ()));
      exit (2);
    }
  fclose (f);

  return Adobe_font_metric::make_afm (fi);
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

/*
  return a molecule, without fontification 
 */
Molecule
Adobe_font_metric::find_by_name (String s) const
{
  AFM_CharMetricInfo const *cm = find_char_metric (s, false);

  if (!cm)
    {
      Molecule m;
      m.set_empty (false);
      return m;
    }
  
  SCM at =  (gh_list (ly_symbol2scm ("char"),
		      gh_int2scm (cm->code),
		      SCM_UNDEFINED));
  
  //  at= fontify_atom ((Font_metric*)this, at);
  Box b = afm_bbox_to_box (cm->charBBox);

  return Molecule (b, at);
}

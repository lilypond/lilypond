/*   
  afm.cc --  implement Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <string.h>

#include "warn.hh"
#include "libc-extension.hh"
#include "afm.hh"
#include "dimensions.hh"

Adobe_font_metric::Adobe_font_metric (AFM_Font_info *fi)
{
  checksum_ = 0;
  font_info_ = fi;
  design_size_ = 1.0;
  
  for (int i = 256 >? fi->numOfChars; i--;)
    ascii_to_metric_idx_.push (-1);
  
  for (int i=0; i < fi->numOfChars; i++)
    {
      AFM_CharMetricInfo * c = fi->cmi + i;

      /* Some TeX afm files contain code = -1.  We don't know why,
	let's ignore it.  */
      if (c->code >= 0)
	ascii_to_metric_idx_[c->code] = i;
      name_to_metric_dict_[c->name] = i;
    }
}

Adobe_font_metric::~Adobe_font_metric ()
{
  AFM_free (font_info_);
}

SCM
Adobe_font_metric::make_afm (AFM_Font_info *fi,
			     unsigned int checksum,
			     Real design_size)
{
  Adobe_font_metric *fm = new Adobe_font_metric (fi);
  fm->checksum_ = checksum;
  fm->design_size_ = design_size;
  return fm->self_scm ();    
}

AFM_CharMetricInfo const*
Adobe_font_metric::find_ascii_metric (int a) const
{
  if (ascii_to_metric_idx_[a] >=0)
    {
      int code = ascii_to_metric_idx_[a];
      if (code >= 0)
	return font_info_->cmi + code;
    }
  return 0;
}

AFM_CharMetricInfo const*
Adobe_font_metric::find_char_metric (String nm) const
{
  int idx = name_to_index (nm);
  if (idx >= 0)
    return font_info_->cmi + idx;
  return 0;
}

int
Adobe_font_metric::name_to_index (String name) const
{
  std::map<String,int>::const_iterator ai = name_to_metric_dict_.find (name);
  if (ai == name_to_metric_dict_.end ())
    return -1;
  return (*ai).second; 
}

int
Adobe_font_metric::count () const
{
  return font_info_->numOfChars;
}

Box
Adobe_font_metric::get_ascii_char (int code) const
{
  AFM_CharMetricInfo const *c = find_ascii_metric (code);
  Box b (Interval (0, 0), Interval (0, 0));
  if (c)
    b = afm_bbox_to_box (c->charBBox);		
  return b;
}

int
Adobe_font_metric::index_to_ascii (int code) const
{
  return font_info_->cmi[code].code;
}

Box
Adobe_font_metric::get_indexed_char (int code) const
{
  if (code >= 0)
    return afm_bbox_to_box (font_info_->cmi[code].charBBox);
  else
    return Box (Interval (0, 0), Interval (0, 0));
}

SCM
read_afm_file (String nm)
{
  FILE *f = fopen (nm.to_str0 () , "r");
  char s[2048] = "";
  char *check_key = "Comment TfmCheckSum";
  char *size_key = "Comment DesignSize";

  unsigned int cs = 0;
  Real ds = 1.0;
  
  /* Assume check_key in first 10 lines */
  for (int i = 0; i < 10; i++)
    {
      fgets (s, sizeof (s), f);
      if (strncmp (s, check_key, strlen (check_key)) == 0)
	sscanf (s + strlen (check_key), "%ud", &cs);
      else if (strncmp (s, size_key, strlen (size_key)) == 0)
	sscanf (s + strlen (size_key), "%lf", &ds);
    }
  
  rewind (f);
    
  AFM_Font_info *fi;
  int ok = AFM_parseFile (f, &fi, ~0);

  if (ok)
    {
      error (_f ("Error parsing AFM file: `%s'", nm.to_str0 ()));
      exit (2);
    }
  fclose (f);

  return Adobe_font_metric::make_afm (fi, cs, ds);
}


/* Actually, AFMs will be printers point, usually, but our .py script dumps
  real points.  */
Box
afm_bbox_to_box (AFM_BBox bb)
{
  return Box (Interval (bb.llx, bb.urx)* (1/1000.0) PT,
	      Interval (bb.lly, bb.ury)* (1/1000.0) PT);

}

Offset
Adobe_font_metric::get_indexed_wxwy (int k) const
{
  AFM_CharMetricInfo const *mi = font_info_->cmi+ k;
  return 1/1000.0 PT * Offset (mi->wx, mi->wy); 
}

Real
Adobe_font_metric::design_size () const
{
  return design_size_;    
}

String
Adobe_font_metric::coding_scheme () const
{
  return font_info_->gfi->encodingScheme;
}

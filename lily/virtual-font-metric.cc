/*   
  virtual-font-metric.cc --  implement Virtual_font_metric

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "virtual-font-metric.hh"
#include "all-font-metrics.hh"
#include "main.hh"
#include "molecule.hh"
#include "paper-def.hh"


/*
  passing DEF is ughish. Should move into paperdef?
  */
Virtual_font_metric::Virtual_font_metric (SCM name_list, 
					  Real mag,Paper_def*def)
{
  font_list_ = SCM_EOL;
  SCM *tail = &font_list_;
  for (SCM s = name_list; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM nm = gh_car (s);

      Font_metric *fm = def->find_font (nm, mag);
      *tail =  scm_cons (fm->self_scm(),SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
}

void
Virtual_font_metric::derived_mark()
{
  scm_gc_mark (font_list_);

}

int
Virtual_font_metric::count () const
{
  int k = 0;
  for (SCM s = font_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      k+= unsmob_metrics (gh_car (s))->count ();
    }

  return k;
}

Molecule
Virtual_font_metric::find_by_name (String glyph) const
{
  Molecule m;  
  for (SCM s = font_list_; m.empty_b () && gh_pair_p (s); s = gh_cdr (s))
    {
      m = unsmob_metrics (gh_car (s))->find_by_name (glyph);
    }

  return m;
}
  
  

Box
Virtual_font_metric::get_char (int code)  const
{
  int last_k = 0;
  for (SCM s = font_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      Font_metric* fm = unsmob_metrics (gh_car (s));
      int k = last_k + fm->count ();
      if (last_k <= code && code < k)
	{
	  return fm->get_char (code - last_k);
	}
      last_k = k;
    }

  
  return Box();
}
  
  
Molecule
Virtual_font_metric::get_char_molecule (int code)  const
{
  Molecule  m ;  
  int last_k = 0;
  for (SCM s = font_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      Font_metric* fm = unsmob_metrics (gh_car (s));
      int k = last_k + fm->count ();
      if (last_k <= code && code < k)
	{
	  m = fm->get_char_molecule (code - last_k);
	  break; 
	}
      last_k = k;
    }

  return m;
}
  

/*   
  virtual-font-metric.cc --  implement Virtual_font_metric

source file of the GNU LilyPond music typesetter

(c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "warn.hh"
#include "virtual-font-metric.hh"
#include "all-font-metrics.hh"
#include "stencil.hh"
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
Virtual_font_metric::derived_mark()const
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

Stencil
Virtual_font_metric::find_by_name (String glyph) const
{
  Stencil m;  
  for (SCM s = font_list_; m.is_empty () && gh_pair_p (s); s = gh_cdr (s))
    {
      m = unsmob_metrics (gh_car (s))->find_by_name (glyph);
    }

  return m;
}
  
  

Box
Virtual_font_metric::get_ascii_char (int)  const
{
  programming_error ("Virtual font metric cannot be indexed by ASCII.");
  return Box();
}

Stencil
Virtual_font_metric::get_ascii_char_stencil (int )  const
{
  programming_error ("Virtual font metric cannot be indexed by ASCII.");
  return Stencil();
}


Offset
Virtual_font_metric::get_indexed_wxwy (int code)  const
{
  int total = 0;
  for (SCM s = font_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      Font_metric* fm = unsmob_metrics (gh_car (s));
      if (code < total + fm->count ())
	{
	  return fm->get_indexed_wxwy (code - total);
	}
      total += fm->count ();
    }

  
  return Offset (0,0);
}

Box
Virtual_font_metric::get_indexed_char (int code)  const
{
  int total = 0;
  for (SCM s = font_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      Font_metric* fm = unsmob_metrics (gh_car (s));
      if (code < total + fm->count ())
	{
	  return fm->get_indexed_char (code - total);
	}
      total += fm->count ();
    }

  
  return Box();
}


int 
Virtual_font_metric::name_to_index (String glyph) const
{
  Stencil m;
  int total = 0; 
  for (SCM s = font_list_; m.is_empty () && gh_pair_p (s); s = gh_cdr (s))
    {
      Font_metric *m =unsmob_metrics (gh_car (s));
      int k = m->name_to_index (glyph);
      if (k >= 0)
	return total + k;

      total += m->count ();
    }

  return -1;
}

  
Stencil
Virtual_font_metric::get_indexed_char_stencil (int code)  const
{
  Stencil  m ;  
  int total = 0;
  
  for (SCM s = font_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      Font_metric* fm = unsmob_metrics (gh_car (s));
      if (code < total + fm->count())
	{
	  m = fm->get_indexed_char_stencil (code - total); // ugh.
	  break; 
	}
      total += fm->count ();
    }

  return m;
}
  

/*   
  virtual-font-metric.cc --  implement Virtual_font_metric

source file of the GNU LilyPond music typesetter

(c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "warn.hh"
#include "virtual-font-metric.hh"
#include "all-font-metrics.hh"
#include "stencil.hh"
#include "output-def.hh"


/*
  passing DEF is ughish. Should move into layoutdef?
  */
Virtual_font_metric::Virtual_font_metric (SCM font_list)
{
  font_list_ = SCM_EOL;
  SCM *tail = &font_list_;

  SCM mag = SCM_EOL;
  SCM name_list = SCM_EOL;
  SCM *name_tail = &name_list;
  
  for (SCM s = font_list; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Font_metric *fm = unsmob_metrics (scm_car (s)))
	{
	  *tail =  scm_cons (scm_car (s),SCM_EOL);
	  tail = SCM_CDRLOC (*tail);

	  if (!scm_is_number (mag))
	    /* Ugh.  */
	    mag = scm_cdr (fm->description_);

	  *name_tail = scm_cons (scm_car (fm->description_), SCM_EOL);
	  name_tail = SCM_CDRLOC (*name_tail);
	}
    }

  description_ = scm_cons (name_list, mag);
}

Real 
Virtual_font_metric::design_size () const
{
  return unsmob_metrics (scm_car (font_list_))-> design_size ();
}

void
Virtual_font_metric::derived_mark ()const
{
  scm_gc_mark (font_list_);
}

int
Virtual_font_metric::count () const
{
  int k = 0;
  for (SCM s = font_list_; scm_is_pair (s); s = scm_cdr (s))
      k += unsmob_metrics (scm_car (s))->count ();
  return k;
}

Stencil
Virtual_font_metric::find_by_name (String glyph) const
{
  Stencil m;  
  for (SCM s = font_list_; m.is_empty () && scm_is_pair (s); s = scm_cdr (s))
    {
      m = unsmob_metrics (scm_car (s))->find_by_name (glyph);
    }

  return m;
}

Box
Virtual_font_metric::get_ascii_char (int)  const
{
  programming_error ("Virtual font metric cannot be indexed by ASCII.");
  return Box ();
}

Stencil
Virtual_font_metric::get_ascii_char_stencil (int )  const
{
  programming_error ("Virtual font metric cannot be indexed by ASCII.");
  return Stencil ();
}

Offset
Virtual_font_metric::get_indexed_wxwy (int code)  const
{
  int total = 0;
  for (SCM s = font_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Font_metric *fm = unsmob_metrics (scm_car (s));
      if (code < total + fm->count ())
	return fm->get_indexed_wxwy (code - total);
      total += fm->count ();
    }
  return Offset (0,0);
}

Box
Virtual_font_metric::get_indexed_char (int code)  const
{
  int total = 0;
  for (SCM s = font_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Font_metric *fm = unsmob_metrics (scm_car (s));
      if (code < total + fm->count ())
	return fm->get_indexed_char (code - total);
      total += fm->count ();
    }
  return Box ();
}

int 
Virtual_font_metric::name_to_index (String glyph) const
{
  Stencil m;
  int total = 0; 
  for (SCM s = font_list_; m.is_empty () && scm_is_pair (s); s = scm_cdr (s))
    {
      Font_metric *m =unsmob_metrics (scm_car (s));
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
  
  for (SCM s = font_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Font_metric *fm = unsmob_metrics (scm_car (s));
      if (code < total + fm->count ())
	{
	  /* Ugh.  */
	  m = fm->get_indexed_char_stencil (code - total);
	  break; 
	}
      total += fm->count ();
    }
  return m;
}


SCM
Virtual_font_metric::get_font_list () const
{
  return font_list_;
}

LY_DEFINE (ly_make_virtual_font, "ly:make-virtual-font", 0, 0, 1,
	   (SCM args),
	   "Make a virtual font metric from @var{args}, "
	   "a list of font objects.")
{
  Virtual_font_metric *fm = new Virtual_font_metric (args);
  return scm_gc_unprotect_object (fm->self_scm ());
}

String
Virtual_font_metric::coding_scheme () const
{
  Font_metric *fm = unsmob_metrics (scm_car (font_list_));
  return fm->coding_scheme ();
}

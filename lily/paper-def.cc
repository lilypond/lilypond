/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <math.h>

#include "book-paper-def.hh"
#include "virtual-font-metric.hh"
#include "all-font-metrics.hh"
#include "string.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "scaled-font-metric.hh"
#include "main.hh"
#include "scm-hash.hh"
#include "paper-outputter.hh"
#include "ly-module.hh"

/*
  This is an almost empty thing. The only substantial thing this class
  handles is scaling up and down to real-world dimensions (internally
  dimensions are against global staff-space.)
 */

Paper_def::Paper_def ()
{
  bookpaper_ = 0;
}

Paper_def::Paper_def (Paper_def const&src)
  : Music_output_def (src)
{
  /* Do not remove this statement, scm_make_hash_table may trigger GC.  */
  bookpaper_ = 0;
}

Paper_def::~Paper_def ()
{
}

void
Paper_def::derived_mark ()
{
  if (bookpaper_)
    scm_gc_mark (bookpaper_->self_scm ());
}

Real
Paper_def::get_dimension (SCM s) const
{
  SCM val = lookup_variable (s);
  return ly_scm2double (val);
}

/* FIXME.  This is broken until we have a generic way of
   putting lists inside the \paper block.  */
Interval
Paper_def::line_dimensions_int (int n) const
{
  Real lw =  get_dimension (ly_symbol2scm ("linewidth"));
  Real ind = n? 0.0:get_dimension (ly_symbol2scm ("indent"));

  return Interval (ind, lw);
}

Paper_outputter*
Paper_def::get_paper_outputter (String outname) const
{
  progress_indication (_f ("paper output to `%s'...",
			   outname == "-" ? String ("<stdout>") : outname));
  return new Paper_outputter (outname);

}

Paper_def* 
unsmob_paper (SCM x)
{
  return dynamic_cast<Paper_def*> (unsmob_music_output_def (x));
}
  

LY_DEFINE (ly_paper_def_p, "ly:paper-def?",
	   1, 0, 0, (SCM def),
	   "Is @var{def} a paper definition?")
{
  Paper_def *op = dynamic_cast<Paper_def*> (unsmob_music_output_def (def));

  bool pap = op;
  return ly_bool2scm (pap);
}

/*
  paper-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "paper-score.hh"

#include "all-font-metrics.hh"
#include "gourlay-breaking.hh"
#include "main.hh"
#include "misc.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-column.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"

Paper_score::Paper_score ()
{
  layout_ = 0;
  system_ = 0;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}

void
Paper_score::typeset_line (System *system)
{
  if (!system_)
    system_ = system;

  systems_ = scm_cons (system->self_scm (), systems_);
  system->pscore_ = this;

  scm_gc_unprotect_object (system->self_scm ());
}

Array<Column_x_positions>
Paper_score::calc_breaking ()
{
  Break_algorithm *algorithm = 0;
  Array<Column_x_positions> sol;

  algorithm = new Gourlay_breaking;
  algorithm->set_pscore (this);
  sol = algorithm->solve ();
  delete algorithm;

  return sol;
}

SCM
Paper_score::process (String)
{
  if (be_verbose_global)
    progress_indication (_f ("Element count %d (spanners %d) ",
			     system_->element_count (),
			     system_->spanner_count ()));

  progress_indication (_ ("Preprocessing graphical objects...") + " ");

  /* FIXME: Check out why we need this - removing gives assertion failures
     down the road.

     doubly, also done in Score_engraver */
  Link_array<Grob> pc (system_->columns ());
  pc[0]->set_property ("breakable", SCM_BOOL_T);
  pc.top ()->set_property ("breakable", SCM_BOOL_T);

  system_->pre_processing ();

  Array<Column_x_positions> breaking = calc_breaking ();
  system_->break_into_pieces (breaking);
  SCM lines = system_->get_lines ();

  /*
    retain Grobs, since they are pointed to by the point & click data
    structures, and are not marked fully, because child -> parent
    links aren't marked.
   */
  return lines;
}

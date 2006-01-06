/*
  paper-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

Paper_score::Paper_score (Output_def *layout)
{
  layout_ = layout;
  system_ = 0;
  systems_ = SCM_EOL;
  paper_systems_ = SCM_EOL;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}

void
Paper_score::derived_mark () const
{
  if (layout_)
    scm_gc_mark (layout_->self_scm ());
  scm_gc_mark (systems_);
  scm_gc_mark (paper_systems_);
}

void
Paper_score::typeset_system (System *system)
{
  if (!system_)
    system_ = system;

  systems_ = scm_cons (system->self_scm (), systems_);
  system->pscore_ = this;
  system->layout_ = layout_;
  system->unprotect ();
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

void
Paper_score::process ()
{
  if (be_verbose_global)
    message (_f ("Element count %d (spanners %d) ",
		 system_->element_count (),
		 system_->spanner_count ()));

  message (_ ("Preprocessing graphical objects...") + " ");

  /* FIXME: Check out why we need this - removing gives assertion failures
     down the road.

     doubly, also done in Score_engraver */
  Link_array<Grob> pc (system_->columns ());
  pc[0]->set_property ("breakable", SCM_BOOL_T);
  pc.top ()->set_property ("breakable", SCM_BOOL_T);

  system_->pre_processing ();

  Array<Column_x_positions> breaking = calc_breaking ();
  system_->break_into_pieces (breaking);

  paper_systems_ = system_->get_paper_systems ();
}

System *
Paper_score::root_system () const
{
  return system_;
}

Output_def *
Paper_score::layout () const
{
  return layout_;
}

SCM
Paper_score::get_paper_systems () const
{
  return paper_systems_;
}

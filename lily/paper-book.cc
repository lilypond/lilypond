/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2021 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "paper-book.hh"

#include "grob.hh"
#include "international.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "text-interface.hh"
#include "warn.hh"
#include "program-option.hh"
#include "page-marker.hh"
#include "ly-module.hh"
#include "lily-imports.hh"

using std::string;
using std::vector;

Paper_book::Paper_book ()
{
  header_ = SCM_EOL;
  header_0_ = SCM_EOL;
  pages_ = SCM_BOOL_F;
  scores_ = SCM_EOL;
  bookparts_ = SCM_EOL;
  performances_ = SCM_EOL;
  systems_ = SCM_BOOL_F;

  paper_ = 0;
  parent_ = 0;
  smobify_self ();
}

Paper_book::~Paper_book ()
{
}

const char *const Paper_book::type_p_name_ = "ly:paper-book?";

SCM
Paper_book::mark_smob () const
{
  if (paper_)
    scm_gc_mark (paper_->self_scm ());
  if (parent_)
    scm_gc_mark (parent_->self_scm ());
  scm_gc_mark (header_);
  scm_gc_mark (header_0_);
  scm_gc_mark (pages_);
  scm_gc_mark (performances_);
  scm_gc_mark (scores_);
  scm_gc_mark (bookparts_);
  return systems_;
}

Output_def *
Paper_book::top_paper ()
{
  Output_def *paper = paper_;
  while (paper->parent_)
    paper = paper->parent_;
  return paper;
}

SCM
dump_fields ()
{
  SCM fields = SCM_EOL;
  for (vsize i = dump_header_fieldnames_global.size (); i--;)
    fields
      = scm_cons (ly_symbol2scm (dump_header_fieldnames_global[i].c_str ()),
                  fields);
  return fields;
}

void
Paper_book::add_score (SCM s)
{
  scores_ = scm_cons (s, scores_);
}

void
Paper_book::add_bookpart (SCM p)
{
  bookparts_ = scm_cons (p, bookparts_);
}

void
Paper_book::add_performance (SCM s)
{
  performances_ = scm_cons (s, performances_);
}

long
Paper_book::output_aux (SCM output_channel,
                        bool is_last,
                        long *first_page_number,
                        long *first_performance_number)
{
  long page_nb = 0;
  if (scm_is_pair (performances_))
    {
      Lily::write_performances_midis (performances (),
                                      output_channel,
                                      to_scm (*first_performance_number));
      *first_performance_number += scm_ilength (performances_);
    }

  if (scm_is_pair (bookparts_))
    {
      for (SCM p = bookparts_; scm_is_pair (p); p = scm_cdr (p))
        if (Paper_book *pbookpart = unsmob<Paper_book> (scm_car (p)))
          {
            bool is_last_part = (is_last && !scm_is_pair (scm_cdr (p)));
            page_nb += pbookpart->output_aux (output_channel,
                                              is_last_part,
                                              first_page_number,
                                              first_performance_number);
          }
    }
  else
    {
      if (scm_is_null (scores_))
        return 0;
      paper_->set_variable (ly_symbol2scm ("first-page-number"),
                            to_scm (*first_page_number));
      paper_->set_variable (ly_symbol2scm ("is-last-bookpart"),
                            ly_bool2scm (is_last));
      /* Generate all stencils to trigger font loads.  */
      page_nb = scm_ilength (pages ());
      *first_page_number += page_nb;
    }
  return page_nb;
}

void
Paper_book::output (SCM output_channel)
{
  long first_page_number
    = from_scm (paper_->c_variable ("first-page-number"), 1);
  long first_performance_number = 0;

  /* FIXME: We need a line-width for ps output (framework-ps.scm:92).
     If we don't have any, we take the paper-width unless we know
     better which line-width to choose (e.g. if there are \bookparts
     with different line-widths) and why we need it at all.
  */

  if (SCM_UNBNDP (paper_->c_variable ("line-width")))
    paper_->set_variable (ly_symbol2scm ("line-width"),
                          paper_->c_variable ("paper-width"));

  if (!output_aux (output_channel,
                   true,
                   &first_page_number,
                   &first_performance_number))
    return;

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  string mod_nm = "lily framework-" + get_output_backend_name ();

  SCM mod = scm_c_resolve_module (mod_nm.c_str ());

  if (get_program_option ("print-pages"))
    {
      SCM framework = ly_module_lookup (mod,
                                        ly_symbol2scm ("output-framework"));

      if (scm_is_true (framework))
        {
          SCM func = scm_variable_ref (framework);
          scm_call_4 (func,
                      output_channel,
                      self_scm (),
                      scopes,
                      dump_fields ());
        }
      else
        warning (_f ("program option -dprint-pages not supported by backend `%s'",
                     get_output_backend_name ()));
    }

  if (get_program_option ("preview"))
    {
      SCM framework
        = ly_module_lookup (mod, ly_symbol2scm ("output-preview-framework"));

      if (scm_is_true (framework))
        {
          SCM func = scm_variable_ref (framework);
          scm_call_4 (func,
                      output_channel,
                      self_scm (),
                      scopes,
                      dump_fields ());
        }
      else
        warning (_f ("program option -dpreview not supported by backend `%s'",
                     get_output_backend_name ()));
    }

  if (get_program_option ("crop"))
    {
      SCM framework
        = ly_module_lookup (mod, ly_symbol2scm ("output-crop-framework"));

      if (scm_is_true (framework))
        {
          SCM func = scm_variable_ref (framework);
          scm_call_4 (func,
                      output_channel,
                      self_scm (),
                      scopes,
                      dump_fields ());
        }
      else
        warning (_f ("program option -dcrop not supported by backend `%s'",
                     get_output_backend_name ()));
    }
}

void
Paper_book::classic_output_aux (SCM output,
                                long *first_performance_number)
{
  if (scm_is_pair (performances_))
    {
      Lily::write_performances_midis (performances (),
                                      output,
                                      to_scm (*first_performance_number));
      *first_performance_number += scm_ilength (performances_);
    }

  /* Generate all stencils to trigger font loads.  */
  systems ();
}

void
Paper_book::classic_output (SCM output)
{
  long first_performance_number = 0;
  classic_output_aux (output, &first_performance_number);

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_is_module (header_0_))
    scopes = scm_cons (header_0_, scopes);

  string format = get_output_backend_name ();
  string mod_nm = "lily framework-" + format;

  SCM mod = scm_c_resolve_module (mod_nm.c_str ());
  SCM func = scm_c_module_lookup (mod, "output-classic-framework");

  func = scm_variable_ref (func);
  scm_call_4 (func,
              output,
              self_scm (),
              scopes,
              dump_fields ());
}

/* TODO: resurrect more complex user-tweaks for titling?  */
Stencil
Paper_book::book_title ()
{
  SCM title_func = paper_->lookup_variable (ly_symbol2scm ("book-title"));
  Stencil title;

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  SCM tit = SCM_EOL;
  if (ly_is_procedure (title_func))
    tit = scm_call_2 (title_func,
                      paper_->self_scm (),
                      scopes);

  if (auto *st = unsmob<const Stencil> (tit))
    title = *st;

  if (!title.is_empty ())
    title.align_to (Y_AXIS, UP);

  return title;
}

Stencil
Paper_book::score_title (SCM header)
{
  SCM title_func = paper_->lookup_variable (ly_symbol2scm ("score-title"));

  Stencil title;

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_is_module (header))
    scopes = scm_cons (header, scopes);

  SCM tit = SCM_EOL;
  if (ly_is_procedure (title_func))
    tit = scm_call_2 (title_func,
                      paper_->self_scm (),
                      scopes);

  if (auto *st = unsmob<const Stencil> (tit))
    title = *st;

  if (!title.is_empty ())
    title.align_to (Y_AXIS, UP);

  return title;
}

void
set_page_permission (SCM sys, SCM symbol, SCM permission)
{
  if (Paper_score *ps = unsmob<Paper_score> (sys))
    {
      vector<Paper_column *> const &cols = ps->get_columns ();
      if (cols.size ())
        {
          Paper_column *col = cols.back ();
          set_property (col, symbol, permission);
          set_property (col->find_prebroken_piece (LEFT), symbol, permission);
        }
    }
  else if (Prob *pb = unsmob<Prob> (sys))
    set_property (pb, symbol, permission);
}

/* read the breakbefore property of a score block and set up the preceding
   system-spec to honour it. That is, SYS should be the system spec that
   immediately precedes the score (from which HEADER is taken)
   in the get_system_specs () list */
void
set_system_penalty (SCM sys, SCM header)
{
  if (ly_is_module (header))
    {
      SCM force = ly_module_lookup (header, ly_symbol2scm ("breakbefore"));
      if (SCM_VARIABLEP (force)
          && scm_is_bool (SCM_VARIABLE_REF (force)))
        {
          if (from_scm<bool> (SCM_VARIABLE_REF (force)))
            {
              set_page_permission (sys, ly_symbol2scm ("page-break-permission"),
                                   ly_symbol2scm ("force"));
              set_page_permission (sys, ly_symbol2scm ("line-break-permission"),
                                   ly_symbol2scm ("force"));
            }
          else
            set_page_permission (sys, ly_symbol2scm ("page-break-permission"),
                                 SCM_EOL);
        }
    }
}

void
set_labels (SCM sys, SCM labels)
{
  if (Paper_score *ps = unsmob<Paper_score> (sys))
    {
      vector<Paper_column *> const &cols = ps->get_columns ();
      if (cols.size ())
        {
          Paper_column *col = cols[0];
          set_property (col, "labels",
                        scm_append (scm_list_2 (get_property (col, "labels"), labels)));
          Paper_column *col_right = col->find_prebroken_piece (RIGHT);
          set_property (col_right, "labels",
                        scm_append (scm_list_2 (get_property (col_right, "labels"), labels)));
        }
    }
  else if (Prob *pb = unsmob<Prob> (sys))
    set_property (pb, "labels",
                  scm_append (scm_list_2 (get_property (pb, "labels"), labels)));
}

SCM
Paper_book::get_score_title (SCM header)
{
  Stencil title = score_title (header);
  if (title.is_empty ())
    title = score_title (header_);
  if (!title.is_empty ())
    {
      /*
        TODO: this should come from the \layout {} block, which should
        override settings from \paper {}
      */
      SCM props
        = paper_->lookup_variable (ly_symbol2scm ("score-title-properties"));
      Prob *ps = make_paper_system (props);
      paper_system_set_stencil (ps, title);

      return ps->self_scm ();
    }

  return SCM_BOOL_F;
}

SCM
Paper_book::get_system_specs ()
{
  SCM system_specs = SCM_EOL;

  Stencil title = book_title ();
  if (!title.is_empty ())
    {
      SCM props
        = paper_->lookup_variable (ly_symbol2scm ("book-title-properties"));
      Prob *ps = make_paper_system (props);
      paper_system_set_stencil (ps, title);

      system_specs = scm_cons (ps->self_scm (), system_specs);
      ps->unprotect ();
    }

  SCM page_properties
    = Lily::layout_extract_page_properties (paper_->self_scm ());

  SCM header = SCM_EOL;
  SCM labels = SCM_EOL;
  for (SCM s = scm_reverse (scores_); scm_is_pair (s); s = scm_cdr (s))
    {
      if (ly_is_module (scm_car (s)))
        {
          header = scm_car (s);
          if (scm_is_null (header_0_))
            header_0_ = header;
        }
      else if (Page_marker *page_marker = unsmob<Page_marker> (scm_car (s)))
        {
          /* page markers are used to set page breaking/turning permission,
             or to place bookmarking labels */
          if (scm_is_symbol (page_marker->permission_symbol ()))
            {
              /* set previous element page break or turn permission */
              if (scm_is_pair (system_specs))
                set_page_permission (scm_car (system_specs),
                                     page_marker->permission_symbol (),
                                     page_marker->permission_value ());
            }
          if (scm_is_symbol (page_marker->label ()))
            {
              /* The next element label is to be set */
              labels = scm_cons (page_marker->label (), labels);
            }
        }
      else if (Music_output *mop = unsmob<Music_output> (scm_car (s)))
        {
          if (Paper_score *pscore = dynamic_cast<Paper_score *> (mop))
            {
              SCM title = get_score_title (header);

              if (scm_is_pair (system_specs))
                set_system_penalty (scm_car (system_specs), header);

              if (unsmob<Prob> (title))
                {
                  system_specs = scm_cons (title, system_specs);
                  unsmob<Prob> (title)->unprotect ();
                }

              header = SCM_EOL;
              system_specs = scm_cons (pscore->self_scm (), system_specs);
              if (scm_is_pair (labels))
                {
                  set_labels (scm_car (system_specs), labels);
                  labels = SCM_EOL;
                }
            }
          else
            {
              /*
                Ignore MIDI
              */
            }
        }
      else if (Text_interface::is_markup_list (scm_car (s)))
        {
          SCM texts = Lily::interpret_markup_list (paper_->self_scm (),
                                                   page_properties,
                                                   scm_car (s));
          Prob *first = 0;
          Prob *last = 0;
          for (SCM list = texts; scm_is_pair (list); list = scm_cdr (list))
            {
              auto *t = unsmob<const Stencil> (scm_car (list));
              // TODO: init props
              Prob *ps = make_paper_system (SCM_EOL);
              set_property (ps, "page-break-permission",
                            ly_symbol2scm ("allow"));
              set_property (ps, "page-turn-permission",
                            ly_symbol2scm ("allow"));
              set_property (ps, "last-markup-line", SCM_BOOL_F);
              set_property (ps, "first-markup-line", SCM_BOOL_F);

              paper_system_set_stencil (ps, *t);

              SCM footnotes = get_footnotes (t->expr ());
              set_property (ps, "footnotes", footnotes);
              set_property (ps, "is-title", SCM_BOOL_T);
              if (scm_is_eq (list, texts))
                first = ps;
              else
                {
                  // last line so far, in a multi-line paragraph
                  last = ps;
                  //Place closely to previous line, no stretching.
                  set_property (ps, "tight-spacing", SCM_BOOL_T);
                }
              system_specs = scm_cons (ps->self_scm (), system_specs);
              ps->unprotect ();

              if (scm_is_pair (labels))
                {
                  set_labels (scm_car (system_specs), labels);
                  labels = SCM_EOL;
                }
              // FIXME: figure out penalty.
              //set_system_penalty (ps, scores_[i].header_);
            }
          /* Set properties to avoid widowed/orphaned lines.
             Single-line markup_lists are excluded, but in future
             we may want to add the case of a very short, single line. */
          if (first && last)
            {
              set_property (last, "last-markup-line", SCM_BOOL_T);
              set_property (first, "first-markup-line", SCM_BOOL_T);
            }
        }
      else
        assert (0);
    }

  system_specs = scm_reverse_x (system_specs, SCM_EOL);
  return system_specs;
}

SCM
Paper_book::systems ()
{
  if (scm_is_true (systems_))
    return systems_;

  systems_ = SCM_EOL;
  if (scm_is_pair (bookparts_))
    {
      SCM system_list = SCM_EOL;
      for (SCM p = bookparts_; scm_is_pair (p); p = scm_cdr (p))
        if (Paper_book *pbookpart = unsmob<Paper_book> (scm_car (p)))
          system_list = scm_cons (pbookpart->systems (), system_list);
      systems_ = scm_append (scm_reverse_x (system_list, SCM_EOL));
    }
  else
    {
      SCM specs = get_system_specs ();
      for (SCM s = specs; scm_is_pair (s); s = scm_cdr (s))
        {
          if (Paper_score * pscore
              = unsmob<Paper_score> (scm_car (s)))
            {
              SCM system_list
                = scm_vector_to_list (pscore->get_paper_systems ());

              systems_ = scm_reverse_x (system_list, systems_);
            }
          else
            {
              systems_ = scm_cons (scm_car (s), systems_);
            }
        }
      systems_ = scm_reverse_x (systems_, SCM_EOL);

      /* backwards compatibility for the old page breaker */
      int i = 0;
      Prob *last = 0;
      for (SCM s = systems_; scm_is_pair (s); s = scm_cdr (s))
        {
          Prob *ps = unsmob<Prob> (scm_car (s));
          set_property (ps, "number", to_scm (++i));

          if (last
              && from_scm<bool> (get_property (last, "is-title"))
              && !scm_is_number (get_property (ps, "penalty")))
            set_property (ps, "penalty", to_scm (10000));
          last = ps;

          if (scm_is_pair (scm_cdr (s)))
            {
              SCM perm = get_property (ps, "page-break-permission");
              Prob *next = unsmob<Prob> (scm_cadr (s));
              if (scm_is_null (perm))
                set_property (next, "penalty", to_scm (10001));
              else if (scm_is_eq (perm, ly_symbol2scm ("force")))
                set_property (next, "penalty", to_scm (-10001));
            }
        }
    }

  return systems_;
}

SCM
Paper_book::pages ()
{
  if (scm_is_true (pages_))
    return pages_;

  pages_ = SCM_EOL;
  if (scm_is_pair (bookparts_))
    {
      for (SCM p = bookparts_; scm_is_pair (p); p = scm_cdr (p))
        if (Paper_book *pbookpart = unsmob<Paper_book> (scm_car (p)))
          pages_ = scm_cons (pbookpart->pages (), pages_);
      pages_ = scm_append (scm_reverse_x (pages_, SCM_EOL));
    }
  else if (scm_is_pair (scores_))
    {
      SCM page_breaking = paper_->c_variable ("page-breaking");
      pages_ = scm_call_1 (page_breaking, self_scm ());

      // Create all the page stencils.
      SCM page_module = scm_c_resolve_module ("lily page");
      SCM page_stencil = scm_c_module_lookup (page_module, "page-stencil");
      page_stencil = scm_variable_ref (page_stencil);
      for (SCM pages = pages_; scm_is_pair (pages); pages = scm_cdr (pages))
        scm_call_1 (page_stencil, scm_car (pages));

      // Perform any user-supplied post-processing.
      SCM post_process = paper_->c_variable ("page-post-process");
      if (ly_is_procedure (post_process))
        scm_call_2 (post_process, paper_->self_scm (), pages_);

      /* set systems_ from the pages */
      if (scm_is_false (systems_))
        {
          systems_ = SCM_EOL;
          for (SCM p = pages_; scm_is_pair (p); p = scm_cdr (p))
            {
              Prob *page = unsmob<Prob> (scm_car (p));
              SCM systems = get_property (page, "lines");
              systems_ = scm_cons (systems, systems_);
            }
          systems_ = scm_append (scm_reverse_x (systems_, SCM_EOL));
        }
    }
  return pages_;
}

SCM
Paper_book::performances () const
{
  return scm_reverse (performances_);
}

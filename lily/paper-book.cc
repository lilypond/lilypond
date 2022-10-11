/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "lily-imports.hh"
#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "page-marker.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "program-option.hh"
#include "std-vector.hh"
#include "string-convert.hh"
#include "text-interface.hh"
#include "warn.hh"

using std::string;
using std::vector;

Paper_book::Paper_book (Output_def *paper, Paper_book *parent_part)
{
  header_ = SCM_EOL;
  header_0_ = SCM_EOL;
  pages_ = SCM_BOOL_F;
  performances_ = SCM_EOL;
  performances_tail_ = &performances_;
  print_elements_ = SCM_EOL;
  print_elements_tail_ = &print_elements_;
  print_bookparts_ = false;
  systems_ = SCM_BOOL_F;

  paper_ = 0;
  parent_ = 0;
  smobify_self ();

  Real scale = from_scm<double> (paper->c_variable ("output-scale"));
  paper_ = scale_output_def (paper, scale);
  paper_->unprotect ();
  if (parent_part)
    {
      parent_ = parent_part;
      paper_->parent_ = parent_part->paper_;
    }
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
  scm_gc_mark (print_elements_);
  return systems_;
}

Output_def *
Paper_book::top_paper () const
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
      = scm_cons (ly_symbol2scm (dump_header_fieldnames_global[i]), fields);
  return fields;
}

static void
append_scm_list (SCM **tail_ptr, SCM s)
{
  **tail_ptr = scm_cons (s, SCM_EOL);
  *tail_ptr = SCM_CDRLOC (**tail_ptr);
}

void
Paper_book::add_score (SCM s)
{
  append_scm_list (&print_elements_tail_, s);
}

void
Paper_book::add_bookpart (SCM p)
{
  print_bookparts_ = true;
  append_scm_list (&print_elements_tail_, p);
}

void
Paper_book::add_performance (SCM s)
{
  append_scm_list (&performances_tail_, s);
}

long
Paper_book::output_aux (SCM output_channel, bool is_last,
                        long *first_page_number, long *first_performance_number)
{
  long page_number = 0;
  if (scm_is_pair (performances_))
    {
      Lily::write_performances_midis (performances (), output_channel,
                                      to_scm (*first_performance_number));
      *first_performance_number += scm_ilength (performances_);
    }

  if (print_bookparts_)
    {
      for (SCM p = print_elements_; scm_is_pair (p); p = scm_cdr (p))
        if (Paper_book *pbookpart = unsmob<Paper_book> (scm_car (p)))
          {
            bool is_last_part = (is_last && !scm_is_pair (scm_cdr (p)));
            page_number += pbookpart->output_aux (output_channel, is_last_part,
                                                  first_page_number,
                                                  first_performance_number);
          }
    }
  else
    {
      if (scm_is_null (print_elements_))
        return 0;
      bool pgnums_per_bookpart = from_scm<bool> (paper_->lookup_variable (
        ly_symbol2scm ("bookpart-level-page-numbering")));
      if (!pgnums_per_bookpart)
        {
          paper_->set_variable (ly_symbol2scm ("first-page-number"),
                                to_scm (*first_page_number));
        }
      paper_->set_variable (ly_symbol2scm ("is-last-bookpart"),
                            to_scm (is_last));
      /* Generate all stencils to trigger font loads.  */
      page_number = scm_ilength (pages ());
      if (!pgnums_per_bookpart)
        {
          *first_page_number += page_number;
        }
    }
  return page_number;
}

SCM
ly_output_formats ()
{
  SCM lst = SCM_EOL;
  for (std::string const &fmt : output_formats_global)
    lst = scm_cons (ly_string2scm (fmt), lst);

  return lst;
}

SCM
lilypond_book_output_formats (SCM sym)
{
  std::string arg = robust_symbol2string (ly_get_option (sym), "");
  std::set<std::string> formats;
  for (string format : string_split (arg, ','))
    if (std::find (formats.begin (), formats.end (), format) == formats.end ())
      formats.insert (format);

  SCM lst = SCM_EOL;
  for (std::string const &fmt : formats)
    lst = scm_cons (ly_string2scm (fmt), lst);

  return lst;
}

SCM
all_formats ()
{
  SCM formats = ly_output_formats ();
  SCM lists[2]
    = {lilypond_book_output_formats (ly_symbol2scm ("tall-page-formats")),
       lilypond_book_output_formats (ly_symbol2scm ("separate-page-formats"))};

  for (int i = 0; i < 2; i++)
    for (SCM s = lists[i]; scm_is_pair (s); s = scm_cdr (s))
      if (scm_is_false (scm_member (scm_car (s), formats)))
        formats = scm_cons (scm_car (s), formats);

  return formats;
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

  if (!output_aux (output_channel, true, &first_page_number,
                   &first_performance_number))
    return;

  dump_header_fields (output_channel, false);

  SCM formats = ly_output_formats ();

  if (get_program_option ("print-pages"))
    {
      SCM stencils = SCM_EOL;
      for (SCM s = pages (); scm_is_pair (s); s = scm_cdr (s))
        {
          stencils = scm_cons (
            get_property (unsmob<Prob> (scm_car (s)), "stencil"), stencils);
        }
      output_stencils (output_channel, scm_reverse_x (stencils, SCM_EOL),
                       formats);
    }

  if (get_program_option ("clip-systems"))
    {
      SCM name_stencil_alist
        = Lily::clipped_systems_stencils (output_channel, systems ());

      // Have to use all formats, otherwise the clip-systems regtest breaks.
      for (SCM p = name_stencil_alist; scm_is_pair (p); p = scm_cdr (p))
        output_stencil (scm_caar (p), scm_cdar (p), all_formats ());
    }

  SCM pngstr = ly_string2scm ("png");
  if (scm_is_false (scm_member (pngstr, formats)))
    formats = scm_cons (pngstr, formats);

  std::string basename = ly_scm2string (output_channel);
  if (get_program_option ("preview"))
    {
      output_stencil (ly_string2scm (basename + ".preview"),
                      Lily::generate_preview_stencil (self_scm ()), formats);
    }

  if (get_program_option ("crop"))
    {
      output_stencil (ly_string2scm (basename + ".cropped"),
                      Lily::generate_crop_stencil (self_scm ()), formats);
    }

  if (get_program_option ("aux-files"))
    Lily::write_lilypond_book_aux_files (output_channel, scm_length (pages ()));
}

void
Paper_book::classic_output_aux (SCM output, long *first_performance_number)
{
  if (scm_is_pair (performances_))
    {
      Lily::write_performances_midis (performances (), output,
                                      to_scm (*first_performance_number));
      *first_performance_number += scm_ilength (performances_);
    }

  /* Generate all stencils to trigger font loads.  */
  systems ();
}

void
Paper_book::dump_header_fields (SCM basename, bool classic)
{
  SCM scopes = SCM_EOL;
  if (classic && ly_is_module (header_0_))
    scopes = scm_cons (header_0_, scopes);
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  SCM fields = SCM_EOL;
  for (string field : dump_header_fieldnames_global)
    {
      fields = scm_cons (ly_symbol2scm (field), fields);
    }

  ly_call (Lily::output_scopes, scopes, fields, basename);
}

void
Paper_book::classic_output (SCM output)
{
  long first_performance_number = 0;
  classic_output_aux (output, &first_performance_number);
  dump_header_fields (output, true);

  output_stencils (output, Lily::generate_system_stencils (self_scm ()),
                   ly_output_formats ());

  if (get_program_option ("aux-files"))
    Lily::write_lilypond_book_aux_files (output, scm_length (systems ()));
}

void
Paper_book::output_stencil (SCM out_name, SCM stencil, SCM formats)
{
  string mod_nm = "lily framework-" + get_output_backend_name ();
  SCM mod = scm_c_resolve_module (mod_nm.c_str ());

  SCM framework = scm_module_variable (mod, ly_symbol2scm ("output-stencil"));
  if (scm_is_false (framework))
    {
      warning (_f ("program option -dclip-systems/-dcrop/-dpreview not "
                   "supported by backend `%s'",
                   get_output_backend_name ()));
      return;
    }

  SCM func = scm_variable_ref (framework);
  ly_call (func, out_name, stencil, paper_->self_scm (), formats);
}

void
Paper_book::output_stencils (SCM out_name, SCM stencils, SCM formats)
{
  SCM tall_formats
    = lilypond_book_output_formats (ly_symbol2scm ("tall-page-formats"));
  SCM separate_formats
    = lilypond_book_output_formats (ly_symbol2scm ("separate-page-formats"));

  if (scm_is_pair (tall_formats) || scm_is_pair (separate_formats))
    {
      Stencil acc;
      std::string base = ly_scm2string (out_name);
      if (scm_is_pair (tall_formats))
        {
          for (SCM s = stencils; scm_is_pair (s); s = scm_cdr (s))
            {
              Stencil const *st = unsmob<Stencil const> (scm_car (s));
              acc.add_at_edge (Y_AXIS, DOWN, *st, 2.0);
            }

          output_stencil (out_name, acc.smobbed_copy (), tall_formats);
        }

      if (scm_is_pair (separate_formats))
        {
          int i = 1;
          for (SCM s = stencils; scm_is_pair (s); s = scm_cdr (s), i++)
            {
              output_stencil (ly_string2scm (base + "-" + std::to_string (i)),
                              scm_car (s), separate_formats);
            }
        }
    }
  else
    {
      string format = get_output_backend_name ();
      string mod_nm = "lily framework-" + format;

      SCM mod = scm_c_resolve_module (mod_nm.c_str ());
      SCM func = scm_c_module_lookup (mod, "output-stencils");
      if (scm_is_false (func))
        {
          warning (_f ("multi-page output not supported by backend `%s'",
                       get_output_backend_name ()));
          return;
        }

      func = scm_variable_ref (func);
      ly_call (func, out_name, stencils, header_, paper ()->self_scm (),
               formats);
    }
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
    tit = ly_call (title_func, paper_->self_scm (), scopes);

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
    tit = ly_call (title_func, paper_->self_scm (), scopes);

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
      SCM force = scm_module_variable (header, ly_symbol2scm ("breakbefore"));
      if (SCM_VARIABLEP (force) && scm_is_bool (SCM_VARIABLE_REF (force)))
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
                        ly_append (get_property (col, "labels"), labels));
          Paper_column *col_right = col->find_prebroken_piece (RIGHT);
          set_property (col_right, "labels",
                        ly_append (get_property (col_right, "labels"), labels));
        }
    }
  else if (Prob *pb = unsmob<Prob> (sys))
    set_property (pb, "labels",
                  ly_append (get_property (pb, "labels"), labels));
}

SCM
Paper_book::get_score_title (SCM header)
{
  Stencil title = score_title (header);
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
  SCM last_system_spec = SCM_BOOL_F;
  SCM *system_specs_tail = &system_specs;

  Stencil title = book_title ();
  if (!title.is_empty ())
    {
      SCM props
        = paper_->lookup_variable (ly_symbol2scm ("book-title-properties"));
      Prob *ps = make_paper_system (props);
      paper_system_set_stencil (ps, title);

      append_scm_list (&system_specs_tail, ps->self_scm ());
      last_system_spec = ps->self_scm ();
      ps->unprotect ();
    }

  SCM page_properties
    = Lily::layout_extract_page_properties (paper_->self_scm ());

  SCM header = SCM_EOL;
  SCM labels = SCM_EOL;
  for (SCM s = print_elements_; scm_is_pair (s); s = scm_cdr (s))
    {
      // in order iteration.
      SCM elem = scm_car (s);
      if (ly_is_module (elem))
        {
          header = elem;
          if (scm_is_null (header_0_))
            header_0_ = header;
        }
      else if (Page_marker *page_marker = unsmob<Page_marker> (elem))
        {
          /* page markers are used to set page breaking/turning permission,
             or to place bookmarking labels */
          if (scm_is_symbol (page_marker->permission_symbol ()))
            {
              /* set previous element page break or turn permission */
              if (scm_is_true (last_system_spec))
                set_page_permission (last_system_spec,
                                     page_marker->permission_symbol (),
                                     page_marker->permission_value ());
            }
          if (scm_is_symbol (page_marker->label ()))
            {
              /* The next element label is to be set */
              labels = scm_cons (page_marker->label (), labels);
            }
        }
      else if (Music_output *mop = unsmob<Music_output> (elem))
        {
          if (Paper_score *pscore = dynamic_cast<Paper_score *> (mop))
            {
              SCM title = get_score_title (header);

              if (scm_is_true (last_system_spec))
                // update prev system-spec
                set_system_penalty (last_system_spec, header);

              if (unsmob<Prob> (title))
                {
                  append_scm_list (&system_specs_tail, title);
                  last_system_spec = title;
                  unsmob<Prob> (title)->unprotect ();
                }

              header = SCM_EOL;
              append_scm_list (&system_specs_tail, pscore->self_scm ());
              last_system_spec = pscore->self_scm ();
              if (scm_is_pair (labels))
                {
                  set_labels (pscore->self_scm (), labels);
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
      else if (Text_interface::is_markup_list (elem))
        {
          SCM texts = Lily::interpret_markup_list (paper_->self_scm (),
                                                   page_properties, elem);
          Prob *first = 0;
          Prob *last = 0;
          for (SCM list = texts; scm_is_pair (list); list = scm_cdr (list))
            {
              // Markup interpretation infrastructure should ensure that this is
              // non-null.
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
              append_scm_list (&system_specs_tail, ps->self_scm ());
              last_system_spec = ps->self_scm ();
              ps->unprotect ();

              if (scm_is_pair (labels))
                {
                  set_labels (ps->self_scm (), labels);
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

  return system_specs;
}

SCM
Paper_book::systems ()
{
  if (scm_is_true (systems_))
    return systems_;

  systems_ = SCM_EOL;
  SCM *systems_tail = &systems_;
  if (print_bookparts_)
    {
      for (SCM p = print_elements_; scm_is_pair (p); p = scm_cdr (p))
        if (Paper_book *pbookpart = unsmob<Paper_book> (scm_car (p)))
          for (SCM sys = pbookpart->systems (); scm_is_pair (sys);
               sys = scm_cdr (sys))
            append_scm_list (&systems_tail, scm_car (sys));
    }
  else
    {
      SCM specs = get_system_specs ();
      for (SCM s = specs; scm_is_pair (s); s = scm_cdr (s))
        {
          if (Paper_score *pscore = unsmob<Paper_score> (scm_car (s)))
            {
              for (SCM t = scm_vector_to_list (pscore->get_paper_systems ());
                   scm_is_pair (t); t = scm_cdr (t))
                append_scm_list (&systems_tail, scm_car (t));
            }
          else
            {
              append_scm_list (&systems_tail, scm_car (s));
            }
        }

      /* backwards compatibility for the old page breaker */
      int i = 0;
      Prob *last = 0;
      for (SCM s = systems_; scm_is_pair (s); s = scm_cdr (s))
        {
          Prob *ps = unsmob<Prob> (scm_car (s));
          set_property (ps, "number", to_scm (++i));

          if (last && from_scm<bool> (get_property (last, "is-title"))
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
  SCM *pages_tail = &pages_;
  if (print_bookparts_)
    {
      for (SCM p = print_elements_; scm_is_pair (p); p = scm_cdr (p))
        if (Paper_book *pbookpart = unsmob<Paper_book> (scm_car (p)))
          for (SCM pg = pbookpart->pages (); scm_is_pair (pg);
               pg = scm_cdr (pg))
            append_scm_list (&pages_tail, scm_car (pg));
    }
  else if (scm_is_pair (print_elements_))
    {
      SCM page_breaking = paper_->c_variable ("page-breaking");
      pages_ = ly_call (page_breaking, self_scm ());

      // Create all the page stencils.
      for (SCM pages = pages_; scm_is_pair (pages); pages = scm_cdr (pages))
        Page::page_stencil (scm_car (pages));

      // Perform any user-supplied post-processing.
      SCM post_process = paper_->c_variable ("page-post-process");
      if (ly_is_procedure (post_process))
        ly_call (post_process, paper_->self_scm (), pages_);

      /* set systems_ from the pages */
      if (scm_is_false (systems_))
        {
          systems_ = SCM_EOL;
          SCM *systems_tail = &systems_;
          for (SCM p = pages_; scm_is_pair (p); p = scm_cdr (p))
            {
              Prob *page = unsmob<Prob> (scm_car (p));
              for (SCM sys = get_property (page, "lines"); scm_is_pair (sys);
                   sys = scm_cdr (sys))
                append_scm_list (&systems_tail, scm_car (sys));
            }
        }
    }
  return pages_;
}

SCM
Paper_book::performances () const
{
  return performances_;
}

SCM
Paper_book::get_scopes () const
{
  SCM scopes = SCM_EOL;
  if (parent_)
    {
      scopes = parent_->get_scopes ();
    }
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  return scopes;
}

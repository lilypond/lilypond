/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"

#include "context.hh"
#include "grob-properties.hh"
#include "grob.hh"
#include "international.hh"
#include "music.hh"
#include "paper-column.hh"
#include "score-engraver.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

Engraver_group *
Engraver::get_group () const
{
  // safe: Engravers belong to Engraver_groups
  return static_cast<Engraver_group *> (Translator::get_group ());
}

void
Engraver::announce_grob (Grob_info inf, Context *reroute_context)
{
  get_group ()->announce_grob (inf, START, reroute_context);
}

void
Engraver::announce_end_grob (Grob_info inf, Context *reroute_context)
{
  get_group ()->announce_grob (inf, STOP, reroute_context);
}

Grob_info
Engraver::make_grob_info (Grob *e, SCM cause)
{
  /* TODO: Remove Music code when it's no longer needed */
  if (Music *m = unsmob<Music> (cause))
    {
      cause = m->to_event ()->unprotect ();
    }
  if (scm_is_null (get_property (e, "cause"))
      && (unsmob<Stream_event> (cause) || unsmob<Grob> (cause)))
    set_property (e, "cause", cause);

  return Grob_info (this, e);
}

/*
  CAUSE is the object (typically a Stream_event object)  that
  was the reason for making E.
*/
void
Engraver::announce_grob (Grob *e, SCM cause)
{
  announce_grob (make_grob_info (e, cause));
}

/*
  CAUSE is the object (typically a grob or stream-event object) that
  was the reason for ending E.  */
void
Engraver::announce_end_grob (Grob *e, SCM cause)
{
  announce_end_grob (make_grob_info (e, cause));
}

Engraver::Engraver (Context *c)
  : Translator (c)
{
}

static Protected_scm creation_callback (SCM_EOL);

LY_DEFINE (ly_set_grob_creation_callback, "ly:set-grob-creation-callback", 1, 0,
           0, (SCM cb),
           R"(
Specify a procedure that gets called every time a new grob is created.  The
callback receives as arguments the grob that was created, the name of the C++
source file that caused the grob to be created, and the corresponding line
number in the C++ source file.  Call with @code{#f} as argument to unset the
callback.
           )")
{
  creation_callback = check_debug_callback (cb);
  return SCM_UNSPECIFIED;
}

template <typename T>
T *
Engraver::choose_grob_type (SCM classes, SCM props)
{
  T *grob = new T (props);
  const char *class_name = grob->class_name ();
  if (!scm_is_true (scm_memq (ly_symbol2scm (class_name), classes)))
    {
      programming_error (_f ("grob %s created with disallowed class %s"
                             " (expected any class in the list %s)",
                             grob->name (), class_name,
                             print_scm_val (classes)));
    }
  return grob;
}

template <>
Grob *
Engraver::choose_grob_type<Grob> (SCM classes, SCM props)
{
  if (!scm_is_pair (classes))
    {
      error (_f ("meta.classes must be non-empty list, found %s",
                 print_scm_val (classes)));
    }
  if (!scm_is_null (scm_cdr (classes)))
    {
      error (_f ("must have only one element in meta.classes to create"
                 " a grob without specifying the class"));
    }
  SCM klass = scm_car (classes);
  Grob *grob;
  if (scm_is_eq (klass, ly_symbol2scm ("Item")))
    grob = new Item (props);
  else if (scm_is_eq (klass, ly_symbol2scm ("Paper_column")))
    grob = new Paper_column (props);
  else if (scm_is_eq (klass, ly_symbol2scm ("Spanner")))
    grob = new Spanner (props);
  else
    {
      error (_f ("grob class should be 'Item, 'Spanner or 'Paper_column,"
                 " found %s",
                 print_scm_val (klass)));
    }
  return grob;
}

/* When T is Item, Paper_column or Spanner, return a pointer to an
   instance of this class after having checked that the meta.classes
   field does contain this class.  This is used by make_item,
   make_spanner, make_column and the respective Scheme functions.

   When T is Grob, the meta.classes field must be have only one element.
   Make a grob with the corresponding class and return it as Grob *.
   This is used by ly:engraver-make-grob. */
template <typename T>
T *
Engraver::internal_make_grob (SCM symbol, SCM cause, char const *file, int line,
                              char const *fun)
{
#ifndef DEBUG
  (void) file;
  (void) line;
  (void) fun;
#endif

  SCM props = Grob_property_info (context (), symbol).updated ();
  if (!scm_is_pair (props))
    {
      error (
        _f ("No grob definition found for `%s'.", ly_symbol2string (symbol)));
    }

  SCM meta = scm_assq_ref (props, ly_symbol2scm ("meta"));
  SCM classes = scm_assq_ref (meta, ly_symbol2scm ("classes"));

  T *grob = choose_grob_type<T> (classes, props);
  announce_grob (grob, cause);

#ifdef DEBUG
  if (ly_is_procedure (creation_callback))
    ly_call (creation_callback, grob->self_scm (), scm_from_utf8_string (file),
             to_scm (line), scm_from_latin1_string (fun));
#endif

  return grob;
}

Item *
Engraver::internal_make_item (SCM x, SCM cause, char const *file, int line,
                              char const *fun)
{
  return internal_make_grob<Item> (x, cause, file, line, fun);
}

Paper_column *
Engraver::internal_make_column (SCM x, char const *file, int line,
                                char const *fun)
{
  return internal_make_grob<Paper_column> (x, SCM_EOL, file, line, fun);
}

Spanner *
Engraver::internal_make_spanner (SCM x, SCM cause, char const *file, int line,
                                 char const *fun)
{
  return internal_make_grob<Spanner> (x, cause, file, line, fun);
}

Grob *
Engraver::internal_make_indeterminate (SCM x, SCM cause, char const *file,
                                       int line, char const *fun)
{
  return internal_make_grob<Grob> (x, cause, file, line, fun);
}

Grob *
Engraver::internal_make_sticky (SCM x, Grob *host, SCM cause, char const *file,
                                int line, char const *fun)
{
  Grob *sticky = host->make_sticky_same_type (this, x, cause, file, line, fun);
  if (!sticky->internal_has_interface (ly_symbol2scm ("sticky-grob-interface")))
    {
      programming_error (_f ("sticky grob %s created with a type that does"
                             " not have the sticky-grob-interface",
                             sticky->name ()));
    }
  set_object (sticky, "sticky-host", host->self_scm ());
  sticky->set_x_parent (host);
  sticky->set_y_parent (host);
  return sticky;
}

bool
ly_is_grob_cause (SCM obj)
{
  return unsmob<Grob> (obj) || unsmob<Stream_event> (obj) || scm_is_null (obj);
}

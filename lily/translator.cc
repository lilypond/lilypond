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

#include "translator.hh"

#include "context-def.hh"
#include "dispatcher.hh"
#include "global-context.hh"
#include "translator-group.hh"
#include "warn.hh"

#include <cassert>

#include "translator.icc"

using std::string;
using std::vector;

Translator::~Translator ()
{
}

/* The five base class methods below are never called: they don't need
   to do anything, so for performance we avoid calling them by
   precomputing the list of overridden methods. */

void
Translator::pre_process_music ()
{
  assert (false);
}

void
Translator::process_music ()
{
  assert (false);
}

void
Translator::process_acknowledged ()
{
  assert (false);
}

void
Translator::stop_translation_timestep ()
{
  assert (false);
}

/*
  this function is called once each moment, before any user
  information enters the translators.  (i.e. no \property or event has
  been processed yet.)
*/
void
Translator::start_translation_timestep ()
{
  assert (false);
}

Translator::Translator (Context *c)
  : context_ (c)
{
  smobify_self ();
}

Moment
Translator::now_mom () const
{
  return context_->now_mom ();
}

Output_def *
Translator::get_output_def () const
{
  return context_->get_output_def ();
}

Translator_group *
Translator::get_group () const
{
  return context_->implementation ();
}

void
Translator::protect_event (SCM ev)
{
  get_group ()->protect_event (ev);
}

SCM
Translator::internal_get_property (SCM sym) const
{
  return context_->internal_get_property (sym);
}

void
Translator::initialize ()
{
}

void
Translator::finalize ()
{
}

void
Translator::connect_to_context (Context *c)
{
  for (SCM r = get_listener_list (); scm_is_pair (r); r = scm_cdr (r))
    {
      SCM event_class = scm_caar (r);
      SCM callback = scm_cdar (r);

      c->events_below ()->add_listener (Listener (callback, self_scm ()),
                                        event_class);
    }
}

void
Translator::disconnect_from_context (Context *c)
{
  for (SCM r = get_listener_list (); scm_is_pair (r); r = scm_cdr (r))
    {
      SCM event_class = scm_caar (r);
      SCM callback = scm_cdar (r);

      c->events_below ()->remove_listener (Listener (callback, self_scm ()),
                                           event_class);
    }
}

SCM
Translator::event_class_symbol (const char *ev_class)
{
  /* ev_class is the C++ identifier name. Convert to scm symbol */
  string name = string (ev_class);
  name = replace_all (&name, '_', '-');
  name += "-event";

  return scm_from_latin1_symbol (name.c_str ());
}

/*
 Helps the individual static_translator_description methods of translators.
*/
SCM
Translator::static_translator_description (const char *grobs, const char *desc,
                                           SCM listener_list, const char *read,
                                           const char *write)
{
  SCM static_properties = SCM_EOL;

  static_properties = scm_acons (ly_symbol2scm ("grobs-created"),
                                 parse_symbol_list (grobs), static_properties);

  static_properties
    = scm_acons (ly_symbol2scm ("description"), scm_from_utf8_string (desc),
                 static_properties);

  SCM list = SCM_EOL;
  for (; scm_is_pair (listener_list); listener_list = scm_cdr (listener_list))
    list = scm_cons (scm_caar (listener_list), list);
  static_properties
    = scm_acons (ly_symbol2scm ("events-accepted"), list, static_properties);

  static_properties = scm_acons (ly_symbol2scm ("properties-read"),
                                 parse_symbol_list (read), static_properties);

  static_properties = scm_acons (ly_symbol2scm ("properties-written"),
                                 parse_symbol_list (write), static_properties);

  return static_properties;
}

/*
  SMOBS
*/
SCM
Translator::mark_smob () const
{
  derived_mark ();
  return SCM_EOL;
}

Global_context *
Translator::find_global_context () const
{
  return ::find_global_context (context_);
}

Context *
Translator::find_score_context () const
{
  Context *score = find_context_above (context_, ly_symbol2scm ("Score"));
  if (score)
    return score;
  programming_error ("no score context");
  abort ();
}

const char *const Translator::type_p_name_ = "ly:translator?";

bool
Translator::must_be_last () const
{
  return false;
}

void
Translator::derived_mark () const
{
}

int
Translator::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Translator ", port);
  scm_puts (class_name (), port);
  scm_puts (" >", port);
  return 1;
}

void
add_acknowledger (SCM ptr, char const *func_name, SCM &ack_hash)
{
  if (SCM_UNBNDP (ack_hash))
    ack_hash = Scheme_hash_table::make_smob ();

  string interface_name (func_name);

  interface_name = replace_all (&interface_name, '_', '-');
  interface_name += "-interface";

  unsmob<Scheme_hash_table> (ack_hash)->set (ly_symbol2scm (interface_name),
                                             ptr);
}

SCM
generic_get_acknowledger (SCM sym, SCM ack_hash)
{
  if (SCM_UNBNDP (ack_hash))
    return SCM_UNDEFINED;

  return unsmob<Scheme_hash_table> (ack_hash)->get (sym);
}

Moment
get_event_length (Stream_event *e)
{
  return from_scm (get_property (e, "length"), Moment (0));
}

Moment
get_event_length (Stream_event *e, Moment now)
{
  auto len = get_event_length (e);

  if (now.grace_part_)
    {
      len.grace_part_ = len.main_part_;
      len.main_part_ = Rational (0);
    }
  return len;
}

// Base class.  Not instantiated.  No ADD_TRANSLATOR call.

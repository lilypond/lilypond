/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"

#include "context-def.hh"
#include "dispatcher.hh"
#include "global-context.hh"
#include "international.hh"
#include "listener.hh"
#include "ly-scm-list.hh"
#include "main.hh"
#include "output-def.hh"
#include "profile.hh"
#include "program-option.hh"
#include "scm-hash.hh"
#include "translator-group.hh"
#include "warn.hh"
#include "lily-imports.hh"
#include "duration.hh"

using std::string;
using std::vector;

bool
Context::is_removable () const
{
  return scm_is_null (context_list_) && !client_count_
         && !dynamic_cast<Global_context const *> (parent_);
}

void
Context::check_removal ()
{
  for (SCM p = context_list_; scm_is_pair (p); p = scm_cdr (p))
    {
      Context *ctx = unsmob<Context> (scm_car (p));

      ctx->check_removal ();
      if (ctx->is_removable ())
        {
          recurse_over_translators (ctx, MFP_WRAP (&Translator::finalize),
                                    MFP_WRAP (&Translator_group::finalize), UP);
          send_stream_event (ctx, "RemoveContext", 0);
        }
    }
}

Scheme_hash_table *
Context::properties_dict () const
{
  return unsmob<Scheme_hash_table> (properties_scm_);
}

void
Context::add_context (Context *child)
{
  context_list_
    = ly_append (context_list_, scm_cons (child->self_scm (), SCM_EOL));

  child->parent_ = this;
  events_below_->register_as_listener (child->events_below_);
}

Context::Context (Context_def *cdef, SCM ops)
{
  parent_ = 0;
  aliases_ = SCM_EOL;
  client_count_ = 0;
  implementation_ = 0;
  properties_scm_ = SCM_EOL;
  context_list_ = SCM_EOL;
  definition_ = SCM_EOL;
  definition_mods_ = SCM_EOL;
  event_source_ = 0;
  events_below_ = 0;

  smobify_self ();

  properties_scm_ = Scheme_hash_table::make_smob ();
  event_source_ = new Dispatcher ();
  event_source_->unprotect ();
  events_below_ = new Dispatcher ();
  events_below_->unprotect ();

  definition_ = cdef->self_scm ();
  definition_mods_ = ops;
  aliases_ = cdef->get_context_aliases ();
  acceptance_.assign_copy (cdef->get_acceptance ());
  // TODO: Set this with "\adopts ##t" in the ly code.
  SCM type_sym = cdef->get_translator_group_type ();
  adopts_ = (scm_is_eq (type_sym, ly_symbol2scm ("Score_engraver"))
             || scm_is_eq (type_sym, ly_symbol2scm ("Score_performer")));
  for (SCM op : as_ly_scm_list (ops))
    {
      SCM tag = scm_car (op);
      if (scm_is_eq (tag, ly_symbol2scm ("accepts")))
        acceptance_.accept (scm_string_to_symbol (scm_cadr (op)));
      else if (scm_is_eq (tag, ly_symbol2scm ("denies")))
        acceptance_.deny (scm_string_to_symbol (scm_cadr (op)));
      else if (scm_is_eq (tag, ly_symbol2scm ("default-child")))
        acceptance_.accept_default (scm_string_to_symbol (scm_cadr (op)));
    }
}

// True if this context has the given type and id.
// These values function as wildcards: type=SCM_EOL, id="".
bool
Context::matches (SCM type, const string &id) const
{
  if (!id.empty () && (id_string () != id))
    return false;

  if (!scm_is_null (type) && !is_alias (type))
    return false;

  return true;
}

// This recursive find-or-create traversal is the core part of \context and
// \new, but it is not the complete search.  See find ().
//
// If dir is UP, do not descend.  If dir is DOWN, do not ascend.  If dir is
// CENTER, do not limit the walk.
//
// n is a symbol: the name of the context to find or create.  In FIND_ONLY
// mode, it may also be SCM_EOL to act as a wild card.
Context *
Context::core_find (FindMode mode, Direction dir, SCM n, const string &id,
                    SCM ops)
{
  const bool allow_create = (mode != FIND_ONLY);
  const bool allow_find = (mode != CREATE_ONLY);
  const bool walk_down = (dir != UP);
  const bool walk_up = (dir != DOWN);

  if (allow_find && matches (n, id))
    return this;

  if (walk_down && allow_find)
    {
      // Searching below in recursive calls can find contexts beyond those that
      // are visible when looking just down and up from the starting point.
      // The regression test lyric-combine-polyphonic.ly has a reasonably
      // simple example of how this can be useful.
      for (SCM s = children_contexts (); scm_is_pair (s); s = scm_cdr (s))
        {
          if (Context *c = unsmob<Context> (scm_car (s)))
            {
              c = c->core_find (FIND_ONLY, DOWN, n, id, SCM_EOL);
              if (c)
                return c;
            }
        }
    }

  if (walk_down && allow_create)
    {
      vector<Context_def *> path = path_to_acceptable_context (n);
      if (!path.empty ())
        {
          // TODO: Would it be OK to use one intermediate ID for all cases?  It
          // changes the output of ly->midi->ly regression tests such as
          // lyrics-addlyrics-midi.ly, but are the differences important?
          const char *intermediate_id = allow_find ? "" : "\\new";
          return create_hierarchy (path, intermediate_id, id, ops);
        }
    }

  if (walk_up && parent_)
    return parent_->core_find (mode, dir, n, id, ops);

  return nullptr;
}

// This implements all the logic of find () except a final check that the found
// context is accessible to the user.
Context *
Context::unchecked_find (FindMode mode, Direction dir, SCM n, const string &id,
                         SCM ops)
{
  const bool allow_create = (mode != FIND_ONLY);
  const bool allow_find = (mode != CREATE_ONLY);

  if (allow_find && (dir == CENTER))
    {
      // Search everything in and below the scope of the current context first.
      // Here is an example that depends on finding a context below.
      //
      //   \new PianoStaff <<
      //     \new Staff = "RH" { ... }
      //     \new Staff = "LH" { ... }
      //     \context Staff = "RH" { ... }
      //   >>
      //
      if (Context *c = core_find (FIND_ONLY, DOWN, n, id, SCM_EOL))
        return c;

      // Search the path to the top before considering more distantly related
      // contexts.
      if (Context *c = core_find (FIND_ONLY, UP, n, id, SCM_EOL))
        return c;
    }

  if (allow_create && scm_is_symbol (n))
    {
      if (Context *c = core_find (mode, dir, n, id, ops))
        return c;
    }
  else if (allow_find)
    {
      if (Context *c = core_find (FIND_ONLY, dir, n, id, SCM_EOL))
        return c;
    }

  return nullptr;
}

Context *
Context::find (FindMode mode, Direction dir, SCM n, const string &id, SCM ops)
{
  if (Context *c = unchecked_find (mode, dir, n, id, ops))
    {
      if (c->is_accessible_to_user ())
        return c;
    }

  return nullptr;
}

Context *
Context::create_unique_context (Direction dir, SCM name, const string &id,
                                SCM ops)
{
  return find (CREATE_ONLY, dir, name, id, ops);
}

Context *
Context::find_context (Direction dir, SCM name, const string &id)
{
  return find (FIND_ONLY, dir, name, id, SCM_EOL);
}

Context *
Context::find_create_context (Direction dir, SCM name, const string &id,
                              SCM ops)
{
  return find (FIND_CREATE, dir, name, id, ops);
}

void
Context::acknowledge_infant (SCM sev)
{
  infant_event_ = unsmob<Stream_event> (sev);
}

// Make a finalization to set (or unset) the current value of the given
// property.
SCM
Context::make_revert_finalization (SCM sym)
{
  SCM val = SCM_UNDEFINED;
  if (here_defined (this, sym, &val))
    {
      return ly_list (ly_context_set_property_x_proc, self_scm (), sym, val);
    }
  else
    {
      return ly_list (ly_context_unset_property_proc, self_scm (), sym);
    }
}

void
Context::add_global_finalization (SCM x)
{
  find_global_context (this)->add_finalization (x);
}

void
Context::set_property_from_event (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);

  SCM sym = get_property (ev, "symbol");
  if (scm_is_symbol (sym))
    {
      SCM val = get_property (ev, "value");
      bool ok = true;
      ok
        = type_check_assignment (sym, val, ly_symbol2scm ("translation-type?"));

      if (ok)
        {
          if (from_scm<bool> (get_property (ev, "once")))
            add_global_finalization (make_revert_finalization (sym));
          set_property (this, sym, val);
        }
    }
}

void
Context::unset_property_from_event (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);

  SCM sym = get_property (ev, "symbol");
  bool ok
    = type_check_assignment (sym, SCM_EOL, ly_symbol2scm ("translation-type?"));

  if (ok)
    {
      if (from_scm<bool> (get_property (ev, "once")))
        add_global_finalization (make_revert_finalization (sym));
      unset_property (sym);
    }
}

/*
  Creates a new context from a CreateContext event, and sends an
  AnnounceNewContext event to this context.
*/
void
Context::create_context_from_event (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);

  string id = ly_scm2string (get_property (ev, "id"));
  SCM ops = get_property (ev, "ops");
  SCM type_scm = get_property (ev, "type");
  string type = ly_symbol2string (type_scm);

  vector<Context_def *> path = path_to_acceptable_context (type_scm);

  if (path.size () != 1)
    {
      programming_error (
        to_string ("Invalid CreateContext event: Cannot create %s context",
                   type.c_str ()));
      return;
    }
  Context_def *cdef = path[0];

  Context *new_context = new Context (cdef, ops);

  new_context->id_string_ = id;

  /* Register various listeners:
      - Make the new context hear events that universally affect contexts
      - connect events_below etc. properly */
  /* We want to be the first ones to hear our own events. Therefore, wait
     before registering events_below_ */
  new_context->event_source ()->add_listener (
    GET_LISTENER (new_context, create_context_from_event),
    ly_symbol2scm ("CreateContext"));
  new_context->event_source ()->add_listener (
    GET_LISTENER (new_context, remove_context),
    ly_symbol2scm ("RemoveContext"));
  new_context->event_source ()->add_listener (
    GET_LISTENER (new_context, change_parent), ly_symbol2scm ("ChangeParent"));
  new_context->event_source ()->add_listener (
    GET_LISTENER (new_context, set_property_from_event),
    ly_symbol2scm ("SetProperty"));
  new_context->event_source ()->add_listener (
    GET_LISTENER (new_context, unset_property_from_event),
    ly_symbol2scm ("UnsetProperty"));

  new_context->events_below_->register_as_listener (new_context->event_source_);
  add_context (new_context);

  new_context->unprotect ();

  Context_def *td = unsmob<Context_def> (new_context->definition_);

  /* This cannot move before add_context (), because \override
     operations require that we are in the hierarchy.  */
  td->apply_default_property_operations (new_context);
  apply_property_operations (new_context, ops);

  send_stream_event (this, "AnnounceNewContext", 0, ly_symbol2scm ("context"),
                     new_context->self_scm (), ly_symbol2scm ("creator"), sev);
}

vector<Context_def *>
Context::path_to_acceptable_context (SCM name) const
{
  Output_def *odef = get_output_def ();

  if (scm_is_eq (name, ly_symbol2scm ("Bottom")))
    {
      return Context_def::path_to_bottom_context (odef,
                                                  acceptance_.get_default ());
    }

  return unsmob<Context_def> (definition_)
    ->path_to_acceptable_context (name, odef, acceptance_.get_list ());
}

Context *
Context::create_context (Context_def *cdef, const string &id, SCM ops)
{
  infant_event_ = 0;
  /* TODO: This is fairly misplaced. We can fix this when we have taken out all
     iterator specific stuff from the Context class */
  event_source_->add_listener (GET_LISTENER (this, acknowledge_infant),
                               ly_symbol2scm ("AnnounceNewContext"));
  /* The CreateContext creates a new context, and sends an announcement of the
     new context through another event. That event will be stored in
     infant_event_ to create a return value. */
  send_stream_event (this, "CreateContext", 0, ly_symbol2scm ("ops"), ops,
                     ly_symbol2scm ("type"), cdef->get_context_name (),
                     ly_symbol2scm ("id"), ly_string2scm (id));
  event_source_->remove_listener (GET_LISTENER (this, acknowledge_infant),
                                  ly_symbol2scm ("AnnounceNewContext"));

  assert (infant_event_);
  SCM infant_scm = get_property (infant_event_, "context");
  Context *infant = unsmob<Context> (infant_scm);

  if (!infant || infant->get_parent () != this)
    {
      programming_error ("create_context: can't locate newly created context");
      return 0;
    }

  return infant;
}

// Create a new context at the end of a given path below this context, using
// leaf_id and leaf_operations for it.
//
// Intermediate contexts in the path are reused or created.  Contexts
// configured to "adopt" new descendants are considered for reuse.  When
// necessary, contexts are created using intermediate_id and no operations.
//
// If the desired leaf context is successfully created, return it.  If the path
// is empty, return this context.
//
// On failure to create any context, stop and return null.  Contexts created to
// that point will continue to exist.
Context *
Context::create_hierarchy (const std::vector<Context_def *> &path,
                           const std::string &intermediate_id,
                           const std::string &leaf_id, SCM leaf_operations)
{
  Context *leaf = this;

  if (!path.empty ())
    {
      // choose or create the intermediate contexts
      for (vsize i = 0; i < path.size () - 1; ++i)
        {
          SCM child_name = path[i]->get_context_name ();
          SCM grandchild_name = path[i + 1]->get_context_name ();
          Context *c = leaf->find_child_to_adopt_grandchild (child_name,
                                                             grandchild_name);
          if (c)
            leaf = c;
          else
            {
              leaf = leaf->create_context (path[i], intermediate_id, SCM_EOL);
              if (!leaf)
                return 0; // expect that create_context logged failure
            }
        }

      leaf = leaf->create_context (path.back (), leaf_id, leaf_operations);
      if (!leaf)
        return 0; // expect that create_context logged failure
    }

  return leaf;
}

// Find an existing child of the exact given type (not an alias), which will
// adopt the given type of grandchild.
Context *
Context::find_child_to_adopt_grandchild (SCM child_name, SCM grandchild_name)
{
  for (SCM s = context_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Context *c = unsmob<Context> (scm_car (s));
      if (c->adopts_ && scm_is_eq (c->context_name_symbol (), child_name) &&
          // Is this way of checking acceptance too heavy?
          (c->path_to_acceptable_context (grandchild_name).size () == 1))
        {
          return c;
        }
    }

  return nullptr;
}

bool
Context::is_bottom_context () const
{
  return !acceptance_.has_default ();
}

Context *
Context::get_default_interpreter (const string &id)
{
  if (is_bottom_context ())
    {
      if (id.empty () || (id == id_string ()))
        return this; // this is where we want to be
    }

  // It's interesting that this goes straight to creating a new hierarchy even
  // if there might be an existing partial (or even full?) path to a bottom
  // context.  This deserves an explanation.
  SCM name = ly_symbol2scm ("Bottom");
  if (Context *c = create_unique_context (CENTER, name, id, SCM_EOL))
    return c;

  // TODO: Avoiding a null return means the caller does not detect this
  // failure, so we have to log here if we want to log at all.  It would be
  // more flexible to return null and let the caller decide whether to warn in
  // its situation.  The caller might know a useful source location too.
  warning (_f ("cannot find or create context: %s",
               diagnostic_id (name, id).c_str ()));
  return this;
}

// Concretely: If the current context is Global, find or create a Score.  The
// name and some of the internals are more general, but not completely general.
Context *
Context::get_user_accessible_interpreter ()
{
  if (is_accessible_to_user ())
    return this;

  // path_to_bottom_context () is a ready way to avoid hard-coding "Score".
  const auto &path = Context_def::path_to_bottom_context (
    get_output_def (), acceptance_.get_default ());
  auto c = this;
  for (const auto &cdef : path)
    {
      c = c->find_create_context (DOWN, cdef->get_context_name (), "", SCM_EOL);
      if (!c || c->is_accessible_to_user ())
        return c;
    }

  return nullptr;
}

/*
  PROPERTIES
*/
Context *
Context::internal_where_defined (SCM sym, SCM *value) const
{
#ifdef DEBUG
  if (profile_property_accesses)
    note_property_access (&context_property_lookup_table, sym);
#endif

  if (properties_dict ()->try_retrieve (sym, value))
    return const_cast<Context *> (this);

  return parent_ ? parent_->internal_where_defined (sym, value) : nullptr;
}

/* Quick variant of where_defined.  Checks only the context itself. */

bool
Context::internal_here_defined (SCM sym, SCM *value) const
{
#ifdef DEBUG
  if (profile_property_accesses)
    note_property_access (&context_property_lookup_table, sym);
#endif

  return properties_dict ()->try_retrieve (sym, value);
}

/*
  return SCM_EOL when not found.
*/
SCM
Context::internal_get_property (SCM sym) const
{
#ifdef DEBUG
  if (profile_property_accesses)
    note_property_access (&context_property_lookup_table, sym);
#endif

  SCM val = SCM_EOL;
  if (properties_dict ()->try_retrieve (sym, &val))
    return val;

  if (parent_)
    return parent_->internal_get_property (sym);

  return val;
}

/*
These methods should not be called from any other place than the
send_stream_event macro.
*/

void
Context::internal_send_stream_event (SCM type, Input *origin)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  set_property (e, prop, val);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                     SCM prop2, SCM val2)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  set_property (e, prop, val);
  set_property (e, prop2, val2);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                     SCM prop2, SCM val2, SCM prop3, SCM val3)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  set_property (e, prop, val);
  set_property (e, prop2, val2);
  set_property (e, prop3, val3);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                     SCM prop2, SCM val2, SCM prop3, SCM val3,
                                     SCM prop4, SCM val4)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  set_property (e, prop, val);
  set_property (e, prop2, val2);
  set_property (e, prop3, val3);
  set_property (e, prop4, val4);
  event_source_->broadcast (e);
  e->unprotect ();
}

bool
Context::is_alias (SCM sym) const
{
  if (scm_is_eq (sym, ly_symbol2scm ("Bottom")))
    return is_bottom_context ();
  if (scm_is_eq (sym, context_name_symbol ()))
    return true;

  return scm_is_true (scm_c_memq (sym, aliases_));
}

void
Context::add_alias (SCM sym)
{
  aliases_ = scm_cons (sym, aliases_);
}

/* we don't (yet) instrument context properties */
void
Context::instrumented_set_property (SCM sym, SCM val, const char *, int,
                                    const char *)
{
  internal_set_property (sym, val);
}

void
Context::internal_set_property (SCM sym, SCM val)
{
  bool type_check_ok
    = type_check_assignment (sym, val, ly_symbol2scm ("translation-type?"));

  if (do_internal_type_checking_global)
    assert (type_check_ok);

  if (type_check_ok)
    properties_dict ()->set (sym, val);
}

/*
  TODO: look up to check whether we have inherited var?
*/
void
Context::unset_property (SCM sym)
{
  properties_dict ()->remove (sym);
}

void
Context::change_parent (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  Context *to = unsmob<Context> (get_property (ev, "context"));

  disconnect_from_parent ();
  to->add_context (this);
}

/*
  Die. The next GC sweep should take care of the actual death.
 */
void
Context::remove_context (SCM)
{
  /* ugh, the translator group should listen to RemoveContext events by itself */
  Translator_group *impl = implementation ();
  if (impl)
    impl->disconnect_from_context ();
  disconnect_from_parent ();
}

void
Context::disconnect_from_parent ()
{
  parent_->events_below_->unregister_as_listener (events_below_);
  parent_->context_list_ = scm_delq_x (self_scm (), parent_->context_list_);
  parent_ = 0;
}

Context *
find_top_context (Context *where)
{
  Context *top = where;
  for (; where; where = where->get_parent ())
    top = where;
  return top;
}

SCM
Context::properties_as_alist () const
{
  return properties_dict ()->to_alist ();
}

SCM
Context::context_name_symbol () const
{
  Context_def *td = unsmob<Context_def> (definition_);
  return td->get_context_name ();
}

string
Context::context_name () const
{
  return ly_symbol2string (context_name_symbol ());
}

string
Context::diagnostic_id (SCM name, const string &id)
{
  // For robustness when this static method is called directly (e.g. after a
  // failure to create a context), we do not assume that name is a symbol.
  string result (ly_scm_write_string (name));
  if (!id.empty ())
    {
      result += " = ";
      result += id;
    }
  return result;
}

Output_def *
Context::get_output_def () const
{
  const auto *top = find_top_context (const_cast<Context *> (this));
  return top->get_output_def ();
}

Context::~Context ()
{
}

Moment
Context::now_mom () const
{
  const auto *top = find_top_context (const_cast<Context *> (this));
  return top->now_mom ();
}

int
Context::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<", port);
  scm_puts (class_name (), port);
  if (Context_def *d = unsmob<Context_def> (definition_))
    {
      scm_puts (" ", port);
      scm_display (d->get_context_name (), port);
    }

  if (!id_string_.empty ())
    {
      scm_puts ("=", port);
      scm_puts (id_string_.c_str (), port);
    }

  scm_puts (" ", port);

  scm_display (context_list_, port);
  scm_puts (" >", port);

  return 1;
}

void
Context::derived_mark () const
{
}

SCM
Context::mark_smob () const
{
  scm_gc_mark (context_list_);
  scm_gc_mark (aliases_);
  scm_gc_mark (definition_);
  scm_gc_mark (definition_mods_);
  scm_gc_mark (properties_scm_);
  acceptance_.gc_mark ();

  if (implementation_)
    scm_gc_mark (implementation_->self_scm ());

  if (event_source_)
    scm_gc_mark (event_source_->self_scm ());

  if (events_below_)
    scm_gc_mark (events_below_->self_scm ());

  derived_mark ();

  return properties_scm_;
}

const char *const Context::type_p_name_ = "ly:context?";

/*
  Ugh. Where to put this?
*/
Rational
measure_length (Context const *context)
{
  return from_scm (get_property (context, "measureLength"), Moment (1))
    .main_part_;
}

Moment
measure_position (Context const *context)
{
  auto m = from_scm (get_property (context, "measurePosition"), Moment ());

  if (m.main_part_ < 0)
    {
      const auto length (measure_length (context));
      do
        m.main_part_ += length;
      while (m.main_part_ < 0);
    }

  return m;
}

// Find the moment where a note of duration dur happening now will end.
Moment
note_end_mom (Context const *context, Duration const *dur)
{
  const auto now = context->now_mom ();
  Rational dur_length = dur ? dur->get_length () : Rational (0);

  Moment end_pos = now.grace_part_ < Rational (0)
                     ? Moment (now.main_part_, now.grace_part_ + dur_length)
                     : Moment (now.main_part_ + dur_length, 0);

  return end_pos;
}

int
measure_number (Context const *context)
{
  SCM barnum = get_property (context, "internalBarNumber");
  SCM smp = get_property (context, "measurePosition");

  auto bn = from_scm (barnum, 0);
  const auto mp = from_scm (smp, Moment (0));
  if (mp.main_part_ < Rational (0))
    bn--;

  return bn;
}

void
set_context_property_on_children (Context *trans, SCM sym, SCM val)
{
  set_property (trans, sym, ly_deep_copy (val));
  for (SCM p = trans->children_contexts (); scm_is_pair (p); p = scm_cdr (p))
    {
      Context *trg = unsmob<Context> (scm_car (p));
      set_context_property_on_children (trg, sym, ly_deep_copy (val));
    }
}

bool
melisma_busy (Context *tr)
{
  // When there are subcontexts, they are responsible for maintaining
  // melismata.
  SCM ch = tr->children_contexts ();
  if (scm_is_pair (ch))
    {
      // all contexts need to have a busy melisma for this to evaluate
      // to true.

      do
        {
          if (!melisma_busy (unsmob<Context> (scm_car (ch))))
            return false;
          ch = scm_cdr (ch);
        }
      while (scm_is_pair (ch));
      return true;
    }

  for (SCM melisma_properties = get_property (tr, "melismaBusyProperties");
       scm_is_pair (melisma_properties);
       melisma_properties = scm_cdr (melisma_properties))
    if (from_scm<bool> (get_property (tr, scm_car (melisma_properties))))
      return true;

  return false;
}

bool
check_repeat_count_visibility (Context const *context, SCM count)
{
  SCM proc = get_property (context, "repeatCountVisibility");
  return (ly_is_procedure (proc)
          && from_scm<bool> (ly_call (proc, count, context->self_scm ())));
}

bool
break_allowed (Context const *ctx)
{
  // A break is allowed if nothing prevented it, or if the user
  // explicitly requested it.
  return (!from_scm<bool> (get_property (ctx, "forbidBreak"))
          || from_scm<bool> (get_property (ctx, "forceBreak")));
}

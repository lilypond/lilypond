/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lily-imports.hh"
#include "main.hh"
#include "output-def.hh"
#include "profile.hh"
#include "program-option.hh"
#include "scm-hash.hh"
#include "translator-group.hh"
#include "warn.hh"

using std::string;
using std::vector;

static const char *const NEW_CONTEXT_ID = "\\new";

bool
Context::is_removable () const
{
  return scm_is_null (context_list_) && !client_count_
         && !dynamic_cast<Global_context const *> (daddy_context_);
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
          recurse_over_translators (
              ctx,
              Callback0_wrapper::make_smob<Translator,
                                           &Translator::finalize> (),
              Callback0_wrapper::make_smob<Translator_group,
                                           &Translator_group::finalize> (),
              UP);
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
      = ly_append2 (context_list_, scm_cons (child->self_scm (), SCM_EOL));

  child->daddy_context_ = this;
  events_below_->register_as_listener (child->events_below_);
}

Context::Context ()
{
  daddy_context_ = 0;
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
}

/* TODO: create_unique_context () and find_create_context () are concerningly
   similar yet different.  If you come to understand whether they should be
   merged, please do it or comment on what should or should not be done. */
Context *
Context::create_unique_context (SCM name, const string &id, SCM operations)
{
  /*
    Don't create multiple score contexts.
  */
  Global_context *gthis = dynamic_cast<Global_context *> (this);
  if (gthis)
    {
      if (Context *score = gthis->get_score_context ())
        return score->create_unique_context (name, id, operations);
    }

  vector<Context_def *> path = path_to_acceptable_context (name);
  if (!path.empty ())
    return create_hierarchy (path, NEW_CONTEXT_ID, id, operations);

  /*
    Don't go up to Global_context, because global goes down to the
    Score context
  */
  Context *ret = 0;
  if (daddy_context_ && !dynamic_cast<Global_context *> (daddy_context_))
    ret = daddy_context_->create_unique_context (name, id, operations);
  return ret;
}

Context *
Context::find_create_context (SCM n, const string &id, SCM operations)
{
  // Searching below in recursive calls can find contexts beyond those that are
  // visible when looking just down and up from the starting point.  The
  // regression test lyric-combine-polyphonic.ly has a reasonably simple
  // example of how this can be useful.
  if (Context *existing = find_context_below (this, n, id))
    return existing->is_accessible_to_user () ? existing : nullptr;

  /*
    Don't create multiple score contexts.
  */
  Global_context *gthis = dynamic_cast<Global_context *> (this);
  if (gthis)
    {
      if (Context *score = gthis->get_score_context ())
        return score->find_create_context (n, id, operations);
    }

  vector<Context_def *> path = path_to_acceptable_context (n);
  if (!path.empty ())
    return create_hierarchy (path, "", id, operations);

  /*
    Don't go up to Global_context, because global goes down to the
    Score context
  */
  if (daddy_context_ && !dynamic_cast<Global_context *> (daddy_context_))
    return daddy_context_->find_create_context (n, id, operations);

  return 0;
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
  if (here_defined (sym, &val))
    {
      return scm_list_4 (ly_context_set_property_x_proc, self_scm (), sym, val);
    }
  else
    {
      return scm_list_3 (ly_context_unset_property_proc, self_scm (), sym);
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

  SCM sym = ev->get_property ("symbol");
  if (scm_is_symbol (sym))
    {
      SCM val = ev->get_property ("value");

      if (SCM_UNBNDP (val))
        {
          // TODO: It looks like this ignores \once.
          // Should this be unset_property_from event (sev)?
          unset_property (sym);
          return;
        }

      bool ok = true;
      ok = type_check_assignment (sym, val,
                                  ly_symbol2scm ("translation-type?"));

      if (ok)
        {
          if (to_boolean (ev->get_property ("once")))
            add_global_finalization (make_revert_finalization (sym));
          set_property (sym, val);
        }
    }
}

void
Context::unset_property_from_event (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);

  SCM sym = ev->get_property ("symbol");
  bool ok = type_check_assignment (sym, SCM_EOL,
                                   ly_symbol2scm ("translation-type?"));

  if (ok)
    {
      if (to_boolean (ev->get_property ("once")))
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

  string id = ly_scm2string (ev->get_property ("id"));
  SCM ops = ev->get_property ("ops");
  SCM type_scm = ev->get_property ("type");
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

  Context *new_context = cdef->instantiate (ops);

  new_context->id_string_ = id;

  /* Register various listeners:
      - Make the new context hear events that universally affect contexts
      - connect events_below etc. properly */
  /* We want to be the first ones to hear our own events. Therefore, wait
     before registering events_below_ */
  new_context->event_source ()->add_listener (
      new_context->GET_LISTENER (Context, create_context_from_event),
      ly_symbol2scm ("CreateContext"));
  new_context->event_source ()->add_listener (
      new_context->GET_LISTENER (Context, remove_context),
      ly_symbol2scm ("RemoveContext"));
  new_context->event_source ()->add_listener (
      new_context->GET_LISTENER (Context, change_parent),
      ly_symbol2scm ("ChangeParent"));
  new_context->event_source ()->add_listener (
      new_context->GET_LISTENER (Context, set_property_from_event),
      ly_symbol2scm ("SetProperty"));
  new_context->event_source ()->add_listener (
      new_context->GET_LISTENER (Context, unset_property_from_event),
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
  event_source_->add_listener (GET_LISTENER (Context, acknowledge_infant),
                               ly_symbol2scm ("AnnounceNewContext"));
  /* The CreateContext creates a new context, and sends an announcement of the
     new context through another event. That event will be stored in
     infant_event_ to create a return value. */
  send_stream_event (this, "CreateContext", 0, ly_symbol2scm ("ops"), ops,
                     ly_symbol2scm ("type"), cdef->get_context_name (),
                     ly_symbol2scm ("id"), ly_string2scm (id));
  event_source_->remove_listener (GET_LISTENER (Context, acknowledge_infant),
                                  ly_symbol2scm ("AnnounceNewContext"));

  assert (infant_event_);
  SCM infant_scm = infant_event_->get_property ("context");
  Context *infant = unsmob<Context> (infant_scm);

  if (!infant || infant->get_parent_context () != this)
    {
      programming_error ("create_context: can't locate newly created context");
      return 0;
    }

  return infant;
}

// Create new contexts below this context for each path element in order.  Use
// leaf_id and leaf_operations for the last; use intermediate_id and no
// operations for the rest.
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
      if (path.size () > 1)
        {
          for (vsize i = 0; i < path.size () - 1; ++i)
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
  if (Context *c = create_unique_context (name, id, SCM_EOL))
    return c;

  // TODO: Avoiding a null return means the caller does not detect this
  // failure, so we have to log here if we want to log at all.  It would be
  // more flexible to return null and let the caller decide whether to warn in
  // its situation.  The caller might know a useful source location too.
  warning (_f ("cannot find or create context: %s",
               diagnostic_id (name, id).c_str ()));
  return this;
}

/*
  PROPERTIES
*/
Context *
Context::where_defined (SCM sym, SCM *value) const
{
#ifdef DEBUG
  if (profile_property_accesses)
    note_property_access (&context_property_lookup_table, sym);
#endif

  if (properties_dict ()->try_retrieve (sym, value))
    return (Context *)this;

  return (daddy_context_) ? daddy_context_->where_defined (sym, value) : 0;
}

/* Quick variant of where_defined.  Checks only the context itself. */

bool
Context::here_defined (SCM sym, SCM *value) const
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

  if (daddy_context_)
    return daddy_context_->internal_get_property (sym);

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
  e->set_property (prop, val);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                     SCM prop2, SCM val2)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  e->set_property (prop, val);
  e->set_property (prop2, val2);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                     SCM prop2, SCM val2, SCM prop3, SCM val3)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  e->set_property (prop, val);
  e->set_property (prop2, val2);
  e->set_property (prop3, val3);
  event_source_->broadcast (e);
  e->unprotect ();
}

void
Context::internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                     SCM prop2, SCM val2, SCM prop3, SCM val3,
                                     SCM prop4, SCM val4)
{
  Stream_event *e = new Stream_event (Lily::ly_make_event_class (type), origin);
  e->set_property (prop, val);
  e->set_property (prop2, val2);
  e->set_property (prop3, val3);
  e->set_property (prop4, val4);
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
  Context *to = unsmob<Context> (ev->get_property ("context"));

  disconnect_from_parent ();
  to->add_context (this);
}

/*
  Die. The next GC sweep should take care of the actual death.
 */
void Context::remove_context (SCM)
{
  /* ugh, the translator group should listen to RemoveContext events by itself
   */
  Translator_group *impl = implementation ();
  if (impl)
    impl->disconnect_from_context ();
  disconnect_from_parent ();
}

void
Context::disconnect_from_parent ()
{
  daddy_context_->events_below_->unregister_as_listener (events_below_);
  daddy_context_->context_list_
      = scm_delq_x (self_scm (), daddy_context_->context_list_);
  daddy_context_ = 0;
}

Context *
find_context_above (Context *where, SCM type)
{
  while (where && !where->is_alias (type))
    where = where->get_parent_context ();

  return where;
}

Context *
find_context_above_by_parent_type (Context *where, SCM parent_type)
{
  while (Context *parent = where->get_parent_context ())
    {
      if (parent->is_alias (parent_type))
        return where;
      where = parent;
    }
  return 0;
}

Context *
find_context_below (Context *where, SCM type, const string &id)
{
  if (where->is_alias (type))
    {
      if (id == "" || where->id_string () == id)
        return where;
    }

  Context *found = 0;
  for (SCM s = where->children_contexts (); !found && scm_is_pair (s);
       s = scm_cdr (s))
    {
      Context *tr = unsmob<Context> (scm_car (s));

      found = find_context_below (tr, type, id);
    }

  return found;
}

Context *
find_context_near (Context *where, SCM type, const string &id)
{
  for (; where; where = where->get_parent_context ())
    {
      Context *found = find_context_below (where, type, id);
      if (found)
        return found;
    }

  return 0;
}

Context *
find_top_context (Context *where)
{
  Context *top = where;
  for (; where; where = where->get_parent_context ())
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
  return daddy_context_ ? daddy_context_->get_output_def () : 0;
}

Context::~Context () {}

Moment
Context::now_mom () const
{
  Context const *p = this;
  while (p->daddy_context_)
    p = p->daddy_context_;

  return p->now_mom ();
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

Context *
Context::get_parent_context () const
{
  return daddy_context_;
}

/*
  Ugh. Where to put this?
*/
Rational
measure_length (Context const *context)
{
  SCM l = context->get_property ("measureLength");
  Rational length (1);
  if (unsmob<Moment> (l))
    length = unsmob<Moment> (l)->main_part_;
  return length;
}

Moment
measure_position (Context const *context)
{
  SCM sm = context->get_property ("measurePosition");

  Moment m = 0;
  if (unsmob<Moment> (sm))
    {
      m = *unsmob<Moment> (sm);

      if (m.main_part_ < Rational (0))
        {
          Rational length (measure_length (context));
          while (m.main_part_ < Rational (0))
            m.main_part_ += length;
        }
    }

  return m;
}

/* Finds the measure position after a note of length DUR that
   begins at the current measure position. */
Moment
measure_position (Context const *context, Duration const *dur)
{
  Moment pos = measure_position (context);
  Rational dur_length = dur ? dur->get_length () : Rational (0);

  Moment end_pos = pos.grace_part_ < Rational (0)
                       ? Moment (pos.main_part_, pos.grace_part_ + dur_length)
                       : Moment (pos.main_part_ + dur_length, 0);

  return end_pos;
}

int
measure_number (Context const *context)
{
  SCM barnum = context->get_property ("internalBarNumber");
  SCM smp = context->get_property ("measurePosition");

  int bn = robust_scm2int (barnum, 0);
  Moment mp = robust_scm2moment (smp, Moment (0));
  if (mp.main_part_ < Rational (0))
    bn--;

  return bn;
}

void
set_context_property_on_children (Context *trans, SCM sym, SCM val)
{
  trans->set_property (sym, ly_deep_copy (val));
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

  for (SCM melisma_properties = tr->get_property ("melismaBusyProperties");
       scm_is_pair (melisma_properties);
       melisma_properties = scm_cdr (melisma_properties))
    if (to_boolean (tr->get_property (scm_car (melisma_properties))))
      return true;

  return false;
}

bool
check_repeat_count_visibility (Context const *context, SCM count)
{
  SCM proc = context->get_property ("repeatCountVisibility");
  return (ly_is_procedure (proc)
          && to_boolean (scm_call_2 (proc, count, context->self_scm ())));
}

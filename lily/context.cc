/*
  context.cc -- implement Context

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"

#include "context-def.hh"
#include "dispatcher.hh"
#include "global-context.hh"
#include "international.hh"
#include "ly-smobs.icc"
#include "main.hh"
#include "output-def.hh"
#include "profile.hh"
#include "program-option.hh"
#include "scm-hash.hh"
#include "translator-group.hh"
#include "warn.hh"

bool
Context::is_removable () const
{
  return context_list_ == SCM_EOL && ! iterator_count_
    && !dynamic_cast<Global_context const *> (daddy_context_);
}

void
Context::check_removal ()
{
  for (SCM p = context_list_; scm_is_pair (p); p = scm_cdr (p))
    {
      Context *ctx = unsmob_context (scm_car (p));

      ctx->check_removal ();
      if (ctx->is_removable ())
	{
	  recurse_over_translators (ctx, &Translator::finalize,
				    &Translator_group::finalize,
				    UP);
	  send_stream_event (ctx, "RemoveContext", 0, 0);
	}
    }
}

Context::Context (Context const &src)
{
  (void) src;
  assert (false);
}

Scheme_hash_table *
Context::properties_dict () const
{
  return Scheme_hash_table::unsmob (properties_scm_);
}

void
Context::add_context (Context *child)
{
  context_list_ = ly_append2 (context_list_,
			      scm_cons (child->self_scm (), SCM_EOL));

  child->daddy_context_ = this;
  this->events_below_->register_as_listener (child->events_below_);
}


Context::Context ()
{
  daddy_context_ = 0;
  aliases_ = SCM_EOL;
  iterator_count_ = 0;
  implementation_ = 0;
  properties_scm_ = SCM_EOL;
  accepts_list_ = SCM_EOL;
  context_list_ = SCM_EOL;
  definition_ = SCM_EOL;
  definition_mods_ = SCM_EOL;
  event_source_ = 0;
  events_below_ = 0;

  smobify_self ();

  Scheme_hash_table *tab = new Scheme_hash_table;
  properties_scm_ = tab->unprotect ();
  event_source_ = new Dispatcher ();
  event_source_->unprotect ();
  events_below_ = new Dispatcher ();
  events_below_->unprotect ();
}

/* TODO:  this shares code with find_create_context ().  */
Context *
Context::create_unique_context (SCM name, string id, SCM operations)
{
  /*
    Don't create multiple score contexts.
  */
  Global_context *gthis = dynamic_cast<Global_context *> (this);
  if (gthis && gthis->get_score_context ())
    return gthis->get_score_context ()->create_unique_context (name, id, operations);

  vector<Context_def*> path = path_to_acceptable_context (name);
  if (path.size ())
    {
      Context *current = this;

      // Iterate through the path and create all of the implicit contexts.
      for (vsize i = 0; i < path.size (); i++)
	{
	  SCM ops = SCM_EOL;
	  string id_str = "\\new";
	  if (i == path.size () - 1)
	    {
	      ops = operations;
	      id_str = id;
	    }
	  current = current->create_context (path[i],
					     id_str,
					     ops);
	}

      return current;
    }

  /*
    Don't go up to Global_context, because global goes down to the
    Score context
  */
  Context *ret = 0;
  if (daddy_context_ && !dynamic_cast<Global_context *> (daddy_context_))
    ret = daddy_context_->create_unique_context (name, id, operations);
  else
    {
      warning (_f ("cannot find or create new `%s'",
		   ly_symbol2string (name).c_str ()));
      ret = 0;
    }
  return ret;
}

Context *
Context::find_create_context (SCM n, string id, SCM operations)
{
  /*
    Don't create multiple score contexts.
  */
  Global_context *gthis = dynamic_cast<Global_context *> (this);
  if (gthis && gthis->get_score_context ())
    return gthis->get_score_context ()->find_create_context (n, id, operations);

  if (Context *existing = find_context_below (this, n, id))
    return existing;

  if (n == ly_symbol2scm ("Bottom"))
    {
      Context *tg = get_default_interpreter ();
      return tg;
    }

  vector<Context_def*> path = path_to_acceptable_context (n);

  if (path.size ())
    {
      Context *current = this;

      // start at 1.  The first one (index 0) will be us.
      for (vsize i = 0; i < path.size (); i++)
	{
	  SCM ops = (i == path.size () -1) ? operations : SCM_EOL;

	  string this_id = "";
	  if (i == path.size () -1)
	    this_id = id;

	  current = current->create_context (path[i],
					     this_id,
					     ops);
	}

      return current;
    }

  /*
    Don't go up to Global_context, because global goes down to the
    Score context
  */
  Context *ret = 0;
  if (daddy_context_ && !dynamic_cast<Global_context *> (daddy_context_))
    ret = daddy_context_->find_create_context (n, id, operations);
  else
    {
      warning (_f ("cannot find or create `%s' called `%s'",
		   ly_symbol2string (n).c_str (), id));
      ret = 0;
    }
  return ret;
}

IMPLEMENT_LISTENER (Context, acknowledge_infant);
void
Context::acknowledge_infant (SCM sev)
{
  infant_event_ = unsmob_stream_event (sev);
}

IMPLEMENT_LISTENER (Context, set_property_from_event);
void
Context::set_property_from_event (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  
  SCM sym = ev->get_property ("symbol");
  if (scm_is_symbol (sym))
    {
      SCM val = ev->get_property ("value");
      bool ok = true;
      if (val != SCM_EOL)
	ok = type_check_assignment (sym, val, ly_symbol2scm ("translation-type?"));
      if (ok)
	set_property (sym, val);
    }
}

IMPLEMENT_LISTENER (Context, unset_property_from_event);
void
Context::unset_property_from_event (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  
  SCM sym = ev->get_property ("symbol");
  type_check_assignment (sym, SCM_EOL, ly_symbol2scm ("translation-type?"));
  unset_property (sym);
}

/*
  Creates a new context from a CreateContext event, and sends an
  AnnounceNewContext event to this context.
*/
IMPLEMENT_LISTENER (Context, create_context_from_event);
void
Context::create_context_from_event (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  
  string id = ly_scm2string (ev->get_property ("id"));
  SCM ops = ev->get_property ("ops");
  SCM type_scm = ev->get_property ("type");
  string type = ly_symbol2string (type_scm);
  
  vector<Context_def*> path = path_to_acceptable_context (type_scm);

  if (path.size () != 1)
    {
      programming_error (_f ("Invalid CreateContext event: Cannot create %s context", type.c_str ()));
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
  new_context->event_source ()->
    add_listener (GET_LISTENER (new_context->create_context_from_event),
                  ly_symbol2scm ("CreateContext"));
  new_context->event_source ()->
    add_listener (GET_LISTENER (new_context->remove_context),
                  ly_symbol2scm ("RemoveContext"));
  new_context->event_source ()->
    add_listener (GET_LISTENER (new_context->change_parent),
                  ly_symbol2scm ("ChangeParent"));
  new_context->event_source ()->
    add_listener (GET_LISTENER (new_context->set_property_from_event),
                  ly_symbol2scm ("SetProperty"));
  new_context->event_source ()->
    add_listener (GET_LISTENER (new_context->unset_property_from_event),
                  ly_symbol2scm ("UnsetProperty"));

  new_context->events_below_->register_as_listener (new_context->event_source_);
  this->add_context (new_context);

  new_context->unprotect ();

  Context_def *td = unsmob_context_def (new_context->definition_);

  /* This cannot move before add_context (), because \override
     operations require that we are in the hierarchy.  */
  td->apply_default_property_operations (new_context);
  apply_property_operations (new_context, ops);

  send_stream_event (this, "AnnounceNewContext", 0,
  		     ly_symbol2scm ("context"), new_context->self_scm (),
  		     ly_symbol2scm ("creator"), sev);
}

vector<Context_def*>
Context::path_to_acceptable_context (SCM name) const
{
  // The 'accepts elements in definition_mods_ is a list of ('accepts string),
  // but the Context_def expects to see elements of the form ('accepts symbol).
  SCM accepts = SCM_EOL;
  for (SCM s = scm_reverse (definition_mods_); scm_is_pair (s); s = scm_cdr (s))
    if (scm_caar (s) == ly_symbol2scm ("accepts"))
      {
	SCM elt = scm_list_2 (scm_caar (s), scm_string_to_symbol (scm_cadar (s)));
	accepts = scm_cons (elt, accepts);
      }

  return unsmob_context_def (definition_)->path_to_acceptable_context (name,
								       get_output_def (),
								       accepts);
								       
}

Context *
Context::create_context (Context_def *cdef,
			 string id,
			 SCM ops)
{
  infant_event_ = 0;
  /* TODO: This is fairly misplaced. We can fix this when we have taken out all
     iterator specific stuff from the Context class */
  event_source_->
    add_listener (GET_LISTENER (acknowledge_infant),
                  ly_symbol2scm ("AnnounceNewContext"));
  /* The CreateContext creates a new context, and sends an announcement of the
     new context through another event. That event will be stored in
     infant_event_ to create a return value. */
  send_stream_event (this, "CreateContext", 0,
                     ly_symbol2scm ("ops"), ops,
                     ly_symbol2scm ("type"), cdef->get_context_name (),
                     ly_symbol2scm ("id"), ly_string2scm (id));
  event_source_->
    remove_listener (GET_LISTENER (acknowledge_infant),
                     ly_symbol2scm ("AnnounceNewContext"));

  assert (infant_event_);
  SCM infant_scm = infant_event_->get_property ("context");
  Context *infant = unsmob_context (infant_scm);

  if (!infant || infant->get_parent_context () != this)
    {
      programming_error ("create_context: can't locate newly created context");
      return 0;
    }

  return infant;
}

/*
  Default child context as a SCM string, or something else if there is
  none.
*/
SCM
Context::default_child_context_name () const
{
  return scm_is_pair (accepts_list_)
    ? scm_car (accepts_list_)
    : SCM_EOL;
}

bool
Context::is_bottom_context () const
{
  return !scm_is_symbol (default_child_context_name ());
}

Context *
Context::get_default_interpreter ()
{
  if (!is_bottom_context ())
    {
      SCM nm = default_child_context_name ();
      SCM st = find_context_def (get_output_def (), nm);

      string name = ly_symbol2string (nm);
      Context_def *t = unsmob_context_def (st);
      if (!t)
	{
	  warning (_f ("cannot find or create: `%s'", name.c_str ()));
	  t = unsmob_context_def (this->definition_);
	}

      Context *tg = create_context (t, "", SCM_EOL);
      return tg->get_default_interpreter ();
    }
  return this;
}

/*
  PROPERTIES
*/
Context *
Context::where_defined (SCM sym, SCM *value) const
{
#ifndef NDEBUG
  if (profile_property_accesses)
    note_property_access (&context_property_lookup_table, sym);
#endif

  if (properties_dict ()->try_retrieve (sym, value))
    return (Context *)this;

  return (daddy_context_) ? daddy_context_->where_defined (sym, value) : 0;
}

/*
  return SCM_EOL when not found.
*/
SCM
Context::internal_get_property (SCM sym) const
{
#ifndef NDEBUG
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
Called by the send_stream_event macro. props is a 0-terminated array of
properties and corresponding values, interleaved. This method should not
be called from any other place than the send_stream_event macro.
*/
void
Context::internal_send_stream_event (SCM type, Input *origin, SCM props[])
{
  Stream_event *e = new Stream_event (type, origin);
  for (int i = 0; props[i]; i += 2)
    {
      e->set_property (props[i], props[i+1]);
    }
  event_source_->broadcast (e);
  e->unprotect ();
}

bool
Context::is_alias (SCM sym) const
{
  if (sym == ly_symbol2scm ("Bottom")
      && !scm_is_pair (accepts_list_))
    return true;
  if (sym == unsmob_context_def (definition_)->get_context_name ())
    return true;

  return scm_c_memq (sym, aliases_) != SCM_BOOL_F;
}

void
Context::add_alias (SCM sym)
{
  aliases_ = scm_cons (sym, aliases_);
}

/* we don't (yet) instrument context properties */
void
Context::instrumented_set_property (SCM sym, SCM val, const char*, int, const char*)
{
  internal_set_property (sym, val);
}

void
Context::internal_set_property (SCM sym, SCM val)
{
  if (do_internal_type_checking_global)
    assert (type_check_assignment (sym, val, ly_symbol2scm ("translation-type?")));

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

IMPLEMENT_LISTENER (Context, change_parent);
void
Context::change_parent (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  Context *to = unsmob_context (ev->get_property ("context"));

  disconnect_from_parent ();
  to->add_context (this);
}

/*
  Die. The next GC sweep should take care of the actual death.
 */
IMPLEMENT_LISTENER (Context, remove_context);
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
  daddy_context_->events_below_->unregister_as_listener (this->events_below_);
  daddy_context_->context_list_ = scm_delq_x (this->self_scm (), daddy_context_->context_list_);
  daddy_context_ = 0;
}

/*
  ID == "" means accept any ID.
*/
Context *
find_context_below (Context *where,
		    SCM type, string id)
{
  if (where->is_alias (type))
    {
      if (id == "" || where->id_string () == id)
	return where;
    }

  Context *found = 0;
  for (SCM s = where->children_contexts ();
       !found && scm_is_pair (s); s = scm_cdr (s))
    {
      Context *tr = unsmob_context (scm_car (s));

      found = find_context_below (tr, type, id);
    }

  return found;
}

SCM
Context::properties_as_alist () const
{
  return properties_dict ()->to_alist ();
}

SCM
Context::context_name_symbol () const
{
  Context_def *td = unsmob_context_def (definition_);
  return td->get_context_name ();
}

string
Context::context_name () const
{
  return ly_symbol2string (context_name_symbol ());
}

Context *
Context::get_score_context () const
{
  if (daddy_context_)
    return daddy_context_->get_score_context ();
  else
    return 0;
}

Output_def *
Context::get_output_def () const
{
  return daddy_context_ ? daddy_context_->get_output_def () : 0;
}

Context::~Context ()
{
}

Moment
Context::now_mom () const
{
  Context const *p = this;
  while (p->daddy_context_)
    p = p->daddy_context_;

  return p->now_mom ();
}

int
Context::print_smob (SCM s, SCM port, scm_print_state *)
{
  Context *sc = (Context *) SCM_CELL_WORD_1 (s);

  scm_puts ("#<", port);
  scm_puts (sc->class_name (), port);
  if (Context_def *d = unsmob_context_def (sc->definition_))
    {
      scm_puts (" ", port);
      scm_display (d->get_context_name (), port);
    }

  if (!sc->id_string_.empty ())
    {
      scm_puts ("=", port);
      scm_puts (sc->id_string_.c_str (), port);
    }

  scm_puts (" ", port);

  scm_display (sc->context_list_, port);
  scm_puts (" >", port);

  return 1;
}

SCM
Context::mark_smob (SCM sm)
{
  Context *me = (Context *) SCM_CELL_WORD_1 (sm);

  scm_gc_mark (me->context_list_);
  scm_gc_mark (me->aliases_);
  scm_gc_mark (me->definition_);
  scm_gc_mark (me->definition_mods_);
  scm_gc_mark (me->properties_scm_);
  scm_gc_mark (me->accepts_list_);

  if (me->implementation_)
    scm_gc_mark (me->implementation_->self_scm ());

  if (me->event_source_)
    scm_gc_mark (me->event_source_->self_scm ());

  if (me->events_below_)
    scm_gc_mark (me->events_below_->self_scm ());

  return me->properties_scm_;
}

IMPLEMENT_SMOBS (Context);
IMPLEMENT_DEFAULT_EQUAL_P (Context);
IMPLEMENT_TYPE_P (Context, "ly:context?");

Global_context *
Context::get_global_context () const
{
  if (dynamic_cast<Global_context *> ((Context *) this))
    return dynamic_cast<Global_context *> ((Context *) this);

  if (daddy_context_)
    return daddy_context_->get_global_context ();

  programming_error ("no Global context");
  return 0;
}

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
  if (unsmob_moment (l))
    length = unsmob_moment (l)->main_part_;
  return length;
}

Moment
measure_position (Context const *context)
{
  SCM sm = context->get_property ("measurePosition");

  Moment m = 0;
  if (unsmob_moment (sm))
    {
      m = *unsmob_moment (sm);

      if (m.main_part_ < Rational (0))
	{
	  Rational length (measure_length (context));
	  while (m.main_part_ < Rational (0))
	    m.main_part_ += length;
	}
    }

  return m;
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
      Context *trg = unsmob_context (scm_car (p));
      set_context_property_on_children (trg, sym, ly_deep_copy (val));
    }
}

bool
melisma_busy (Context *tr)
{
  SCM melisma_properties = tr->get_property ("melismaBusyProperties");
  bool busy = false;

  for (; !busy && scm_is_pair (melisma_properties);
       melisma_properties = scm_cdr (melisma_properties))
    busy = busy || to_boolean (tr->internal_get_property (scm_car (melisma_properties)));

  return busy;
}

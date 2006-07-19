/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "translator.hh"

#include "warn.hh"
#include "translator-group.hh"
#include "context-def.hh"
#include "dispatcher.hh"
#include "global-context.hh"

#include "translator.icc"
#include "ly-smobs.icc"

Translator::~Translator ()
{
}

void
Translator::init ()
{
  must_be_last_ = false;
  self_scm_ = SCM_EOL;
  daddy_context_ = 0;
  smobify_self ();
}

void
Translator::process_music ()
{
}

void
Translator::process_acknowledged ()
{
}

Translator::Translator ()
{
  init ();
}

Translator::Translator (Translator const &src)
{
  init ();
  must_be_last_ = src.must_be_last_;
}

bool
Translator::try_music (Music *)
{
  return false;
}

Moment
Translator::now_mom () const
{
  return daddy_context_->now_mom ();
}

Output_def *
Translator::get_output_def () const
{
  return daddy_context_->get_output_def ();
}

Translator_group *
Translator::get_daddy_translator () const
{
  return daddy_context_->implementation ();
}

void
Translator::protect_event (SCM ev)
{
  get_daddy_translator ()->protect_event (ev);
}

SCM
Translator::internal_get_property (SCM sym) const
{
  return daddy_context_->internal_get_property (sym);
}

void
Translator::stop_translation_timestep ()
{
}

/*
  this function has 2 properties

  - It is called before try_music ()

  - It is called before any user information enters the translators.
  (i.e. any \property or event is not processed yet.)
*/
void
Translator::start_translation_timestep ()
{
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
  for (translator_listener_record *r = get_listener_list (); r; r=r->next_)
    c->events_below ()->add_listener (r->get_listener_ (this), r->event_class_);
}

void
Translator::disconnect_from_context (Context *c)
{
  for (translator_listener_record *r = get_listener_list (); r; r=r->next_)
    c->events_below ()->remove_listener (r->get_listener_ (this), r->event_class_);
}

/*
  Internally called once, statically, for each translator
  listener. Connects the name of an event class with a procedure that
  fetches the corresponding listener.

  The method should only be called from the macro
  IMPLEMENT_TRANSLATOR_LISTENER.
 */
void
Translator::add_translator_listener (translator_listener_record **listener_list,
			 translator_listener_record *r,
			 Listener (*get_listener) (void *), 
			 const char *ev_class)
{
  /* ev_class is the C++ identifier name. Convert to scm symbol */
  string name = string (ev_class);
  name = replace_all (name, '_', '-');
  name = name + "-event";
  /* It's OK to use scm_gc_protect_object for protection, because r is
     statically allocated. */
  r->event_class_ = scm_gc_protect_object (scm_str2symbol (name.c_str ()));
  r->get_listener_ = get_listener;
  r->next_ = *listener_list;
  *listener_list = r;
}

/*
  SMOBS
*/
SCM
Translator::mark_smob (SCM sm)
{
  Translator *me = (Translator *) SCM_CELL_WORD_1 (sm);
  me->derived_mark ();
  return SCM_EOL;
}

Global_context *
Translator::get_global_context () const
{
  return daddy_context_->get_global_context ();
}

Context *
Translator::get_score_context () const
{
  return daddy_context_->get_score_context ();
}

IMPLEMENT_SMOBS (Translator);
IMPLEMENT_DEFAULT_EQUAL_P (Translator);
IMPLEMENT_TYPE_P (Translator, "ly:translator?");

bool
Translator::must_be_last () const
{
  return must_be_last_;
}

void
Translator::derived_mark () const
{
}

int
Translator::print_smob (SCM s, SCM port, scm_print_state *)
{
  Translator *me = (Translator *) SCM_CELL_WORD_1 (s);
  scm_puts ("#<Translator ", port);
  scm_puts (me->class_name (), port);
  scm_puts (" >", port);
  return 1;
}

void
add_acknowledger (Engraver_void_function_engraver_grob_info ptr,
		  char const *func_name,
		  vector<Acknowledge_information> *ack_array)
{
  Acknowledge_information inf;
  inf.function_ = ptr;

  string interface_name (func_name);

  interface_name = replace_all (interface_name, '_', '-');
  interface_name += "-interface";

  inf.symbol_ = scm_gc_protect_object (ly_symbol2scm (interface_name.c_str ()));
  ack_array->push_back (inf);
}

Engraver_void_function_engraver_grob_info
generic_get_acknowledger (SCM sym, vector<Acknowledge_information> const *ack_array)
{
  for (vsize i = 0; i < ack_array->size (); i++)
    {
      if (ack_array->at (i).symbol_ == sym)
	return ack_array->at (i).function_;
    }
  return 0;
}

Moment
get_event_length (Stream_event *e)
{
  Moment *m = unsmob_moment (e->get_property ("length"));
  if (m)
    return *m;
  else
    return Moment (0);
}

ADD_TRANSLATOR (Translator,
		"Base class. Unused",
		"",
		"",
		"",
		"");

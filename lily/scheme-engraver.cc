/* 
  scheme-engraver.cc -- implement Scheme_engraver
  
  source file of the GNU LilyPond music typesetter
  
  Copyright (c) 2009--2010 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#include "scheme-engraver.hh"

#include "grob.hh"

#include "translator.icc"

Scheme_engraver::Scheme_engraver ()
{
  stop_translation_timestep_function_ = SCM_EOL;
  start_translation_timestep_function_ = SCM_EOL;
  process_music_function_ = SCM_EOL;
  process_acknowledged_function_ = SCM_EOL;
  initialize_function_ = SCM_EOL;
  finalize_function_ = SCM_EOL;
  listeners_alist_ = SCM_EOL;

  interface_acknowledger_hash_ = SCM_EOL;
  interface_end_acknowledger_hash_ = SCM_EOL;

  must_be_last_ = false;
  per_instance_listeners_ = 0;
}

Scheme_engraver::~Scheme_engraver ()
{
  translator_listener_record *next = 0;
  for (translator_listener_record *r = per_instance_listeners_;
       r; r = next)
    {
      next = r->next_;
      delete r;
    }
}

// Extracts the value if callable, if not return #f.
static SCM
callable (SCM symbol, SCM defn)
{
  SCM val = ly_assoc_get (symbol, defn, SCM_BOOL_F);
  return ly_is_procedure (val) ? val : SCM_BOOL_F;
}

bool
Scheme_engraver::must_be_last () const
{
  return must_be_last_;
}

void
Scheme_engraver::init_from_scheme (SCM definition)
{
  start_translation_timestep_function_ = callable (ly_symbol2scm ("start-translation-timestep"),
						   definition);
  stop_translation_timestep_function_ = callable (ly_symbol2scm ("stop-translation-timestep"),
						  definition);
  process_music_function_ = callable (ly_symbol2scm ("process-music"), definition);
  process_acknowledged_function_ = callable (ly_symbol2scm ("process-acknowledged"),
					     definition);
  initialize_function_ = callable (ly_symbol2scm ("initialize"), definition);
  finalize_function_ = callable (ly_symbol2scm ("finalize"), definition);

  SCM listeners = ly_assoc_get (ly_symbol2scm ("listeners"), definition, SCM_EOL);

  listeners_alist_ = SCM_EOL;

  must_be_last_ = to_boolean (ly_assoc_get (ly_symbol2scm ("must-be-last"),
					    definition,
					    SCM_BOOL_F));
				
  translator_listener_record **tail = &per_instance_listeners_;
  for (SCM p = listeners; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM event_class = scm_caar (p);
      SCM proc = scm_cdar (p);

      if (!(ly_is_procedure (proc) && ly_is_symbol (event_class)))
	continue;

      // We should check the arity of the function?
      
      // Record for later lookup.
      listeners_alist_ = scm_acons (event_class, proc, listeners_alist_);

      translator_listener_record *rec = new translator_listener_record;
      *tail = rec;
      rec->event_class_ = event_class;
      rec->get_listener_ = &Scheme_engraver::get_listener;
      tail = &rec->next_;
    }

  init_acknowledgers (ly_assoc_get(ly_symbol2scm ("acknowledgers"),
				   definition, SCM_EOL),
		      &interface_acknowledger_hash_);
  
  init_acknowledgers (ly_assoc_get(ly_symbol2scm ("end-acknowledgers"),
				   definition, SCM_EOL),
		      &interface_end_acknowledger_hash_);

  // TODO: hook up description, props read/written, grobs created
  // etc. to provide automatic documentation.
}

void
Scheme_engraver::init_acknowledgers (SCM alist,
				     SCM *hash)
{  
  *hash = scm_c_make_hash_table(7);
  for (SCM p = alist; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM iface = scm_caar (p);
      SCM proc = scm_cdar (p);

      if (!(ly_is_procedure (proc) && ly_is_symbol (iface)))
	continue;

      scm_hashq_set_x (*hash, iface, proc);
    }
}

// This is the easy way to do it, at the cost of too many invocations
// of Scheme_engraver::acknowledge_grob.  The clever dispatching of
// acknowledgers is hardwired to have 1 method per engraver per
// grob-type, which doesn't work for this case. 
void
Scheme_engraver::acknowledge_grob (Grob_info info)
{
  acknowledge_grob_by_hash (info, interface_acknowledger_hash_);
}

void
Scheme_engraver::acknowledge_end_grob (Grob_info info)
{
  acknowledge_grob_by_hash (info, interface_end_acknowledger_hash_);
}

void
Scheme_engraver::acknowledge_grob_by_hash (Grob_info info,
					   SCM iface_function_hash)
{
  SCM meta = info.grob ()->internal_get_property (ly_symbol2scm ("meta"));
  SCM ifaces = scm_cdr (scm_assoc (ly_symbol2scm ("interfaces"), meta));
  for (SCM s = ifaces; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM func = scm_hashq_ref (iface_function_hash,
				scm_car (s), SCM_BOOL_F);

      if (ly_is_procedure (func))
	scm_call_3 (func, self_scm (), info.grob ()->self_scm (),
		    info.origin_translator ()->self_scm ());
    }
}

static
void call_listen_closure (void *target, SCM ev)
{
  SCM cl = (SCM) target;
  SCM func = scm_car (cl);
  SCM engraver = scm_cdr (cl);
  scm_call_2 (func, engraver, ev);
}

static
void mark_listen_closure (void *target)
{
  scm_gc_mark ((SCM)target);
}

Listener_function_table listen_closure = {
  call_listen_closure, mark_listen_closure
};

/* static */
Listener
Scheme_engraver::get_listener (void *arg, SCM name)
{
  Scheme_engraver *me = (Scheme_engraver*) arg;
  SCM func = ly_assoc_get (name, me->listeners_alist_, SCM_BOOL_F);
  assert (ly_is_procedure (func));

  SCM closure = scm_cons (func, me->self_scm());
  return Listener((void*)closure, &listen_closure);
}

translator_listener_record *
Scheme_engraver::get_listener_list () const				
{									
  return per_instance_listeners_;
}

#define DISPATCH(what)					\
  void							\
  Scheme_engraver::what ()				\
  {							\
    if (what ## _function_ != SCM_BOOL_F)		\
      scm_call_1 (what ## _function_, self_scm ());	\
  }

DISPATCH(start_translation_timestep);
DISPATCH(stop_translation_timestep);
DISPATCH(initialize);
DISPATCH(finalize);
DISPATCH(process_music);
DISPATCH(process_acknowledged);

void
Scheme_engraver::derived_mark () const
{
  scm_gc_mark (start_translation_timestep_function_);
  scm_gc_mark (stop_translation_timestep_function_);
  scm_gc_mark (initialize_function_);
  scm_gc_mark (finalize_function_);
  scm_gc_mark (process_music_function_);
  scm_gc_mark (process_acknowledged_function_);
  scm_gc_mark (listeners_alist_);    
  scm_gc_mark (interface_acknowledger_hash_);    
  scm_gc_mark (interface_end_acknowledger_hash_);    
} 

ADD_ACKNOWLEDGER (Scheme_engraver, grob);
ADD_END_ACKNOWLEDGER (Scheme_engraver, grob);

ADD_TRANSLATOR (Scheme_engraver,
		/* doc */
		"Implement engravers in Scheme.  Interprets arguments to @code{\\consists} "
		"as callbacks. ",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

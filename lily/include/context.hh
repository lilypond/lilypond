/*
  context.hh -- declare  Context

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CONTEXT_HH
#define CONTEXT_HH

#include "context-key-manager.hh"
#include "lily-proto.hh"
#include "listener.hh"
#include "moment.hh"
#include "std-vector.hh"
#include "virtual-methods.hh"

class Context
{
  Scheme_hash_table *properties_dict () const;
  Context (Context const &src);

  DECLARE_SMOBS (Context);
  DECLARE_CLASSNAME(Context);
  void terminate ();

private:
  friend class Context_handle;
  int iterator_count_;
  
  /* Used internally by create_context */
  Stream_event *infant_event_;

protected:
  Context *daddy_context_;
  /* The used Context_def */
  SCM definition_;
  /* Additions to the Context_def, given by \with */
  SCM definition_mods_;
  Context_key_manager key_manager_;
  
  SCM properties_scm_;
  SCM context_list_;
  SCM accepts_list_;
  SCM aliases_;
  Translator_group *implementation_;
  string id_string_;
  
  /* Events reported in the context is sent to this dispatcher. */
  Dispatcher *event_source_;

  /* Events reported to this context or recursively in any of its
     children, are sent to this dispatcher. */
  Dispatcher *events_below_;

  // Translator_group is allowed to set implementation_.
  friend class Translator_group;
  // Context_def::instantiate initialises some protected members.
  friend class Context_def;
  // UGH! initialises implementation_
  friend SCM ly_make_global_translator (SCM);
  void clear_key_disambiguations ();

  DECLARE_LISTENER (set_property_from_event);
  DECLARE_LISTENER (unset_property_from_event);
  
public:
  Object_key const *get_grob_key (string name);
  Object_key const *get_context_key (string name, string id);

  string id_string () const { return id_string_; }
  SCM children_contexts () const { return context_list_; }
  SCM default_child_context_name () const;

  Dispatcher *event_source () const { return event_source_; }
  Dispatcher *events_below () const { return events_below_; }
  void internal_send_stream_event (SCM type, Input *origin, SCM props[]);

  SCM get_definition () const { return definition_; }
  SCM get_definition_mods () const { return definition_mods_; }

  Translator_group *implementation () const { return implementation_; }
  Context *get_parent_context () const;
  Context (Object_key const *);

  /* properties:  */
  SCM internal_get_property (SCM name_sym) const;
  SCM properties_as_alist () const;
  Context *where_defined (SCM name_sym, SCM *value) const;
  void unset_property (SCM var_sym);

#ifndef NDEBUG
  void internal_set_property (SCM var_sym, SCM value, char const *file, int line, char const *fun);
#else
  void internal_set_property (SCM var_sym, SCM value);
#endif

  Context *create_context (Context_def *, string, SCM);
  DECLARE_LISTENER (create_context_from_event);
  DECLARE_LISTENER (acknowledge_infant);
  DECLARE_LISTENER (remove_context);
  DECLARE_LISTENER (change_parent);
  void disconnect_from_parent ();
  void check_removal ();
  string context_name () const;
  SCM context_name_symbol () const;
  Global_context *get_global_context () const;

  virtual Context *get_score_context () const;
  virtual Output_def *get_output_def () const;
  virtual Moment now_mom () const;
  virtual Context *get_default_interpreter ();

  bool is_alias (SCM) const;
  void add_alias (SCM);
  void add_context (Context *trans);
  bool is_bottom_context () const;
  bool is_removable () const;

  Context *find_create_context (SCM context_name,
				string id, SCM ops);
  Context *create_unique_context (SCM context_name, string context_id,
				  SCM ops);
  vector<Context*> path_to_acceptable_context (SCM alias,
						  Output_def *) const;
};

/*
  Context arg?
*/

void apply_property_operations (Context *tg, SCM pre_init_ops);
void execute_pushpop_property (Context *trg, SCM prop, SCM eltprop, SCM val);
void execute_general_pushpop_property (Context *context,
				       SCM context_property, SCM grob_property_path, SCM val);
SCM updated_grob_properties (Context *tg, SCM sym);
Context *find_context_below (Context *where,
			     SCM type_sym, string id);
bool melisma_busy (Context *);

Context *get_voice_to_lyrics (Context *lyrics);
Grob *get_current_note_head (Context *voice);
Grob *get_current_rest (Context *voice);
DECLARE_UNSMOB (Context, context);

Moment measure_position (Context const *context);
Rational measure_length (Context const *context);
void set_context_property_on_children (Context *trans, SCM sym, SCM val);

/* Shorthand for creating and broadcasting stream events. */
#define send_stream_event(ctx, type, origin, ...)				\
{									\
  SCM props[] = { __VA_ARGS__, 0 };					\
  ctx->internal_send_stream_event (ly_symbol2scm (type), origin, props);	\
}

#endif /* CONTEXT_HH */


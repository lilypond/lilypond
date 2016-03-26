/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef CONTEXT_HH
#define CONTEXT_HH

#include "duration.hh"
#include "lily-proto.hh"
#include "listener.hh"
#include "moment.hh"
#include "scm-hash.hh"
#include "std-vector.hh"
#include "virtual-methods.hh"

class Context : public Smob<Context>
{
public:
  SCM mark_smob () const;
  int print_smob (SCM, scm_print_state *) const;
  static const char type_p_name_[];
  virtual ~Context ();
private:
  Scheme_hash_table *properties_dict () const;
  Context (Context const &src); // Do not define!  Not copyable!

  DECLARE_CLASSNAME (Context);
  void terminate ();

private:
  friend class Context_handle;
  /* how many Context_handles point to this Context */
  int client_count_;

  /* Used internally by create_context */
  Stream_event *infant_event_;

protected:
  Context *daddy_context_;
  /* The used Context_def */
  SCM definition_;
  /* Additions to the Context_def, given by \with */
  SCM definition_mods_;

  SCM properties_scm_;
  SCM context_list_;
  SCM accepts_list_;
  SCM default_child_;
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

  void set_property_from_event (SCM);
  void unset_property_from_event (SCM);

public:
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
  Context ();

  /* properties:  */
  SCM internal_get_property (SCM name_sym) const;
  SCM properties_as_alist () const;
  Context *where_defined (SCM name_sym, SCM *value) const;
  bool here_defined (SCM name_sym, SCM *value) const;
  void unset_property (SCM var_sym);

  void instrumented_set_property (SCM, SCM, const char *, int, const char *);
  void internal_set_property (SCM var_sym, SCM value);

  Context *create_context (Context_def *, const string&, SCM);
  void create_context_from_event (SCM);
  void acknowledge_infant (SCM);
  void remove_context (SCM);
  void change_parent (SCM);
  void disconnect_from_parent ();
  void check_removal ();
  string context_name () const;
  SCM context_name_symbol () const;
  Global_context *get_global_context () const;

  virtual Context *get_score_context () const;
  virtual Output_def *get_output_def () const;
  virtual Moment now_mom () const;
  virtual Context *get_default_interpreter (const string &context_id = "");

  bool is_alias (SCM) const;
  void add_alias (SCM);
  void add_context (Context *trans);
  bool is_bottom_context () const;
  bool is_removable () const;

  Context *find_create_context (SCM context_name,
                                const string &id, SCM ops);
  Context *create_unique_context (SCM context_name, const string &context_id,
                                  SCM ops);
  vector<Context_def *> path_to_acceptable_context (SCM alias) const;
};

/*
  Context arg?
*/

void apply_property_operations (Context *tg, SCM pre_init_ops);
void execute_pushpop_property (Context *trg, SCM prop, SCM eltprop, SCM val);

// Search for a context of the given type starting from the given context and
// moving toward the root of the tree.  If the starting context matches, it is
// returned.
Context *find_context_above (Context *where, SCM type_sym);

// Search for a context of the given type starting from the given context and
// moving toward the root of the tree.  If found, return its child that was
// found on the way there.
Context *find_context_above_by_parent_type (Context *where, SCM parent_type);

// Search for a context of the given type and ID starting from the given
// context and moving toward the leaves of the tree.  If the starting context
// matches, it is returned.  An empty ID matches any context of the given type.
Context *find_context_below (Context *where,
                             SCM type_sym, const string &id);

// Search for a context of the given type and ID starting with the given
// context, then searching its descendants, then its parent's descendants, etc.
// An empty ID matches any context of the given type.
Context *find_context_near (Context *where,
                            SCM type_sym, const string &id);

// Search for the top context (i.e. the ancestor with no parent) starting with
// the given context.
Context *find_top_context (Context *where);

bool melisma_busy (Context *);

Context *get_voice_to_lyrics (Context *lyrics);
Grob *get_current_note_head (Context *voice);
Grob *get_current_rest (Context *voice);

Moment measure_position (Context const *context);
Moment measure_position (Context const *context, Duration const *dur);
Rational measure_length (Context const *context);
int measure_number (Context const *context);

bool check_repeat_count_visibility (Context const *context, SCM count);

void set_context_property_on_children (Context *trans, SCM sym, SCM val);

/* Shorthand for creating and broadcasting stream events. */
#define send_stream_event(ctx, type, origin, ...)                       \
  do {                                                                  \
    SCM props[] = { __VA_ARGS__, 0 };                                   \
    ctx->internal_send_stream_event (ly_symbol2scm (type), origin, props); \
  } while (0)

SCM nested_property_alist (SCM alist, SCM prop_path, SCM value);
SCM nested_property (SCM alist, SCM prop_path, SCM fallback = SCM_EOL);
SCM nested_create_alist (SCM prop_path, SCM value);
SCM partial_list_copy (SCM alist, SCM tail, SCM newtail);
SCM assq_tail (SCM key, SCM alist, SCM alist_end);
SCM assv_tail (SCM key, SCM alist, SCM alist_end);
SCM assoc_tail (SCM key, SCM alist, SCM alist_end);
SCM evict_from_alist (SCM, SCM, SCM);
SCM nalist_to_alist (SCM nalist, int nested);
extern SCM ly_context_set_property_x_proc;
extern SCM ly_context_unset_property_proc;
extern SCM ly_context_matched_pop_property_proc;

#endif /* CONTEXT_HH */

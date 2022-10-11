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

#ifndef CONTEXT_HH
#define CONTEXT_HH

#include "acceptance-set.hh"
#include "direction.hh"
#include "duration.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "scm-hash.hh"
#include "virtual-methods.hh"

#include <vector>

class Context_def;

class Context : public Smob<Context>
{
public:
  SCM mark_smob () const;
  int print_smob (SCM, scm_print_state *) const;
  static const char *const type_p_name_;
  virtual ~Context ();

protected:
  Context (Context_def *odef, SCM ops);

private:
  void add_global_finalization (SCM x);
  Context *create_hierarchy (const std::vector<Context_def *> &path,
                             const std::string &intermediate_id,
                             const std::string &leaf_id, SCM leaf_operations);
  Context *find_child_to_adopt_grandchild (SCM child_name, SCM grandchild_name);
  SCM make_revert_finalization (SCM sym);
  Scheme_hash_table *properties_dict () const;

  enum FindMode
  {
    FIND_CREATE,
    FIND_ONLY,
    CREATE_ONLY
  };

  Context *core_find (FindMode, Direction, SCM context_name,
                      const std::string &id, SCM ops);

  Context *find (FindMode, Direction, SCM context_name, const std::string &id,
                 SCM ops);

  Context *unchecked_find (FindMode, Direction, SCM context_name,
                           const std::string &id, SCM ops);

  Context (Context const &src) = delete;
  Context &operator= (Context const &) = delete;

  VIRTUAL_CLASS_NAME (Context);
  void terminate ();

private:
  friend class Context_handle;
  /* how many Context_handles point to this Context */
  int client_count_;

  /* Used internally by create_context */
  Stream_event *infant_event_;

protected:
  virtual void derived_mark () const;

private:
  Context *parent_;

protected:
  /* The used Context_def */
  SCM definition_;
  /* Additions to the Context_def, given by \with */
  SCM definition_mods_;

  SCM properties_scm_;
  SCM context_list_;
  Acceptance_set acceptance_;
  /* When a context needs to be added to the tree, but its descent is
     not fully specified, can this context accept it as a descendant?
     Currently set to true for children of the Global context, to avoid
     duplicate Score-like contexts. */
  bool adopts_ = false;
  SCM aliases_;
  Translator_group *implementation_;
  std::string id_string_;

  /* Events reported in the context is sent to this dispatcher. */
  Dispatcher *event_source_;

  /* Events reported to this context or recursively in any of its
     children, are sent to this dispatcher. */
  Dispatcher *events_below_;

  // Translator_group is allowed to set implementation_.
  friend class Translator_group;
  // UGH! initialises implementation_
  friend SCM ly_make_global_translator (SCM);

  void set_property_from_event (SCM);
  void unset_property_from_event (SCM);

public:
  // e.g. "mel" in "\context Voice = mel ..."
  std::string id_string () const { return id_string_; }

  // formatted identification for log messages
  static std::string diagnostic_id (SCM name, const std::string &id);

  SCM children_contexts () const { return context_list_; }

  Dispatcher *event_source () const { return event_source_; }
  Dispatcher *events_below () const { return events_below_; }
  void internal_send_stream_event (SCM type, Input *origin);
  void internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val);
  void internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                   SCM prop2, SCM val2);
  void internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                   SCM prop2, SCM val2, SCM prop3, SCM val3);
  void internal_send_stream_event (SCM type, Input *origin, SCM prop, SCM val,
                                   SCM prop2, SCM val2, SCM prop3, SCM val3,
                                   SCM prop4, SCM val4);
  SCM get_definition () const { return definition_; }
  SCM get_definition_mods () const { return definition_mods_; }

  Translator_group *implementation () const { return implementation_; }
  Context *get_parent () const { return parent_; }

  /* properties:  */
  SCM internal_get_property (SCM name_sym) const;
  SCM properties_as_alist () const;
  Context *internal_where_defined (SCM name_sym) const
  {
    SCM value;
    return internal_where_defined (name_sym, &value);
  }
  Context *internal_where_defined (SCM name_sym, SCM *value) const;
  bool internal_here_defined (SCM name_sym) const
  {
    SCM value;
    return internal_here_defined (name_sym, &value);
  }
  bool internal_here_defined (SCM name_sym, SCM *value) const;
  void unset_property (SCM var_sym);

  void instrumented_set_property (SCM, SCM, const char *, int, const char *);
  void internal_set_property (SCM var_sym, SCM value);

  Context *create_context (Context_def *, const std::string &, SCM);
  bool matches (SCM type, const std::string &id) const;
  virtual bool is_accessible_to_user () const { return true; }

  void create_context_from_event (SCM);
  void acknowledge_infant (SCM);
  void remove_context (SCM);
  void change_parent (SCM);
  void disconnect_from_parent ();
  void check_removal ();
  std::string context_name () const;
  SCM context_name_symbol () const;

  virtual Output_def *get_output_def () const;
  virtual Moment now_mom () const;

  Context *get_default_interpreter (const std::string &context_id = "");
  Context *get_user_accessible_interpreter ();

  bool is_alias (SCM) const;
  void add_alias (SCM);
  void add_context (Context *trans);
  bool is_bottom_context () const;
  bool is_removable () const;

  // This is like find_create_context () without the possibility of finding an
  // existing context.
  Context *create_unique_context (Direction dir, SCM name,
                                  const std::string &id, SCM ops);

  // This is like find_create_context () without the possibility of creating a
  // new context.
  Context *find_context (Direction dir, SCM name, const std::string &id);

  // Find the first context with the given name and ID, or create a new context
  // with the given name, ID, and context ops.
  //
  // The search proceeds as follows:
  //   1. this context
  //   2. an existing descendant of this context
  //   3. an ancestor of this context (nearest first)
  //   4. a new descendant of this context
  //   5. an existing or new descendant of an ancestor (nearest first)
  //
  // A direction of DOWN skips the ancestors and a direction of UP skips the
  // descendants.  This context is always considered.
  Context *find_create_context (Direction dir, SCM name, const std::string &id,
                                SCM ops);

  std::vector<Context_def *> path_to_acceptable_context (SCM alias) const;
};

/*
  Context arg?
*/

void apply_property_operations (Context *tg, SCM pre_init_ops);
void execute_pushpop_property (Context *trg, SCM prop, SCM eltprop, SCM val);

// abbreviate calling where->find_context when where might be null
inline Context *
find_context_above (Context *where, SCM type_sym, const std::string &id = "")
{
  return where ? where->find_context (UP, type_sym, id) : nullptr;
}

// abbreviate calling where->find_context when where might be null
inline Context *
find_context_below (Context *where, SCM type_sym, const std::string &id)
{
  return where ? where->find_context (DOWN, type_sym, id) : nullptr;
}

// abbreviate calling where->find_context when where might be null
inline Context *
find_context_near (Context *where, SCM type_sym, const std::string &id)
{
  return where ? where->find_context (CENTER, type_sym, id) : nullptr;
}

// Search for the top context (i.e. the ancestor with no parent) starting with
// the given context.
Context *find_top_context (Context *where);

bool melisma_busy (Context *);

Context *get_voice_to_lyrics (Context *lyrics);
Grob *get_current_note_head (Context *voice);
Grob *get_current_rest (Context *voice);

Moment measure_position (Context const *context);
Moment note_end_mom (Context const *context, Duration const *dur);
Rational measure_length (Context const *context);
int measure_number (Context const *context);

bool check_repeat_count_visibility (Context const *context, SCM count);

bool break_allowed (Context const *);

void set_context_property_on_children (Context *trans, SCM sym, SCM val);

/* Shorthand for creating and broadcasting stream events. */
#define send_stream_event(ctx, type, origin, ...)                              \
  ctx->internal_send_stream_event (ly_symbol2scm (type), origin, ##__VA_ARGS__)

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

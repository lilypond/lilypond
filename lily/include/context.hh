/*
  context.hh -- declare  Context

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CONTEXT_HH
#define CONTEXT_HH

#include <map>
using namespace std;

#include "moment.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"

class Context
{
  Scheme_hash_table *properties_dict () const;
  Context (Context const &src);

  DECLARE_SMOBS (Context, dummy);
  DECLARE_CLASSNAME(Context);
  void terminate ();

private:
  friend class Context_handle;
  int iterator_count_;
  bool init_;

  map<String, int> grob_counts_;
  map<String, int> context_counts_;

protected:
  Object_key const *key_;
  Context *daddy_context_;
  SCM definition_;

  SCM properties_scm_;
  SCM context_list_;
  SCM accepts_list_;
  SCM aliases_;
  Translator_group *implementation_;
  String id_string_;

  friend class Context_def;
  void clear_key_disambiguations ();

public:
  Object_key const *key () const { return key_; }
  Object_key const *get_grob_key (String);
  Object_key const *get_context_key (String, String);

  Context *create_context (Context_def *, String, SCM);
  String id_string () const { return id_string_; }
  SCM children_contexts () const { return context_list_; }
  SCM default_child_context_name () const;

  Translator_group *implementation () const { return implementation_; }
  Context *get_parent_context () const;
  Context (Object_key const *);

  /* properties:  */
  SCM internal_get_property (SCM name_sym) const;
  SCM properties_as_alist () const;
  void internal_set_property (SCM var_sym, SCM value);
  Context *where_defined (SCM name_sym, SCM *value) const;
  void unset_property (SCM var_sym);

  Context *remove_context (Context *trans);
  void check_removal ();
  String context_name () const;
  SCM context_name_symbol () const;
  Global_context *get_global_context () const;

  virtual Score_context *get_score_context () const;
  virtual Output_def *get_output_def () const;
  virtual Moment now_mom () const;
  virtual Context *get_default_interpreter ();

  bool is_alias (SCM) const;
  void add_alias (SCM);
  void add_context (Context *trans);
  bool is_bottom_context () const;
  bool is_removable () const;
  bool try_music (Music *);

  Context *find_create_context (SCM context_name,
				String id, SCM ops);
  Context *create_unique_context (SCM context_name,
				  SCM ops);
  Link_array<Context> path_to_acceptable_context (SCM alias,
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
			     SCM type_sym, String id);
bool melisma_busy (Context *);

Context *get_voice_to_lyrics (Context *lyrics);
Grob *get_current_note_head (Context *voice);
Grob *get_current_rest (Context *voice);
DECLARE_UNSMOB (Context, context);

Moment measure_position (Context const *context);
Rational measure_length (Context const *context);
void set_context_property_on_children (Context *trans, SCM sym, SCM val);

#endif /* CONTEXT_HH */


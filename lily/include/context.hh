/*   
     context.hh -- declare  Context

     source file of the GNU LilyPond music typesetter

     (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
 */

#ifndef CONTEXT_HH
#define CONTEXT_HH

#include "moment.hh"
#include "lily-proto.hh"


class Context
{
  Scheme_hash_table *properties_dict () const;
  Context (Context const &src);
  
  DECLARE_SMOBS (Context, dummy);

  void terminate ();

protected:
  Context * daddy_context_;
  SCM definition_;
  SCM properties_scm_;
  SCM context_list_;
  SCM accepts_list_;
  SCM aliases_;

  friend class Context_def;
public:
  SCM children_contexts () const { return context_list_; }
  SCM default_child_context_name () const;

  Context * get_parent_context () const;
  
  Context ();
  void execute_pushpop_property (SCM prop, SCM sym, SCM val);
  SCM internal_get_property (SCM name_sym) const;

  Context *remove_context (Context*trans);
  void check_removal ();

  SCM properties_as_alist () const;
  void unset_property (SCM var_sym);
  void internal_set_property (SCM var_sym, SCM value);  

  Context *where_defined (SCM name_sym) const;
  String context_name () const;
  Global_context * get_global_context () const;
  
  virtual Score_context * get_score_context () const;  
  bool is_alias (SCM) const;
  void add_alias (SCM); 
  void add_context (Context *trans);
  bool is_bottom_context () const;
  bool is_removable () const;
  bool try_music (Music *);
  
  virtual Music_output_def *get_output_def () const;
  virtual Moment now_mom () const;
  Context *find_create_context (SCM context_name,
				String id, SCM ops);
  Link_array<Context> path_to_acceptable_context (SCM alias,
						  Music_output_def*) const;
  virtual Context *get_default_interpreter ();
  String id_string_;

  SCM implementation_;
private:
  friend class Interpretation_context_handle;
  int iterator_count_;
  bool init_;
};

/*
  Context arg? 
 */

void apply_property_operations (Context*tg, SCM pre_init_ops);
void execute_pushpop_property (Context * trg, SCM prop, SCM eltprop, SCM val);
SCM updated_grob_properties (Context* tg, SCM sym);
Context * find_context_below (Context * where,
			      SCM type_sym, String id);
bool melisma_busy (Context*);

Context *get_voice_to_lyrics (Context *lyrics);
Grob *get_current_note_head (Context * voice);
Context *unsmob_context (SCM);

DECLARE_UNSMOB(Context,context);

#endif /* CONTEXT_HH */


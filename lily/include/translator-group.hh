/*
  translator-group.hh -- declare Translator_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TRANSLATOR_GROUP_HH
#define TRANSLATOR_GROUP_HH

#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "translator.hh"
#include "parray.hh"
#include "smobs.hh"

// egcs
typedef void (Translator::*Method_pointer) (void);
#define set_property(x,y) internal_set_property(ly_symbol2scm(x),y)

class Scheme_hash_table;


/** Make some kind of Elements from Requests. Elements are made by
  hierarchically grouped Translators
  */
class Translator_group : public virtual Translator {

  Scheme_hash_table *properties_dict () const;
  int iterator_count_;

  friend class Interpretation_context_handle;
protected:
  ~Translator_group ();
public:
  SCM add_translator (SCM, Translator*);
  void execute_single_pushpop_property (SCM prop, SCM sym, SCM val);
  SCM internal_get_property (SCM name_sym) const;

  void unset_property (SCM var_sym);
  void internal_set_property (SCM var_sym, SCM value);  
  Translator_group *where_defined (SCM name_sym) const;

  String id_str_;

  VIRTUAL_COPY_CONS (Translator);
  Translator_group (Translator_group const &);
  Translator_group ();
  void add_group_translator (Translator *trans_p);
  
  /// Score_register = 0, Staff_registers = 1, etc)
  Translator_group* ancestor_l (int l=1);
  int depth_i () const;
  bool is_bottom_translator_b () const;
  bool removable_b () const;
  void terminate_translator (Translator*r_l);
  Translator *remove_translator_p (Translator*trans_l);
  void check_removal ();
  // Translator *get_simple_translator (String) const;
  Translator_group *find_existing_translator_l (String n, String id);
  Translator_group *find_create_translator_l (String n, String id);
  Link_array<Translator_group> path_to_acceptable_translator (String alias, Music_output_def*) const;
  Translator_group*get_default_interpreter ();

public:
  bool try_music_on_nongroup_children (Music *m);
  
  virtual void do_announces ();
  virtual bool try_music (Music* req_l);       
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();   
  virtual void initialize ();
  virtual void finalize ();
  virtual void each (Method_pointer);
};

#endif // TRANSLATOR_GROUP_HH

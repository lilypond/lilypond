/*
  translator-group.hh -- declare Translator_group

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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


/** Make some kind of Elements from Events. Elements are made by
  hierarchically grouped Translators
  */
class Translator_group : public virtual Translator {
public:
  String id_string_;
private:
  int iterator_count_;
  friend class Interpretation_context_handle;

  Scheme_hash_table *properties_dict () const;
  SCM add_translator (SCM, Translator*);
protected:
  ~Translator_group ();

  virtual SCM get_simple_trans_list ();

public:
  void execute_pushpop_property (SCM prop, SCM sym, SCM val);
  SCM internal_get_property (SCM name_sym) const;
  SCM properties_as_alist () const;
  void unset_property (SCM var_sym);
  void internal_set_property (SCM var_sym, SCM value);  

  Translator_group *where_defined (SCM name_sym) const;
  String context_name () const;  
  Translator_group (Translator_group const &);
  Translator_group ();


  void add_fresh_group_translator (Translator *trans);
  void add_used_group_translator (Translator *trans);
  bool is_bottom_context () const;
  bool is_removable () const;
  void terminate_translator (Translator*r);
  Translator *remove_translator (Translator*trans);
  void check_removal ();
  Translator_group *find_existing_translator (SCM context_name, String id);
  Translator_group *find_create_translator (SCM context_name,
					    String id, SCM ops);
  Link_array<Translator_group>
    path_to_acceptable_translator (SCM alias,
				   Music_output_def*) const;
  Translator_group*get_default_interpreter ();
  VIRTUAL_COPY_CONS (Translator);
public:
  bool try_music_on_nongroup_children (Music *m);
  
  virtual void do_announces ();
  virtual bool try_music (Music* req);       
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();   
  virtual void initialize ();
  virtual void finalize ();
  virtual void each (Method_pointer);
};


bool melisma_busy (Translator* tr); // where to put this? --hwn
void apply_property_operations (Translator_group*tg, SCM pre_init_ops);
SCM names_to_translators (SCM namelist, Translator_group*tg);
void execute_pushpop_property (Translator_group * trg,
			       SCM prop, SCM eltprop, SCM val);
SCM updated_grob_properties (Translator_group* tg, SCM sym);

#endif // TRANSLATOR_GROUP_HH

/*   
  translator-def.hh -- declare Translator_def
  
  source file of the GNU LilyPond music typesetter
  
  (c)  2000--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TRANSLATOR_DEF_HH
#define TRANSLATOR_DEF_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "input.hh"

/*
  The definition of a interpretation context as given in the
  input. The lists are stored in order of definition.
*/
struct Translator_def : public Input
{
private:
  /*
    these lists store the definition, in opposite order of entry
  */
  
  SCM consists_name_list_;
  SCM end_consists_name_list_;
  SCM accepts_name_list_;
  SCM property_ops_;

public:
  SCM description_;
  /*
    "type" is overloaded.
   */
  SCM type_name_;
  SCM type_aliases_;
  SCM translator_group_type_;

  SCM modify_definition (SCM, SCM, bool);
  SCM default_child_context_name ();
  
  void set_acceptor (SCM accepts, bool add);
  void add_element (SCM name);
  void remove_element (SCM name);
  void add_last_element (SCM name);
  void add_property_operation (SCM);

  Link_array<Translator_def> path_to_acceptable_translator (SCM type_string, Music_output_def* odef) const;
  Translator_group * instantiate (Music_output_def*);

  SCM to_alist () const;

  static SCM make_scm () ;

  SCM clone_scm ()const;
  void apply_default_property_operations (Translator_group*);

private:
  DECLARE_SMOBS (Translator_def,foo);
  Translator_def ();
  Translator_def (Translator_def const&);


};

DECLARE_UNSMOB(Translator_def,translator_def);


#endif /* TRANSLATOR_DEF_HH */


/*   
  translator-def.hh -- declare Translator_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TRANSLATOR_DEF_HH
#define TRANSLATOR_DEF_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "input.hh"

struct Translator_def : public Input
{
  SCM consists_name_list_;
  SCM end_consists_name_list_;
  SCM accepts_name_list_;
  SCM property_ops_;
  SCM type_name_;
  SCM translator_group_type_;

  SCM modify_definition (SCM, SCM, bool);
  
  void set_acceptor (SCM accepts, bool add);
  void add_element (SCM name);
  void remove_element (SCM name);
  void add_last_element (SCM name);

  void add_push_property (SCM,SCM,SCM);
  void add_pop_property (SCM,SCM);
  void add_property_assign (SCM, SCM);
  Link_array<Translator_def> path_to_acceptable_translator (SCM type_str, Music_output_def* odef) const;
  Translator_group * instantiate (Music_output_def*);

  static SCM make_scm () ;
  static void apply_pushpop_property (Translator_group*, SCM syms, SCM eprop, SCM val);

  SCM clone_scm ()const;
  DECLARE_SMOBS(Translator_def,foo);
private:

  Translator_def ();
  Translator_def (Translator_def const&);


};

Translator_def* unsmob_translator_def (SCM);


#endif /* TRANSLATOR_DEF_HH */


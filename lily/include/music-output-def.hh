/*
  music-output-def.hh -- declare Music_output_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_output_DEF_HH
#define Music_output_DEF_HH

#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "dictionary.hh"
#include "scm-hash.hh"

/**
  Definition of how to output mudela. 
 */
class Music_output_def  
{
  Scope *translator_p_dict_p_;
public:
  Scope *scope_p_;
  
  Array<String> filename_str_arr_;
  Scheme_hash_table default_properties_;
  
  Music_output_def (Music_output_def const&);
  Music_output_def ();
  virtual ~Music_output_def ();

  VIRTUAL_COPY_CONS(Music_output_def);
  
  virtual void print () const;

  Global_translator *get_global_translator_p ();
  Translator_group *get_group_translator_p (String type) const;
  String get_default_output () const;
  void assign_translator (Translator_group*);
  Translator * find_translator_l (String) const;
  virtual int get_next_default_count () const;
};

#endif // Music_output_DEF_HH

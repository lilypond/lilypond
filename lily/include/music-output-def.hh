/*
  music-output-def.hh -- declare Music_output_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_output_DEF_HH
#define Music_output_DEF_HH

#include "string.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"
#include "virtual-methods.hh"
#include "smobs.hh"

/**
  Definition of how to output lilypond.
 */
class Music_output_def  
{
public:
  Scheme_hash_table * translator_tab_;

  SCM scope_;
  SCM scaled_fonts_;
  SCM style_sheet_;
  
  VIRTUAL_COPY_CONS (Music_output_def);
  Music_output_def (Music_output_def const&);
  Music_output_def ();
  virtual int get_next_score_count () const;

  Global_translator *get_global_translator ();
  Translator_group *get_group_translator (String type) const;
  void assign_translator (SCM transdef);
  SCM find_translator (SCM name) const;
  String outname_string () ;
  SCM get_scmvar (String id)const;
  SCM lookup_variable  (SCM sym) const;
  void set_variable  (SCM, SCM sym);
  
  DECLARE_SMOBS (Music_output_def,);

  
};

DECLARE_UNSMOB(Music_output_def,music_output_def);
#endif // Music_output_DEF_HH

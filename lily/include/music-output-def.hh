/*
  music-output-def.hh -- declare Music_output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MUSIC_OUTPUT_DEF_HH
#define MUSIC_OUTPUT_DEF_HH

#include "string.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"
#include "virtual-methods.hh"
#include "smobs.hh"
#include "input.hh"

/**
  Definition of how to output lilypond.
 */
class Music_output_def   
{
public:
  Scheme_hash_table * translator_tab_;
  Input input_origin_;
  Array<String> targets_;
  SCM scope_;

  virtual void derived_mark ();
  Music_output_def (Music_output_def const&);
  Music_output_def ();
  VIRTUAL_COPY_CONSTRUCTOR (Music_output_def, Music_output_def);

  Context *get_group_translator (String type) const;
  void assign_context_def (SCM transdef);
  SCM find_context_def (SCM name) const;
  String outname_string () ;
  SCM c_variable (String id) const;
  SCM lookup_variable (SCM sym) const;
  void set_variable (SCM, SCM sym);
  // fixme: dependencies
  //  virtual void add_target_file (String);
  
  DECLARE_SMOBS (Music_output_def,);
};

DECLARE_UNSMOB (Music_output_def, music_output_def);

#endif /* MUSIC_OUTPUT_DEF_HH */

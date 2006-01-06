/*
  recording-group-engraver.hh -- declare Recording_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef RECORDING_GROUP_ENGRAVER_HH
#define RECORDING_GROUP_ENGRAVER_HH

#include "engraver-group.hh"

class Recording_group_engraver : public Engraver_group
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator_group, Recording_group_engraver);
  virtual bool try_music (Music *m);
  void add_music (SCM, SCM);
  Recording_group_engraver ();
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void derived_mark () const;
  virtual void fetch_precomputable_methods (Translator_group_void_method ptrs[]);

  SCM now_events_;
  SCM accumulator_;
};
#endif /* RECORDING_GROUP_ENGRAVER_HH */

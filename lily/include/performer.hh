/*
  performer.hh -- declare Performer

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "audio-element-info.hh"
#include "array.hh"
#include "event.hh"
#include "grob-info.hh"
#include "translator.hh"

/**
  Convert a music definition into a audio representation.
  A baseclass
 */
class Performer : public virtual Translator
{
public:
  VIRTUAL_COPY_CONS (Translator);
  friend class Performer_group_performer;  
  Performer_group_performer* get_daddy_perf () const;

protected:
  virtual void announce_element (Audio_element_info);
  virtual void acknowledge_audio_element (Audio_element_info);
  virtual void create_audio_elements ();
  virtual int get_tempo () const;
  virtual void play_element (Audio_element * elem );
};


#endif // PERFORMER_HH


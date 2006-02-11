/*
  performer-group.hh -- declare Performer_group

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PERFORMER_GROUP_HH
#define PERFORMER_GROUP_HH

#include "performer.hh"
#include "translator-group.hh"

typedef void (Performer:: *Performer_method) (void);

class Performer_group : public virtual Translator_group
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator_group, Performer_group);

  void do_announces ();
  virtual void announce_element (Audio_element_info);
  virtual void play_element (Audio_element *p);
  virtual int get_tempo () const;

protected:
  vector<Audio_element_info> announce_infos_;

private:
  void acknowledge_audio_elements ();
};

void performer_each (SCM list, Performer_method method);

#endif /* PERFORMER_GROUP_HH */

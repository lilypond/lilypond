/*
  performer-group.hh -- declare Performer_group

  (c) 1996--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

protected:
  vector<Audio_element_info> announce_infos_;
  virtual void acknowledge_audio_elements ();
};

void performer_each (SCM list, Performer_method method);

#endif /* PERFORMER_GROUP_HH */

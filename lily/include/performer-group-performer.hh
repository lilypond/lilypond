/*
  performer-group-performer.hh -- declare Performer_group_performer

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PERFORMER_GROUP_PERFORMER_HH
#define PERFORMER_GROUP_PERFORMER_HH

#include "performer.hh"
#include "translator-group.hh"

typedef void (Performer::*Performer_method) (void);

class Performer_group_performer : public Performer, public virtual Translator_group
{
public:
  TRANSLATOR_DECLARATIONS (Performer_group_performer);

  virtual void do_announces ();
  virtual void announce_element (Audio_element_info);
protected:
  Array<Audio_element_info> announce_infos_;

private:
  void acknowledge_audio_elements ();
};

void performer_each (SCM list, Performer_method method);

#endif // PERFORMER_GROUP_PERFORMER_HH


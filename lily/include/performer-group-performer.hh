/*
  performer-group-performer.hh -- declare Performer_group_performer

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PERFORMER_GROUP_PERFORMER_HH
#define PERFORMER_GROUP_PERFORMER_HH

#include "performer.hh"
#include "translator-group.hh"

typedef void (Performer:: *Performer_method) (void);

class Performer_group_performer : public virtual Translator_group
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator_group, Performer_group_performer);

  void do_announces ();
  virtual void announce_element (Audio_element_info);
  virtual void play_element (Audio_element *p);
  virtual int get_tempo () const;

protected:
  Array<Audio_element_info> announce_infos_;

private:
  void acknowledge_audio_elements ();
};

void performer_each (SCM list, Performer_method method);

#endif // PERFORMER_GROUP_PERFORMER_HH


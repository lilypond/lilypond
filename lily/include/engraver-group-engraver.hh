
/*
  engraver-group-engraver.hh -- declare Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ENGRAVERGROUP_HH
#define ENGRAVERGROUP_HH

#include "lily-proto.hh"
#include "parray.hh"
#include "grob-info.hh"
#include "engraver.hh"
#include "translator-group.hh"


class Engraver_group_engraver : public virtual Engraver,
				public virtual Translator_group
{
protected:
  Array<Grob_info> announce_infos_;
  
public:
  TRANSLATOR_DECLARATIONS(Engraver_group_engraver);

  virtual void initialize ();
  virtual void do_announces ();
  virtual void announce_grob (Grob_info);

  int pending_grob_count () const;
private:
  virtual void acknowledge_grobs ();
};

typedef void (Engraver::*Engraver_method) (void);

void engraver_each (SCM list, Engraver_method method);

#endif // ENGRAVERGROUP_HH



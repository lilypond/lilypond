
/*
  engraver-group-engraver.hh -- declare Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ENGRAVERGROUP_HH
#define ENGRAVERGROUP_HH

#include "lily-proto.hh"
#include "parray.hh"
#include "grob-info.hh"
#include "engraver.hh"
#include "translator-group.hh"


/**
  Group a number of engravers. Usually delegates everything to its contents.
  Postfix: group
  */
class Engraver_group_engraver : public Engraver,
				public virtual Translator_group
{
protected:
  Array<Grob_info> announce_infos_;

  
  
public:
  TRANSLATOR_DECLARATIONS(Engraver_group_engraver);

  virtual void initialize ();
  virtual void do_announces ();
  virtual void announce_grob (Grob_info);
  virtual void process_music ();
private:
  virtual void acknowledge_grobs ();
  virtual void process_acknowledged_grobs_in_simple_children ();
};

#endif // ENGRAVERGROUP_HH



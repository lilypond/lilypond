
/*
  engraver-group-engraver.hh -- declare Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Array<Grob_info> announce_info_arr_;

  
  
public:
  VIRTUAL_COPY_CONS (Translator);

  virtual void do_announces ();
  virtual void announce_grob (Grob_info);
  virtual void process_music ();
private:
  void create_grobs ();
  void acknowledge_grobs ();
};

#endif // ENGRAVERGROUP_HH



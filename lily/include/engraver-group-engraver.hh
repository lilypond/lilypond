
/*
  engraver-group-engraver.hh -- declare Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef ENGRAVERGROUP_HH
#define ENGRAVERGROUP_HH

#include "lily-proto.hh"
#include "parray.hh"
#include "score-element-info.hh"
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
  Array<Score_element_info> announce_info_arr_;
public:
  VIRTUAL_COPY_CONS(Translator);
    
  virtual Staff_info get_staff_info() const;
  virtual void do_announces();
  virtual void announce_element (Score_element_info);
};

#endif // ENGRAVERGROUP_HH



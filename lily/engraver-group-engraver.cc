/*
  engravergroup.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "flower-proto.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "debug.hh"
#include "paper-score.hh"
#include "grob.hh"


ADD_THIS_TRANSLATOR(Engraver_group_engraver);

void
Engraver_group_engraver::announce_grob (Grob_info info)
{
  announce_info_arr_.push (info);
  Engraver::announce_grob (info);
}


void
Engraver_group_engraver::create_grobs ()
{

  for (SCM p = simple_trans_list_; gh_pair_p (p); p = gh_cdr ( p))
    {
      Translator * t = unsmob_translator (gh_car (p));
      Engraver * eng = dynamic_cast<Engraver*> (t);
      if (eng)
	eng->create_grobs ();
    }
}

void
Engraver_group_engraver::acknowledge_grobs ()
{
  for (int j =0; j < announce_info_arr_.size(); j++)
    {
      Grob_info info = announce_info_arr_[j];
      for (SCM p = simple_trans_list_; gh_pair_p (p); p = gh_cdr (p))
	{
	  Translator * t = unsmob_translator (gh_car (p));
	  Engraver * eng = dynamic_cast<Engraver*> (t);
	  if (eng && eng!= info.origin_trans_l_)
	    eng->acknowledge_grob (info);
	}
    }
}

void
Engraver_group_engraver::do_announces()
{
  for (SCM p = trans_group_list_; gh_pair_p (p); p =gh_cdr ( p))
    {
      Translator * t = unsmob_translator (gh_car (p));
      dynamic_cast<Engraver_group_engraver*> (t)->do_announces ();
    }

  create_grobs ();
    
  while (announce_info_arr_.size ())
    {
      acknowledge_grobs ();
      announce_info_arr_.clear ();
      create_grobs ();
    }
}

#include <iostream.h>

/*
  order is : top to bottom (as opposed to do_announces)
 */
void
Engraver_group_engraver::process_music ()
{
   for (SCM p = simple_trans_list_; gh_pair_p (p); p =gh_cdr ( p))
    {
      Translator * t = unsmob_translator (gh_car (p));
      Engraver * eng = dynamic_cast<Engraver*> (t);

      if (eng)
	eng->process_music ();
    }
   for (SCM p = trans_group_list_; gh_pair_p (p); p =gh_cdr ( p))
    {
      Translator * t = unsmob_translator (gh_car (p));
      Engraver*eng = dynamic_cast<Engraver*> (t);
      if (eng)
	eng->process_music ();
    }
}





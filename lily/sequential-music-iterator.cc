/*
  Sequential_music_iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "warn.hh"
#include "sequential-music-iterator.hh"
#include "music-list.hh"

/*
  move to context of child iterator if it is deeper down in the
  hierarchy.
  */
void
Sequential_music_iterator::descend_to_child ()
{
  Translator_group  * child_report = child_report = iter_->report_to ();
  Translator_group * me_report = report_to ();

  Translator_group * c = child_report;
  while (c && c != me_report)
    {
      c= c->daddy_trans_;
    }
  
  if (c == me_report)
    set_translator (child_report);
}


IMPLEMENT_CTOR_CALLBACK (Sequential_music_iterator);

SCM
Sequential_music_iterator::get_music_list()const
{
  return dynamic_cast<Music_sequence const*> (get_music ())->music_list ();
}

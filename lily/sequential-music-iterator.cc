/*
  Sequential_music_iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "warn.hh"
#include "sequential-music-iterator.hh"
#include "music-list.hh"




IMPLEMENT_CTOR_CALLBACK (Sequential_music_iterator);

SCM
Sequential_music_iterator::get_music_list()const
{
  return dynamic_cast<Music_sequence const*> (get_music ())->music_list ();
}

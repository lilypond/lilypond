/*
  Sequential_music_iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "sequential-music-iterator.hh"

#include "context.hh"
#include "warn.hh"
#include "music-list.hh"

IMPLEMENT_CTOR_CALLBACK (Sequential_music_iterator);

SCM
Sequential_music_iterator::get_music_list ()const
{
  return get_music()->get_property ("elements");
}

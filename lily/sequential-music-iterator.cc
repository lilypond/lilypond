/*
  sequential-music-iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "sequential-music-iterator.hh"
#include "music.hh"
#include "warn.hh"

IMPLEMENT_CTOR_CALLBACK (Sequential_music_iterator);

SCM
Sequential_music_iterator::get_music_list ()const
{
  return get_music()->get_property ("elements");
}

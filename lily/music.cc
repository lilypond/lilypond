/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "music.hh"
#include "music-list.hh"
#include "debug.hh"

MInterval
Music::time_int() const
{
    return MInterval(0,0);
}
void
Music::print()const
{
#ifndef NPRINT
    mtor << name() << "{" ;
    do_print();
    mtor << "}\n";
#endif
}

void
Music::transpose(Melodic_req const*)
{
    
}

void
Music::translate(Moment )
{
}

void
Music::do_print()const
{
}


IMPLEMENT_IS_TYPE_B(Music);



    
Music::Music()
{
    parent_music_l_ =0;
}

IMPLEMENT_IS_TYPE_B1(Change_reg,Music)


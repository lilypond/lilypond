/*
  input-staff.cc -- implement Input_staff

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "score.hh"
#include "input-music.hh"
#include "input-staff.hh"
#include "staff.hh"
#include "complex-staff.hh"
#include "lyric-staff.hh"

#include "my-lily-lexer.hh"


Input_staff::Input_staff(String s)
{
    type= s;
    defined_ch_C_ = 0;
}

void
Input_staff::add(Input_music*m)
{
    music_.bottom().add(m);
}

Staff*
Input_staff::parse(Score*score_l)
{
    Staff *p=0;
    if (type == "melodic")
	p = new Complex_staff;
    else if (type == "lyric")
    	p = new Lyric_staff;
    else {
 	error( "Unknown staff-type `" + type +"\'", defined_ch_C_ );
	exit( 1 );
    }

    p->score_l_ = score_l;
    
    for (iter_top(music_,i); i.ok(); i++) {
	Voice_list vl = i->convert();
	p->add(vl);
    }
    return p;
}

Input_staff::Input_staff(Input_staff const&s)
{    
    for (iter_top(s.music_,i); i.ok(); i++)
	add(i->clone());
    defined_ch_C_ = s.defined_ch_C_;
    type = s.type;
}

void
Input_staff::print() const
{
#ifndef NPRINT
    mtor << "Input_staff {\n";
    for (iter_top(music_,i); i.ok(); i++)
	i->print();
    mtor << "}\n";
#endif
}
Input_staff::~Input_staff()
{
}

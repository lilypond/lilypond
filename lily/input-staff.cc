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
#include "my-lily-lexer.hh"
#include "input-register.hh"

Input_staff::Input_staff()
{
    ireg_p_ =0;
}

void
Input_staff::add(Input_music*m)
{
    music_.bottom().add(m);
}

Staff*
Input_staff::parse(Score*score_l)
{
    Staff *p=new Complex_staff;
   
    p->score_l_ = score_l;
    p->ireg_p_ = (ireg_p_)? new Input_register(*ireg_p_):0;
    for (iter_top(music_,i); i.ok(); i++) {
	Voice_list vl = i->convert();
	p->add(vl);
    }
    return p;
}

Input_staff::Input_staff(Input_staff const&s)
    : Input(s)
{    
    for (iter_top(s.music_,i); i.ok(); i++)
	add(i->clone());

    ireg_p_ = (s.ireg_p_)? new Input_register(*s.ireg_p_):0;
}

void
Input_staff::print() const
{
#ifndef NPRINT
    mtor << "Input_staff {\n";
    for (iter_top(music_,i); i.ok(); i++)
	i->print();
    ireg_p_->print();
    mtor << "}\n";
#endif
}
Input_staff::~Input_staff()
{
    delete ireg_p_;
}

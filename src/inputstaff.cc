/*
  inputstaff.cc -- implement Input_staff

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "score.hh"
#include "inputmusic.hh"
#include "inputstaff.hh"
#include "staff.hh"
#include "complexstaff.hh"
#include "lyricstaff.hh"
#include "midistaff.hh"
#include "lexer.hh"


Input_staff::Input_staff(String s)
{
    score_wide_music_p_ =0;
    type= s;
    defined_ch_c_l_ = 0;
}

void
Input_staff::add(Input_music*m)
{
    music_.bottom().add(m);
}

Staff*
Input_staff::parse(Score*score_l, Input_music *default_score_wide)
{
    Staff *p=0;
    if (type == "melodic")
	p = new Complex_staff;
    else if (type == "lyric")
    	p = new Lyric_staff;
    else if (type == "midi")
    	p = new Midi_staff;
    else {
 	error( "Unknown staff-type `" + type +"\'", defined_ch_c_l_ );
	exit( 1 );
    }

    p->score_l_ = score_l;
    
    for (iter_top(music_,i); i.ok(); i++) {
	Voice_list vl = i->convert();
	p->add(vl);
    }
    Voice_list vl =  (score_wide_music_p_) ? score_wide_music_p_->convert()
	: default_score_wide->convert();
    p->add(vl);
    return p;
}

Input_staff::Input_staff(Input_staff const&s)
{    
    for (iter_top(s.music_,i); i.ok(); i++)
	add(i->clone());
    defined_ch_c_l_ = s.defined_ch_c_l_;
    type = s.type;
    score_wide_music_p_ = (s.score_wide_music_p_) ?
	s.score_wide_music_p_->clone() : 0;
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
void
Input_staff::set_score_wide(Input_music *m_p)
{
    delete score_wide_music_p_;
    score_wide_music_p_ = m_p;
}

Input_staff::~Input_staff()
{
    delete score_wide_music_p_;
}

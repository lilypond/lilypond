#include "getcommand.hh"
#include "debug.hh"
#include "score.hh"
#include "inputmusic.hh"
#include "inputstaff.hh"
#include "inputcommand.hh"
#include "staffcommands.hh"
#include "staff.hh"
#include "complexstaff.hh"
#include "lexer.hh"
#include "lyricstaff.hh"

void
Input_staff::add(Array<Input_command*> &s)
{
    commands_.bottom().add(get_reset_command());
    for (int i=0; i < s.size(); i++)
	commands_.bottom().add(s[i]);
    s.set_size(0);
}

Input_staff::Input_staff(String s)
{
    type= s;
    defined_ch_c_l_ = 0;
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
#if 0
    if (type == "simple")
	p = new Melodic_staff;
    else if (type == "rhythmic")
	p = new Rhythmic_staff;
    else
#endif
	
	if (type == "melodic")
	p = new Complex_staff;
    else if (type == "lyric")
    	p = new Lyric_staff;
    else
	error( "Unknown staff-type `" + type +"\'", 0 );

    p->score_l_ = score_l;
    p->define_spot_str_ = "";	// todo
    
    for (iter_top(music_,i); i.ok(); i++) {
	Voice_list vl = i->convert();
	p->add(vl);
    }
    
    {
	Array<String> mark_arr;
	Array<Moment> moment_arr;
	p->get_marks(mark_arr, moment_arr);
	score_l->add_marks(mark_arr, moment_arr);
    }
    
    return p;
}

Input_staff::Input_staff(Input_staff const&s)
{
    
    for (iter_top(s.commands_,i); i.ok(); i++)
	commands_.bottom().add(new Input_command(**i));
    for (iter_top(s.music_,i); i.ok(); i++)
	add(i->clone());
    defined_ch_c_l_ = s.defined_ch_c_l_;
    type = s.type;
}

void
Input_staff::print() const
{
#ifndef NPRINT
    mtor << "Input_staff {\n";
    for (iter_top(commands_,i); i.ok(); i++)
	i->print();
    for (iter_top(music_,i); i.ok(); i++)
	i->print();
    mtor << "}\n";
#endif
}

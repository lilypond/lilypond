#include "getcommand.hh"
#include "debug.hh"
#include "inputmusic.hh"
#include "inputstaff.hh"
#include "inputcommands.hh"
#include "inputcommand.hh"
#include "staffcommands.hh"
#include "melodicstaff.hh"
#include "rhythmstaff.hh"
#include "staff.hh"

void
Input_staff::add(svec<Input_command*> &s)
{
    commands_.bottom().add(get_reset_command());
    for (int i=0; i < s.sz(); i++)
	commands_.bottom().add(s[i]);
    s.set_size(0);
}

Input_staff::Input_staff(String s)
{
    type= s;
}

void
Input_staff::add(Input_music*m)
{
    music_.bottom().add(m);
}

Staff*
Input_staff::parse(PointerList<Input_command*> score_wide)
{
    Staff *p=0;
    
    if (type == "melodic")
	p = new Melodic_staff;
    else if (type == "rhythmic")
	p = new Rhythmic_staff;

    for (iter_top(music_,i); i.ok(); i++) {
	Voice_list vl = i->convert();
	p->add(vl);
    }

    Input_commands commands;
    for (iter_top(score_wide,i); i.ok(); i++) 
	commands.add(**i);
    for (iter_top(commands_,i); i.ok(); i++) 
	commands.add(**i);

    p->staff_commands_ = commands.parse();

    return p;
}

Input_staff::Input_staff(Input_staff&s)
{
    for (iter_top(s.commands_,i); i.ok(); i++)
	commands_.bottom().add(new Input_command(**i));
    for (iter_top(s.music_,i); i.ok(); i++)
	add(i);

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

#include "debug.hh"
#include "inputcommand.hh"
#include "inputscore.hh"
#include "inputstaff.hh"
#include "score.hh"
#include "paper.hh"

void
Input_score::add(svec<Input_command*> &s)
{
    commands_.bottom().add(get_reset_command());
    for (int i=0; i < s.sz(); i++)
	commands_.bottom().add(s[i]);
}

void
Input_score::add(Input_staff*s)
{
    staffs_.bottom().add(s);
}

void
Input_score::set(Paperdef*p)
{
    delete paper_;
    paper_ = p;
}

Score*
Input_score::parse()
{
    Paperdef* p=new Paperdef(*paper_);
    Score *s = new Score(p);
    
    for (PCursor<Input_staff*> i(staffs_); i.ok(); i++) {
	Staff* staf=i->parse(commands_);
	s->add(staf);
    }
    return s;
}

Input_score::~Input_score()
{
    // should fix paper/symtabs to allow this deletion.
//    delete paper_;
}

Input_score::Input_score()
{
    paper_=new Paperdef;
}

void
Input_score::print()const
{
    mtor << "Input_score {\n";
    for (PCursor<Input_staff*> i(staffs_); i.ok(); i++) {
	i->print();
    }
    mtor << "}\n";
}

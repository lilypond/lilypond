#include "debug.hh"
#include "inputcommand.hh"
#include "inputscore.hh"
#include "inputstaff.hh"
#include "score.hh"
#include "paper.hh"
#include "staff.hh"

void
Input_score::add(Array<Input_command*> &s)
{
    commands_.bottom().add(get_reset_command());
    for (int i=0; i < s.size(); i++)
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
Input_score::Input_score(Input_score const&)
{
    assert(false);
}

Score*
Input_score::parse()
{
    Paperdef* paper_p=new Paperdef(*paper_);
    Score *s_p = new Score(paper_p);
    s_p->define_spot_str_ = define_spot_str_;
    Array<Staff*> parsed_staffs;
    for (iter_top(staffs_,i); i.ok(); i++) {
	Staff* staf_p=i->parse(s_p);
	parsed_staffs.push(staf_p);
	s_p->add(staf_p);
    }
    int j = 0;
    for (iter_top(staffs_,i); i.ok(); i++,j++) {
	parsed_staffs[j]->do_commands(commands_, i->commands_);
    }
    return s_p;
}

Input_score::~Input_score()
{
    // TODO!
//should fix paper/symtabs to allow this deletion.
//    delete paper_;
}

Input_score::Input_score()
{
    paper_= 0;
}

void
Input_score::print()const
{
    #ifndef NPRINT
    mtor << "Input_score {\n";
    for (iter_top(staffs_,i); i.ok(); i++) {
	i->print();
    }
    mtor << "}\n";
#endif
}

#include "request.hh"
#include "voice.hh"
#include "pscore.hh"
#include "lyricstaff.hh"
#include "lyricwalker.hh"
//#include "sccol.hh"
#include "debug.hh"
#include "lyricitem.hh"

void
Lyric_walker::do_word(Word_info)
{
}

void
Lyric_walker::do_INTERPRET_command(Command* )
{
}

void
Lyric_walker::do_TYPESET_command(Command* )
{
}

void
Lyric_walker::process_requests()
{
    Lyric_column* lcol_l = Lyric_walker::lcol_l();

    for (int i = 0; i <  lcol_l->winfo_array_.size(); i++)  {
	lcol_l->typeset_item(new Lyric_item(lcol_l->winfo_array_[i].lreq_l_, i));
    }
    
}

Lyric_walker::Lyric_walker(Lyric_staff* lstaff_l)
    : Staff_walker(lstaff_l, lstaff_l->pstaff_l_->pscore_l_)
{
    reset();
}



Lyric_staff*
Lyric_walker::lstaff_l()
{
    return (Lyric_staff*)staff_l_;
}

Lyric_column*
Lyric_walker::lcol_l()
{
    return (Lyric_column*) *(*this);
}

void
Lyric_walker::reset()
{
}


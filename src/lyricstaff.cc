#include "request.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "command.hh"
#include "lyricstaff.hh"
#include "lyriccolumn.hh"
#include "sccol.hh" 
#include "lyricwalker.hh"
#include "pscore.hh"

Lyric_staff::Lyric_staff()
{
    pstaff_l_=0;
}

Staff_column*
Lyric_staff::create_col(Score_column*s)
{
    return new Lyric_column(s,this);
}

void
Lyric_staff::set_output(PScore*pscore_l)
{
    pstaff_l_ = new PStaff(pscore_l);
    pscore_l_ = pscore_l;
    pscore_l_->add(pstaff_l_);
}

void
Lyric_staff::walk()
{
    for (Lyric_walker lcols(this); lcols.ok(); lcols++) {
	lcols.lcol_l()->setup_requests();
	lcols.process();
    }
}

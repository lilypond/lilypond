#include "request.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "command.hh"
#include "lyricstaff.hh"
#include "lyriccolumn.hh"
#include "linepstaff.hh"
#include "sccol.hh" 
#include "lyricwalker.hh"
#include "pscore.hh"



Lyric_staff::Lyric_staff()
{
    line_pstaff_p_ = 0;
}

Staff_column*
Lyric_staff::create_col(Score_column*s)
{
    return new Lyric_column(s,this);
}

void
Lyric_staff::set_output(PScore*ps)
{
    line_pstaff_p_ = new Linestaff(0,ps);
    pscore_l_ = ps;
    pscore_l_->add(line_pstaff_p_);
}

void
Lyric_staff::walk()
{
    for (Lyric_walker lcols(this); lcols.ok(); lcols++) {
	lcols.lcol_l()->setup_requests();// TODO
	lcols.process();
    }
}

#include "musicalrequest.hh"
#include "voice.hh"
#include "staff-walker.hh"
#include "debug.hh"
#include "staff.hh"
#include "lyricstaff.hh"
#include "lyricwalker.hh"
#include "pscore.hh"

void
Lyric_staff::set_output(PScore*pscore_l)
{
    pstaff_l_ = new PStaff(pscore_l);
    pscore_l_ = pscore_l;
    pscore_l_->add(pstaff_l_);
}

Staff_walker*
Lyric_staff::get_walker_p()
{
    return new Lyric_walker(this);
}

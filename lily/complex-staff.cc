#include "complex-staff.hh"
#include "complex-walker.hh"
#include "p-score.hh"

/** Aside from putting fields right
 */
void
Complex_staff::set_output(PScore* pscore_l )
{
    pstaff_l_ = new PStaff(pscore_l);
    pscore_l_ = pscore_l;
    pscore_l_->add(pstaff_l_);
}


Staff_walker * 
Complex_staff::get_walker_p()
{
    return new Complex_walker(this);
}

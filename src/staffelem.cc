#include "pscore.hh"
#include "symbol.hh"
#include "pstaff.hh"
#include "molecule.hh"
#include "staffelem.hh"

String
Staff_elem::TeXstring() const
{
    assert(!calc_children);
    Molecule m(*output);
    m.translate(offset_);	// ugh?
    return m.TeXstring();
}

Staff_elem::Staff_elem(Staff_elem const&s)
    :    dependencies(s.dependencies)
{
    status = s.status;
    assert(!s.output);
    output = 0;
    pstaff_l_ = s.pstaff_l_;
    calc_children = false;
    offset_ = Offset(0,0);
}

Staff_elem::~Staff_elem()
{
   delete output;
}
void
Staff_elem::translate(Offset O)
{
    offset_ += O;
}
Interval
Staff_elem::width() const
{
    Interval r;

    if (!output){
	Molecule*m	= brew_molecule_p();
	r = m->extent().x;
	delete m;
    } else
	r = output->extent().x;
  
    if (!r.empty()) // float exception on DEC Alpha
	r+=offset_.x;

    return r;
}
Interval
Staff_elem::height() const
{
    Interval r;

    if (!output){
	Molecule*m	= brew_molecule_p();
	r = m->extent().y;
	delete m;
    } else
	r = output->extent().y;
    
    if (!r.empty())
	r+=offset_.y;

  
    return r;
}

void
Staff_elem::print()const
{
#ifndef NPRINT
    if (output)
	output->print();
#endif
}

Staff_elem::Staff_elem()
{
    calc_children = false;
    pstaff_l_=0;
    offset_ = Offset(0,0);
    output = 0;
    status = ORPHAN;
}


Paperdef*
Staff_elem::paper()  const
{
    assert(pstaff_l_);
    return pstaff_l_->pscore_l_->paper_l_;
}

void
Staff_elem::add_processing()
{
    if (status >= VIRGIN)
	return;

    do_add_processing();
    status = VIRGIN;
}

void
Staff_elem::pre_processing()
{
    if (status >= PRECALCED )
	return;
    for (int i=0; i < dependencies.size(); i++)
	if (dependencies[i])
	    dependencies[i]->pre_processing();
    if (!calc_children)
	do_pre_processing();
    status = PRECALCED;
}
void
Staff_elem::post_processing()
{
    if (status > POSTCALCED)
	return;
    for (int i=0; i < dependencies.size(); i++)
	if (dependencies[i])
	    dependencies[i]->post_processing();
    if (!calc_children)
	do_post_processing();
    status=POSTCALCED;
}

void 
Staff_elem::molecule_processing()
{
    if (status >= OUTPUT)
	return;
    for (int i=0; i < dependencies.size(); i++)
	if (dependencies[i])
	    dependencies[i]->molecule_processing();
    if (!calc_children)
	output= brew_molecule_p();
    status = OUTPUT;    
}

void
Staff_elem::do_post_processing()
{
}

void
Staff_elem::do_pre_processing()
{
}

void
Staff_elem::do_add_processing()
{
}

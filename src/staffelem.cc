#include "pscore.hh"
#include "symbol.hh"
#include "pstaff.hh"
#include "molecule.hh"
#include "staffelem.hh"

String
Staff_elem::TeXstring() const
{
    assert(output && !calc_children);
    Molecule m(*output);
    m.translate(offset_);	// ugh?
    return m.TeXstring();
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
    Molecule*m= brew_molecule();
    Interval r = m->extent().x;

    if (!r.empty())		// float exception on DEC Alpha
	 r+=offset_.x;

    delete m;
    return r;
}
Interval
Staff_elem::height() const
{
    Molecule*m= brew_molecule();
    Interval r = m->extent().y;

 
    if (!r.empty())
	r+=offset_.y;
     delete m;
  
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
    pstaff_=0;
    offset_ = Offset(0,0);
    output = 0;
    status = ORPHAN;
}


Paperdef*
Staff_elem::paper()  const
{
    assert(pstaff_);
    return pstaff_->pscore_->paper_;
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
    for (int i=0; i < dependencies.sz(); i++)
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
    for (int i=0; i < dependencies.sz(); i++)
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
    for (int i=0; i < dependencies.sz(); i++)
	if (dependencies[i])
	    dependencies[i]->molecule_processing();
    if (!calc_children)
	output= brew_molecule();
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

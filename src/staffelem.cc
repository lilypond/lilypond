#include "pscore.hh"
#include "symbol.hh"
#include "pstaff.hh"
#include "molecule.hh"
#include "staffelem.hh"
#include "debug.hh"

String
Staff_elem::TeXstring() const
{
    Molecule m(*output);
    m.translate(offset_);	// ugh?
    return m.TeXstring();
}

Staff_elem::Staff_elem(Staff_elem const&s)
    : dependants(s.dependants),
      dependencies(s.dependencies)
{
    status = s.status;
    assert(!s.output);
    output = 0;
    pstaff_l_ = s.pstaff_l_;
    offset_ = Offset(0,0);
}
/**
  TODO:
  If deleted, then remove dependants depency!
  */
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
    mtor << name() << "{\n";
    do_print();
    if (output)
	output->print();
    
    mtor <<  "}\n";
#endif
}

NAME_METHOD(Staff_elem);

Staff_elem::Staff_elem()
{
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
    status = VIRGIN;
    do_add_processing();
}

void
Staff_elem::pre_processing()
{
    if (status >= PRECALCED )
	return;
    assert(status != PRECALCING); // cyclic dependency
    status = PRECALCING;

    for (int i=0; i < dependencies.size(); i++)
	if (dependencies[i])
	    dependencies[i]->pre_processing();

    
    do_pre_processing();
    status = PRECALCED;
}
void
Staff_elem::post_processing()
{
    if (status >= POSTCALCED)
	return;
    assert(status != POSTCALCING);// cyclic dependency
    status=POSTCALCING;	

    for (int i=0; i < dependencies.size(); i++)
	if (dependencies[i])
	    dependencies[i]->post_processing();
    do_post_processing();
    status=POSTCALCED;
}

void 
Staff_elem::molecule_processing()
{
    if (status >= OUTPUT)
	return;
    status = OUTPUT;		// do it only once.
    for (int i=0; i < dependencies.size(); i++)
	if (dependencies[i])
	    dependencies[i]->molecule_processing();

    output= brew_molecule_p();
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

void
Staff_elem::substitute_dependency(Staff_elem * old, Staff_elem * newdep)
{
    bool hebbes_b=false;
    for (int i=0; i < dependencies.size(); i++) {
	if (dependencies[i] == old){
	    dependencies[i] = newdep;
	    hebbes_b = true;
	} else if (dependencies[i] == newdep) {
	    hebbes_b = true;
	}
    }
    if (!hebbes_b)
	dependencies.push(newdep);
}

void
Staff_elem::add_depedency(Staff_elem * p)
{
    for (int i=0; i < dependencies.size(); i ++)
	if (dependencies[i] == p)
	    return;
    
    dependencies.push(p);
    p->dependants.push(p);
}

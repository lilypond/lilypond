/*
  staff-elem.cc -- implement Score_elem

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "paper-def.hh"
#include "lookup.hh"
#include "p-score.hh"
#include "symbol.hh"
#include "p-staff.hh"
#include "molecule.hh"
#include "staff-elem.hh"
#include "debug.hh"

String
Score_elem::TeXstring() const
{
    Molecule m(*output);
    m.translate(offset_);	// ugh?
    return m.TeXstring();
}

Score_elem::Score_elem(Score_elem const&s)
      :dependancy_l_arr_(s.dependancy_l_arr_),
        dependant_l_arr_(s.dependant_l_arr_)
{
    status = s.status;
    assert(!s.output);
    output = 0;
    pstaff_l_ = s.pstaff_l_;
    offset_ = Offset(0,0);
}

/**
  TODO:
  If deleted, then remove dependant_l_arr_ depency!
  */
Score_elem::~Score_elem()
{
    assert(status < DELETED);
    delete output;
    status = DELETED;
    output = 0;
}

void
Score_elem::translate(Offset O)
{
    offset_ += O;
}

Interval
Score_elem::do_width() const 
{
    Interval r;
    
    if (!output){
	Molecule*m = brew_molecule_p();
	r = m->extent().x;
	delete m;
    } else
	r = output->extent().x;
    return r;
}

Interval
Score_elem::width() const
{
    Interval r=do_width();

    if (!r.empty_b()) // float exception on DEC Alpha
	r+=offset_.x;

    return r;
}
Interval
Score_elem::do_height() const 
{
    Interval r;
    if (!output){
	Molecule*m	= brew_molecule_p();
	r = m->extent().y;
	delete m;
    } else
	r = output->extent().y;
    return r;
}

Interval
Score_elem::height() const
{
    Interval r=do_height();

    if (!r.empty_b())
	r+=offset_.y;

  
    return r;
}

void
Score_elem::print()const
{
#ifndef NPRINT
    mtor << name() << "{\n";
    do_print();
    if (output)
	output->print();
    
    mtor <<  "}\n";
#endif
}



Score_elem::Score_elem()
{
    pstaff_l_=0;
    offset_ = Offset(0,0);
    output = 0;
    status = ORPHAN;
}


Paper_def*
Score_elem::paper()  const
{
    assert(pstaff_l_);
    return pstaff_l_->pscore_l_->paper_l_;
}

void
Score_elem::add_processing()
{
    if (status >= VIRGIN)
	return;
    status = VIRGIN;
    do_add_processing();
}

void
Score_elem::pre_processing()
{
    if (status >= PRECALCED )
	return;
    assert(status != PRECALCING); // cyclic dependency
    status = PRECALCING;

    for (int i=0; i < dependancy_l_arr_.size(); i++)
	if (dependancy_l_arr_[i])
	    dependancy_l_arr_[i]->pre_processing();

    
    do_pre_processing();
    status = PRECALCED;
}
void
Score_elem::post_processing()
{
    if (status >= POSTCALCED)
	return;
    assert(status != POSTCALCING);// cyclic dependency
    status=POSTCALCING;	

    for (int i=0; i < dependancy_l_arr_.size(); i++)
	if (dependancy_l_arr_[i])
	    dependancy_l_arr_[i]->post_processing();
    do_post_processing();
    status=POSTCALCED;
}

void 
Score_elem::molecule_processing()
{
    if (status >= OUTPUT)
	return;
    status = OUTPUT;		// do it only once.
    for (int i=0; i < dependancy_l_arr_.size(); i++)
	if (dependancy_l_arr_[i])
	    dependancy_l_arr_[i]->molecule_processing();

    output= brew_molecule_p();
}

void
Score_elem::do_post_processing()
{
}

void
Score_elem::do_pre_processing()
{
}
void
Score_elem::do_verticalcing()
{
}

void
Score_elem::do_add_processing()
{
}

void
Score_elem::substitute_dependency(Score_elem * old, Score_elem * newdep)
{
    bool hebbes_b=false;
    for (int i=0; i < dependancy_l_arr_.size(); i++) {
	if (dependancy_l_arr_[i] == old){
	    dependancy_l_arr_[i] = newdep;
	    hebbes_b = true;
	} else if (dependancy_l_arr_[i] == newdep) {
	    hebbes_b = true;
	}
    }
    if (!hebbes_b)
	dependancy_l_arr_.push(newdep);
}

void
Score_elem::add_dependency(Score_elem * p)
{
    for (int i=0; i < dependancy_l_arr_.size(); i ++)
	if (dependancy_l_arr_[i] == p)
	    return;
    
    dependancy_l_arr_.push(p);
    p->dependant_l_arr_.push(p);
}
IMPLEMENT_STATIC_NAME(Score_elem);

Molecule*
Score_elem::brew_molecule_p()const
{
    Atom a(paper()->lookup_l()->fill(Box(Interval(0,0), Interval(0,0))));
    return new Molecule (a);
}
Offset
Score_elem::offset() const
{
    return offset_; 
}

/*
  score-elem.cc -- implement Score_elem

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-score.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "score-elem.hh"
#include "debug.hh"
#include "tex.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "scoreline.hh"

Score_elem*
Score_elem::dependency(int i)const
{
    return (Score_elem*) get_out_edge_arr()[i];
}

int
Score_elem::dependency_size() const
{
    return get_out_edge_arr().size();
}

Score_elem*
Score_elem::dependent(int i) const
{
    return (Score_elem*) get_in_edge_arr()[i];
}

int
Score_elem::dependent_size() const
{
    return get_in_edge_arr().size();
}


String
Score_elem::TeX_string() const
{
    assert( status > POSTCALCED);
    String s("\\placebox{%}{%}{%}");
    Array<String> a;
    a.push(print_dimen(offset_.y));
    a.push(print_dimen(offset_.x));
    a.push( output->TeX_string());
    return substitute_args(s, a);
}


Score_elem::Score_elem(Score_elem const&s)
{
    /* called from derived ctor, so most info points to the same deps
      as (Directed_graph_node&)s. Nobody points to us, so don't copy
      dependents.      
     */
    copy_edges_out(s);
    group_element_i_ = 0;
    status = s.status;
    assert(!s.output);
    output = 0;
    pscore_l_ = s.pscore_l_;
    offset_ = Offset(0,0);
}

Score_elem::~Score_elem()
{
    // some paranoia to prevent weird segv's
    assert(status < DELETED);
    delete output;
    status = DELETED;
    output = 0;
    assert(!group_element_i_ );
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
	Molecule*m = brew_molecule_p();
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
    mtor << "deps: " << dependent_size() << "depts: \n" << 
	dependency_size() << "\n";
    do_print();
    if (output)
	output->print();
    
    mtor <<  "}\n";
#endif
}



Score_elem::Score_elem()
{
    group_element_i_ = 0;
    pscore_l_=0;
    offset_ = Offset(0,0);
    output = 0;
    status = ORPHAN;
}


Paper_def*
Score_elem::paper()  const
{
    assert(pscore_l_);
    return pscore_l_->paper_l_;
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

    for (int i=0; i < dependency_size(); i++)
	dependency(i)->pre_processing();

    
    do_pre_processing();
    status = PRECALCED;
}

void
Score_elem::break_processing()
{
    if (status >= BROKEN )
	return;

    assert(status != BREAKING); // cyclic dependency
    status = BREAKING;

    for (int i=0; i < dependency_size(); i++)
	dependency(i)->break_processing();

    
    do_break_processing();
    status = BROKEN;
}

void
Score_elem::do_break_processing()
{
    handle_broken_dependencies();
}


void
Score_elem::post_processing()
{
    if (status >= POSTCALCED)
	return;
    assert(status != POSTCALCING);// cyclic dependency
    status=POSTCALCING;	

  
    for (int i=0; i < dependency_size(); i++)
	dependency(i)->post_processing();
    do_post_processing();
    status=POSTCALCED;
}

void 
Score_elem::molecule_processing()
{
    if (status >= OUTPUT)
	return;
    status = OUTPUT;		// do it only once.
  
    for (int i=0; i < dependency_size(); i++)
	dependency(i)->molecule_processing();

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
Score_elem::do_add_processing()
{

}

void
Score_elem::do_substitute_dependency(Score_elem*,Score_elem*)
{
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

Line_of_score *
Score_elem::line_l()const
{
    return 0;
}

/********************
  DEPENDENCIES
 */

void
Score_elem::remove_dependency(Score_elem*e)
{
    remove_edge_out(e);
    do_substitute_dependency(e, 0);
}

void
Score_elem::add_dependency(Score_elem*e)
{
    Directed_graph_node::add(e);
}

bool
Score_elem::is_type_b(char const *s)
{
    return s == static_name();
}

void
Score_elem::handle_broken_dependencies()
{
    Line_of_score *line  = line_l();
    if (!line)
	return;

    Link_array<Score_elem> remove_us_arr;
    for (int i=0; i < dependency_size(); i++) {
	Score_elem * elt = dependency(i);
	if (elt->line_l() != line){ 
	    if (elt->spanner()) {
		Spanner * sp = elt->spanner();
		Spanner * broken = sp->find_broken_piece(line);
		do_substitute_dependency(sp, broken);
		add_dependency(broken);
		remove_us_arr.push(sp);
	    }
	    remove_us_arr.push(elt);
	} 
	
    }

    remove_us_arr.default_sort();
    remove_us_arr.uniq();
    for (int i=0;  i <remove_us_arr.size(); i++)
	remove_dependency(remove_us_arr[i]);

    if (status < BROKEN)
	status = BROKEN;
}


void
Score_elem::unlink_all()
{
    for (int i=0; i < dependency_size(); i++) 
	dependency(i)->unlink_all();
    junk_links();
    group_element_i_ = 0;
}

void
Score_elem::unlink()
{
    while ( dependency_size()) {
	do_substitute_dependency(dependency(0),0);
	remove_edge_out_idx(0);
    }
    while  ( dependent_size() ) {
	dependent(0)->remove_dependency(this);
    }
}



void
Score_elem::OK()const
{
#ifndef NDEBUG
    for (int i=0; i < dependency_size(); i++) {
	dependency(i)->OK();
    }
#endif
}

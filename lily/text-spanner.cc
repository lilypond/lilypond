/*
  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "molecule.hh"
#include "boxes.hh"
#include "text-spanner.hh"
#include "text-def.hh"
#include "debug.hh"
#include "paper-def.hh"



void
Text_spanner::set_support(Directional_spanner*d)
{
    if (support)
	remove_dependency(support);
    
    support =d;
    add_dependency(d);
}

Text_spanner::Text_spanner()
{
    support = 0;
}

IMPLEMENT_STATIC_NAME(Text_spanner);

void
Text_spanner::do_print() const
{
    spec.print();
}

void
Text_spanner::do_post_processing()
{
    switch(spec.align_i_) {
    case 0:
	text_off_ = support->center() +
	    Offset(0,support->dir_i_ * paper()->internote_f() * 4); // todo
	break;
    default:
	assert(false);
	break;
    }    
}

Molecule*
Text_spanner::brew_molecule_p() const
{
    Atom tsym (spec.create_atom());
    tsym.translate(text_off_);

    Molecule*output = new Molecule;
    output->add( tsym );
    return output;
}

void
Text_spanner::do_pre_processing()
{
    right_col_l_ = support->right_col_l_;
    left_col_l_ = support->left_col_l_;
    assert(left_col_l_ && right_col_l_);
    spec.pdef_l_ = paper();
}

Interval
Text_spanner::height()const
{
    return brew_molecule_p()->extent().y;
}

void
Text_spanner::do_substitute_dependency(Score_elem* o, Score_elem*n)
{
    Directional_spanner * old = (Directional_spanner*)o->spanner();
    if (support == old)
	support = (Directional_spanner*) n->spanner();
}


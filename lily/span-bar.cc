/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "span-bar.hh"
#include "lookup.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "vertical-align-elem.hh"

void
Span_bar::add(Bar*b)
{
    b->spanned_i_ ++;
    spanning_l_arr_.push(b);
    add_dependency( b );
}



void
Span_bar::do_substitute_dependency(Score_elem*o, Score_elem*n)
{
    Bar * bold = 0;
    if  (o->is_type_b(Bar::static_name() )) 
	bold = (Bar*)o->item();
    else
	return;

    bold->spanned_i_ --;
    Bar * b =0;
    if (n && n->is_type_b(Bar::static_name() )) {
	b = (Bar*)n->item();
	b->spanned_i_ ++;
    }
    
    spanning_l_arr_.substitute( bold , b);

}

void
Span_bar::set(Vertical_align_element *a)
{
    add_dependency( a );
}
    

void
Span_bar::do_pre_processing()
{
    if ( spanning_l_arr_.size () < 1) {
	transparent_b_ = true;
	empty_b_ =true;
    } else {
	type_str_ = spanning_l_arr_[0]->type_str_;
	if (type_str_ =="") {
	    transparent_b_=true;
	    empty_b_ = true;
	}
    }
}

Molecule*
Span_bar::brew_molecule_p()const
{
    Interval y;
    for (int i=0; i < spanning_l_arr_.size(); i++)
	y.unite( spanning_l_arr_[i]->height() );
    Symbol s = paper()->lookup_l()->bar(type_str_, y.length());
        Molecule*output = new Molecule(Atom(s));
    output->translate_y (  y[-1] );

    return output;
}

IMPLEMENT_STATIC_NAME(Span_bar);
IMPLEMENT_IS_TYPE_B1(Span_bar,Bar);

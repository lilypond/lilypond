/*
  span-brace-item.cc -- implement Span_brace_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
	   Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "span-brace-item.hh"
#include "lookup.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "vertical-align-elem.hh"

IMPLEMENT_STATIC_NAME(Span_brace_item);
IMPLEMENT_IS_TYPE_B1( Span_brace_item, Item );

void
Span_brace_item::add( Bar* b )
{
    b->spanned_i_++;
    spanning_l_arr_.push( b );
    add_dependency( b );
}

void
Span_brace_item::do_substitute_dependency( Score_elem* o, Score_elem* n )
{
    Bar* bold = 0;
    if ( o->is_type_b( Bar::static_name() ) ) 
	bold = (Bar*)o->item();
    else
	return;

    bold->spanned_i_--;
    Bar* b = 0;
    if ( n && n->is_type_b( Bar::static_name() ) ) {
	b = (Bar*)n->item();
	b->spanned_i_++;
    }
    
    spanning_l_arr_.substitute( bold, b );
}

void
Span_brace_item::set( Vertical_align_element* a )
{
    add_dependency( a );
}
    

void
Span_brace_item::do_pre_processing()
{
    if ( spanning_l_arr_.size () < 1 ) {
	transparent_b_ = true;
	empty_b_ = true;
    } 
    else { // 0: nobreak, 1: pre, 2: post
	empty_b_ = ( break_status_i() != 2 );
	transparent_b_ = ( break_status_i() != 1 );
    }
}

Molecule*
Span_brace_item::brew_molecule_p() const
{
    Interval y;
    for ( int i = 0; i < spanning_l_arr_.size(); i++ )
	y.unite( spanning_l_arr_[ i ]->height() );
    // ugh, one extra staff @ 16pt
//    Real length_f = y.length() + spanning_l_arr_[ 0 ]->height().length();
//    Real length_f = y.length() + 16;
    Real length_f = y.length() - 6;
    Symbol s = paper()->lookup_l()->vbrace( length_f );
    Molecule* mol_p = new Molecule( Atom ( s ) );
//    mol_p->translate_y( - ( length_f + 16 ) / 2 );
    // urgh, what's wrong here?
    mol_p->translate_y( - length_f / 2 - 6 );

    return mol_p;
}


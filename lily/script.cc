/*
  script.cc -- implement Script

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "paper-def.hh"
#include "script.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"



void
Script::set_stem(Stem*st_l)
{
    stem_l_ = st_l;
    add_support(st_l);
}


Script::Script(Script_req* rq)
{    
    specs_l_ = rq->scriptdef_p_;
    inside_staff_b_ = specs_l_->inside_staff_b_;
    stem_l_ = 0;
    pos_i_ = 0;
    symdir_i_=1;
    dir_i_ =rq->dir_i_;
}

void
Script::set_symdir()
{
    if (specs_l_->invertsym_b_)
	symdir_i_ = (dir_i_ < 0) ? -1:1;
}

void
Script::set_default_dir()
{
    int s_i=specs_l_->rel_stem_dir_i_;
    if (s_i && stem_l_)
	dir_i_ = stem_l_->dir_i_ * s_i;
    else {
	dir_i_ =specs_l_->staff_dir_i_;
    }
}

void
Script::set_default_index()
{
    pos_i_ = get_position_i(symbol().dim.y);
}

Interval
Script::do_width() const
{
    return symbol().dim.x;
}

Symbol
Script::symbol()const
{
    String preidx_str = (symdir_i_ < 0) ?"-" :"";
    return paper()->lookup_l()->script(preidx_str + specs_l_->symidx);
}

void
Script::do_pre_processing()
{
    if (!dir_i_)
	set_default_dir();
    set_symdir();
}

void
Script::do_post_processing()
{
    set_default_index();
}

Molecule*
Script::brew_molecule_p() const
{
    Real dy = paper()->internote_f();
    
    Molecule*out = new Molecule(Atom(symbol()));
    out->translate(Offset(0,dy * pos_i_));
    return out;
}
IMPLEMENT_STATIC_NAME(Script);

int 
Script::compare(Script  *const&l1, Script *const&l2) 
{
    return l1->specs_l_->priority_i_ - l2->specs_l_->priority_i_;
}
    

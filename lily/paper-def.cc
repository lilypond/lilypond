/*
  paper-def.cc -- implement Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <math.h>
#include "string.hh"
#include "assoc.hh"
#include "misc.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "lookup.hh"
#include "dimen.hh"
#include "input-translator.hh"
#include "engraver-group.hh"
#include "assoc-iter.hh"

void
Paper_def::set_var(String s, Real r)
{
   real_vars_p_->elem(s) = r;
}

Real
Paper_def::get_var(String s)const
{
    if(! real_vars_p_->elt_b(s))
	error ( "unknown paper variable `"  + s+"'");
    return real_vars_p_->elem(s);
}

Real
Paper_def::linewidth_f() const
{
    return get_var("linewidth");
}

Real
Paper_def::duration_to_dist(Moment d)
{
    if (!d)
	return 0;
    
    return get_var("unitspace")  * pow(get_var("geometric"), log_2(d));
}


Paper_def::Paper_def()
{
    itrans_p_ = 0;
    lookup_p_ = 0;
    real_vars_p_ = new Assoc<String,Real>;
    outfile_str_ = "lelie.tex";
}

Paper_def::~Paper_def()
{
    delete itrans_p_;
    delete real_vars_p_;
    delete lookup_p_;
}

Paper_def::Paper_def(Paper_def const&s)
{
    itrans_p_ = s.itrans_p_ ? new Input_translator( *s.itrans_p_):0;
    lookup_p_ = s.lookup_p_? new Lookup(*s.lookup_p_) : 0;
    lookup_p_->paper_l_ = this;
    real_vars_p_ = new Assoc<String,Real> (*s.real_vars_p_);
    outfile_str_ = s.outfile_str_;
}

void
Paper_def::set(Input_translator * itrans_p)
{
    delete itrans_p_;
    itrans_p_  = itrans_p;
}

void
Paper_def::set(Lookup*l)
{
    assert(l != lookup_p_);
    delete lookup_p_;
    lookup_p_ = l;
    lookup_p_->paper_l_ = this;
}

Real
Paper_def::interline_f() const
{
    return get_var("interline");
}


Real
Paper_def::rule_thickness()const
{
    return get_var("rule_thickness");
}

Real
Paper_def::interbeam_f() const
{
    return get_var("interbeam");
}
Real
Paper_def::internote_f() const
{
    return interline_f() / 2; 
}

Real
Paper_def::note_width()const
{
    return get_var("notewidth");
}

void
Paper_def::print() const
{
#ifndef NPRINT
    mtor << "Paper {";
    mtor << "out: " <<outfile_str_;
    lookup_p_->print();
    itrans_p_->print();
    for (Assoc_iter<String,Real> i(*real_vars_p_); i.ok(); i++) {
	mtor << i.key() << "= " << i.val() << "\n";
    }
    mtor << "}\n";
#endif
}

Lookup const *
Paper_def::lookup_l()
{
    assert( lookup_p_ );
    return lookup_p_;
}

Global_translator*
Paper_def::get_global_translator_p() const
{
    return  itrans_p_->get_group_engraver_p()->global_l();
}

/*

  TODO:
  
  think about crossing stems.
  Begin and end should be treated as a Script.
  
 */
#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "notehead.hh"
#include "p-col.hh"
#include "molecule.hh"
#include "debug.hh"
#include "boxes.hh"



Slur::Slur()
{
    open_right=open_left=false;
}

Offset
Slur::center() const
{
    int pos1 = encompass.top()->position_i_;
    int pos2 = encompass[0]->position_i_;

    int dy =  pos1-pos2;

    Real w = width().length();

    return Offset(w/2,dy * paper()->internote());
}

void
Slur::add(Notehead*n)
{
    encompass.push(n);
    add_dependency(n);
}

void
Slur::set_default_dir()
{
    int sumpos=0;
    for (int i=0; i < encompass.size(); i ++) {
	sumpos += encompass[i]->position_i_;
    }

    /* should consult stems */
    Real meanpos = sumpos/Real(encompass.size());
    if (meanpos < 5)		// todo
	dir_i_ = -1;
    else
	dir_i_ = 1;    
}

void
Slur::do_pre_processing()
{
    right_col_l_  = encompass.top()->pcol_l_;
    left_col_l_ = encompass[0]->pcol_l_;    
}

Spanner*
Slur::do_break_at(PCol*l, PCol*r) const
{
    assert(l->line_l_ == r->line_l_);
    Slur*ret = new Slur(*this);

    ret->encompass.set_size(0);
    for (int i =0; i < encompass.size(); i++) {
	if (encompass[i]->pcol_l_->line_l_==l->line_l_)
	    ret->encompass.push(encompass[i]);
    }
    if (right_col_l_ != r)
	ret->open_right = true;
    if (left_col_l_ != l)
	ret->open_left = true;


    return ret;
}

void
Slur::do_post_processing()
{
    if (!dir_i_)
	set_default_dir();
}

Molecule*
Slur::brew_molecule_p() const
{
    Molecule*output = new Molecule;

    int minp=1000, maxp=-1000;	// todo    
    for (int i=0; i<encompass.size(); i++) {
	minp = encompass[i]->position_i_ <? minp;
	maxp = encompass[i]->position_i_ >? maxp;
    }
    assert(encompass.size()>0);	// todo
    
    Notehead *lnote_p =encompass[0];
    Notehead *rnote_p =encompass.top();
    int lpos_i = lnote_p->position_i_;
    int rpos_i = rnote_p->position_i_;
    Offset  left_off(lnote_p->x_dir_i_, lpos_i + 2*dir_i_);
    Offset right_off(lnote_p->x_dir_i_, rpos_i + 2*dir_i_);
    if (!lnote_p->extremal_i_)
	left_off += Offset(0.5, -dir_i_);
    if (!rnote_p->extremal_i_)
	right_off+= Offset(-0.5, -dir_i_);
    
    int dy = int(right_off.y - left_off.y);
    
    Real nw_f = paper()->note_width();
    Real nh_f = paper()->internote();
    Real w = width().length();
    
    w+= (right_off.x - left_off.x) * nw_f ;
    Real round_w = w;		// slur lookup rounds the slurwidth .
    
    Symbol sl = paper()->lookup_l()->slur(dy , round_w, dir_i_);

    Real error = w-round_w;
    
    Atom a(sl);
    a.translate(Offset((left_off.x + 0.5 )*nw_f + error/2,
		       left_off.y * nh_f));
    output->add(a);
    return output;
}

IMPLEMENT_STATIC_NAME(Slur);

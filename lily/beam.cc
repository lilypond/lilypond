/*
  beam.cc -- implement Beam

  source file of the GNU GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO

  Less hairy code. Better slope calculations.
  knee: ([\stem 1; c8 \stem -1; c8]
  
*/

#include <math.h>

#include "p-col.hh"
#include "varray.hh"
#include "proto.hh"
#include "dimen.hh"
#include "beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "grouping.hh"



struct Stem_info {
    Real x;
    Real idealy;
    Real miny;
    int no_beams;

    
    Stem_info(){}
    Stem_info(Stem const *);
};

Stem_info::Stem_info(Stem const *s)
{
    x = s->hpos_f();
    int dir = s->dir_i_;
    idealy  = dir * s->stem_end_f();
    miny = dir * s->stem_start_f() + 2;	// ugh
    assert(miny <= idealy);
}


/* *************** */



Offset
Beam::center()const
{
    Real w=(paper()->note_width() + width().length())/2.0;
    return Offset(w, (left_pos + w* slope)*paper()->internote_f());
}


Beam::Beam()
{
    slope = 0;
    left_pos = 0.0;
}

void
Beam::add(Stem*s)
{
    stems.push(s);
    s->add_dependency(this);
    s->print_flag_b_ = false;
}

void
Beam::set_default_dir()
{
    int up = 0, down = 0;
    int up_count = 0, down_count = 0;

    for (int i=0; i <stems.size(); i++) {
	Stem *sl = stems[i];
	int cur_down = sl->get_center_distance_from_top();
	int cur_up = sl->get_center_distance_from_bottom();
	if (cur_down) {
	    down += cur_down;
	    down_count++;
	}
	if (cur_up) {
	    up += cur_up;
	    up_count++;
	}
    }
    if (!down)
	down_count = 1;
    if (!up)
	up_count = 1;

    // the following relation is equal to
    //        up / up_count > down / down_count
    dir_i_ = (up * down_count > down * up_count) ? 1 : -1;

   for (int i=0; i <stems.size(); i++) {
	Stem *sl = stems[i];
	sl->dir_i_ = dir_i_;
   }
}

/*
  should use minimum energy formulation (cf linespacing)
  */
void
Beam::solve_slope()
{
    Array<Stem_info> sinfo;
    for (int j=0; j <stems.size(); j++) {
	Stem *i = stems[j];

	i->set_default_extents();
	if (i->invisible_b())
	    continue;
	
	Stem_info info(i);
	sinfo.push(info);
    }
    Real leftx = sinfo[0].x;
    Least_squares l;
    for (int i=0; i < sinfo.size(); i++) {
	sinfo[i].x -= leftx;
	l.input.push(Offset(sinfo[i].x, sinfo[i].idealy));
    }

    l.minimise(slope, left_pos);
    Real dy = 0.0;
    for (int i=0; i < sinfo.size(); i++) {
	Real y = sinfo[i].x * slope + left_pos;
	Real my = sinfo[i].miny;

	if (my - y > dy)
	    dy = my -y;	
    }
    left_pos += dy;
    left_pos *= dir_i_;    

    slope *= dir_i_;
    slope = 0.6 * tanh(slope);  // damping

				// ugh
    Real sl = slope*paper()->internote_f();
    paper()->lookup_l()->beam(sl, 20 PT);
    slope = sl /paper()->internote_f();
}

void
Beam::set_stemlens()
{
    Real x0 = stems[0]->hpos_f();    
    for (int j=0; j <stems.size(); j++) {
	Stem *s = stems[j];

	Real x =  s->hpos_f()-x0;
	s->set_stemend(left_pos + slope * x);	
    }
}


void
Beam::do_post_processing()
{
    solve_slope();    
    set_stemlens();
}

void
Beam::set_grouping(Rhythmic_grouping def, Rhythmic_grouping cur)
{
    def.OK();
    cur.OK();
    assert(cur.children.size() == stems.size());
    
    cur.split(def);

    Array<int> b;
    {
	Array<int> flags;
	for (int j=0; j <stems.size(); j++) {
	    Stem *s = stems[j];

	    int f = intlog2(abs(s->flag_i_))-2;
	    assert(f>0);
	    flags.push(f);
	}
	int fi =0;
	b= cur.generate_beams(flags, fi);
	b.insert(0,0);
	b.push(0);
	assert(stems.size() == b.size()/2);
    }

    for (int j=0, i=0; i < b.size() && j <stems.size(); i+= 2, j++) {
	Stem *s = stems[j];
	s->beams_left_i_ = b[i];
	s->beams_right_i_ = b[i+1];
    }
}

void
Beam::do_pre_processing()
{
    assert(stems.size()>1);
    if (!dir_i_)
	set_default_dir();

}


Interval
Beam::do_width() const
{
    return Interval( stems[0]->hpos_f(),
		     stems.top()->hpos_f() );
}

/*
  beams to go with one stem.
  */
Molecule
Beam::stem_beams(Stem *here, Stem *next, Stem *prev)const
{
    assert( !next || next->hpos_f() > here->hpos_f()  );
    assert( !prev || prev->hpos_f() < here->hpos_f()  );
    Real dy=paper()->internote_f()*2;
    Real stemdx = paper()->rule_thickness();
    Real sl = slope*paper()->internote_f();
    paper()->lookup_l()->beam(sl, 20 PT);

    Molecule leftbeams;
    Molecule rightbeams;

    /* half beams extending to the left. */
    if (prev) {
	int lhalfs= lhalfs = here->beams_left_i_ - prev->beams_right_i_ ;
	int lwholebeams= here->beams_left_i_ <? prev->beams_right_i_ ;
	Real w = (here->hpos_f() - prev->hpos_f())/4;
	Symbol dummy;
	Atom a(dummy);
	if (lhalfs)		// generates warnings if not
	    a =  paper()->lookup_l()->beam(sl, w);
	a.translate(Offset (-w, -w * sl));
	for (int j = 0; j  < lhalfs; j++) {
	    Atom b(a);
	    b.translate_y( -dir_i_ * dy * (lwholebeams+j));
	    leftbeams.add( b );
	}
    }
	
    if (next){
	int rhalfs = here->beams_right_i_ - next->beams_left_i_;
	int rwholebeams = here->beams_right_i_ <? next->beams_left_i_; 

	Real w = next->hpos_f() - here->hpos_f();
	Atom a = paper()->lookup_l()->beam(sl, w + stemdx);
	
	int j = 0;
	for (; j  < rwholebeams; j++) {
	    Atom b(a);
	    b.translate_y( -dir_i_ * dy * j);
	    rightbeams.add( b ); 
	}

	w /= 4;
	if (rhalfs)
	    a = paper()->lookup_l()->beam(sl, w);
	
	for (; j  < rwholebeams + rhalfs; j++) {
	    Atom b(a);
	    b.translate_y( -dir_i_ * dy * j);
	    rightbeams.add(b ); 
	}
	
    }
    leftbeams.add(rightbeams);
    return leftbeams;
}


Molecule*
Beam::brew_molecule_p() const 
{
    Molecule *out=0;
    Real inter=paper()->internote_f();
    out = new Molecule;
    Real x0 = stems[0]->hpos_f();
    for (int j=0; j <stems.size(); j++) {
	Stem *i = stems[j];
	Stem * prev = (j > 0)? stems[j-1] : 0;
	Stem * next = (j < stems.size()-1) ? stems[j+1] :0;

	Molecule sb = stem_beams(i, next, prev);
	Real  x = i->hpos_f()-x0;
	sb.translate(Offset(x, (x * slope  + left_pos)* inter));
	out->add(sb);
    }
    out->translate_x(x0 - left_col_l_->hpos);
    return out;
}

IMPLEMENT_STATIC_NAME(Beam);
IMPLEMENT_IS_TYPE_B1(Beam, Spanner);

void
Beam::do_print()const
{
#ifndef NPRINT
    mtor << "slope " <<slope << "left ypos " << left_pos;
    Spanner::print();
#endif
}

void
Beam::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    int i;
    while ((i=stems.find_i((Stem*)o->item())) >=0) 
	   if (n) stems[i] = (Stem*) n->item();
	   else stems.del(i);
}

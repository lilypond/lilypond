/*
  beam.cc -- implement Beam

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO

  Less hairy code.  knee: ([\stem 1; c8 \stem -1; c8]
  
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
    int dir_i_;
    Real idealy_f_;
    Real miny_f_;
    int beams_i_;

    Stem_info(){}
    Stem_info (Stem const *);
};

Stem_info::Stem_info (Stem const *s)
{
    x = s->hpos_f();
    dir_i_ = s->dir_i_;
    beams_i_ = intlog2( s->flag_i_) - 2;

    /*
     [todo] 
         * get algorithm
	 * runtime

     Breitkopf + H\"artel:
         miny_f_ = interline + #beams * interbeam
	 ideal8 = 2 * interline + interbeam
	 ideal16,32,64,128 = 1.5 * interline + #beams * interbeam

     * B\"arenreiter:
         miny_f_ = interline + #beams * interbeam
	 ideal8,16 = 2 interline + #beams * interbeam
	 ideal32,64,128 = 1.5 interline + #beams * interbeam
         
     */

    Real notehead_y = s->paper()->interline_f ();
    // huh? why do i need the / 2
//    Real interbeam_f = s->paper()->interbeam_f ();
    Real interbeam_f = s->paper()->interbeam_f () / 2;
           
    /* well eh, huh?
    idealy_f_  = dir_i_ * s->stem_start_f() + beams_i_ * interbeam_f; 
    if ( beams_i_ < 3)
	idealy_f_ += 2 * interline_f;
    else
	idealy_f_ += 1.5 * interline_f;
    */

    idealy_f_  = dir_i_ * s->stem_end_f();

    miny_f_ = dir_i_ * s->stem_start_f() + notehead_y + beams_i_ * interbeam_f;

    idealy_f_ =  miny_f_ >? idealy_f_;
//    assert (miny_f_ <= idealy_f_);
}


/* *************** */


Offset
Beam::center()const
{
    Real w=(paper()->note_width () + width ().length ())/2.0;
    return Offset (w, (left_pos + w* slope)*paper()->internote_f ());
}


Beam::Beam()
{
    slope = 0;
    left_pos = 0.0;
}

void
Beam::add (Stem*s)
{
    stems.push (s);
    s->add_dependency (this);
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

  [todo]
  the y of the (start) of the beam should be quantisized,
  so that no stafflines appear just in between two beam-flags

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
	
	Stem_info info (i);
	sinfo.push (info);
    }
    if (! sinfo.size())
	slope = left_pos = 0;
    else if (sinfo.size() == 1) {
	slope = 0;
	left_pos = sinfo[0].idealy_f_;
    } else {
	
	Real leftx = sinfo[0].x;
	Least_squares l;
	for (int i=0; i < sinfo.size(); i++) {
	    sinfo[i].x -= leftx;
	    l.input.push (Offset (sinfo[i].x, sinfo[i].idealy_f_));
	}

	l.minimise (slope, left_pos);
    }
    
    Real dy = 0.0;
    for (int i=0; i < sinfo.size(); i++) {
	Real y = sinfo[i].x * slope + left_pos;
	Real my = sinfo[i].miny_f_;

	if (my - y > dy)
	    dy = my -y;	
    }
    left_pos += dy;
    left_pos *= dir_i_;    

    slope *= dir_i_;

    /*
      This neat trick is by Werner Lemberg, damped = tanh (slope) corresponds
      with some tables in [Wanske]
     */
    slope = 0.6 * tanh (slope);  

				// ugh
    Real sl = slope*paper()->internote_f ();
    paper()->lookup_l ()->beam (sl, 20 PT);
    slope = sl /paper()->internote_f ();
}

void
Beam::set_stemlens()
{
    Real x0 = stems[0]->hpos_f();    
    for (int j=0; j <stems.size(); j++) {
	Stem *s = stems[j];

	Real x =  s->hpos_f()-x0;
	s->set_stemend (left_pos + slope * x);	
    }
}


void
Beam::do_post_processing()
{
    if ( stems.size() < 2) {
	warning ("Beam with less than 2 stems");
	transparent_b_ = true;
	return ;
    }
    solve_slope();    
    set_stemlens();
}

void
Beam::set_grouping (Rhythmic_grouping def, Rhythmic_grouping cur)
{
    def.OK();
    cur.OK();
    assert (cur.children.size() == stems.size ());
    
    cur.split (def);

    Array<int> b;
    {
	Array<int> flags;
	for (int j=0; j <stems.size(); j++) {
	    Stem *s = stems[j];

	    int f = intlog2(abs (s->flag_i_))-2;
	    assert (f>0);
	    flags.push (f);
	}
	int fi =0;
	b= cur.generate_beams (flags, fi);
	b.insert (0,0);
	b.push (0);
	assert (stems.size() == b.size ()/2);
    }

    for (int j=0, i=0; i < b.size() && j <stems.size (); i+= 2, j++) {
	Stem *s = stems[j];
	s->beams_left_i_ = b[i];
	s->beams_right_i_ = b[i+1];
    }
}

void
Beam::do_pre_processing()
{
    if (!dir_i_)
	set_default_dir();

}


Interval
Beam::do_width() const
{
    return Interval (stems[0]->hpos_f(),
		     stems.top()->hpos_f ());
}

/*
  beams to go with one stem.
  */
Molecule
Beam::stem_beams (Stem *here, Stem *next, Stem *prev)const
{
    assert (!next || next->hpos_f() > here->hpos_f () );
    assert (!prev || prev->hpos_f() < here->hpos_f () );
//    Real dy=paper()->internote_f ()*2;
    Real dy = paper()->interbeam_f ();
    Real stemdx = paper()->rule_thickness ();
    Real sl = slope*paper()->internote_f ();
    paper()->lookup_l ()->beam (sl, 20 PT);

    Molecule leftbeams;
    Molecule rightbeams;

    /* half beams extending to the left. */
    if (prev) {
	int lhalfs= lhalfs = here->beams_left_i_ - prev->beams_right_i_ ;
	int lwholebeams= here->beams_left_i_ <? prev->beams_right_i_ ;
	Real w = (here->hpos_f() - prev->hpos_f ())/4;
	Symbol dummy;
	Atom a (dummy);
	if (lhalfs)		// generates warnings if not
	    a =  paper()->lookup_l ()->beam (sl, w);
	a.translate (Offset (-w, -w * sl));
	for (int j = 0; j  < lhalfs; j++) {
	    Atom b (a);
	    b.translate (-dir_i_ * dy * (lwholebeams+j), Y_AXIS);
	    leftbeams.add (b);
	}
    }
	
    if (next){
	int rhalfs = here->beams_right_i_ - next->beams_left_i_;
	int rwholebeams = here->beams_right_i_ <? next->beams_left_i_; 

	Real w = next->hpos_f() - here->hpos_f ();
	Atom a = paper()->lookup_l ()->beam (sl, w + stemdx);
	
	int j = 0;
	for (; j  < rwholebeams; j++) {
	    Atom b (a);
	    b.translate (-dir_i_ * dy * j, Y_AXIS);
	    rightbeams.add (b); 
	}

	w /= 4;
	if (rhalfs)
	    a = paper()->lookup_l ()->beam (sl, w);
	
	for (; j  < rwholebeams + rhalfs; j++) {
	    Atom b (a);
	    b.translate (-dir_i_ * dy * j, Y_AXIS);
	    rightbeams.add (b); 
	}
	
    }
    leftbeams.add (rightbeams);
    return leftbeams;
}


Molecule*
Beam::brew_molecule_p() const 
{
 
    Molecule *mol_p = new Molecule;
    // huh? inter-what
//    Real inter_f = paper()->interbeam_f ();
    Real inter_f = paper()->internote_f ();
    Real x0 = stems[0]->hpos_f();
    for (int j=0; j <stems.size(); j++) {
	Stem *i = stems[j];
	Stem * prev = (j > 0)? stems[j-1] : 0;
	Stem * next = (j < stems.size()-1) ? stems[j+1] :0;

	Molecule sb = stem_beams (i, next, prev);
	Real  x = i->hpos_f()-x0;
	sb.translate (Offset (x, (x * slope  + left_pos)* inter_f));
	mol_p->add (sb);
    }
    mol_p->translate (x0 - left_col_l_->hpos_f_, X_AXIS);
    return mol_p;
}


IMPLEMENT_IS_TYPE_B1(Beam, Spanner);

void
Beam::do_print()const
{
#ifndef NPRINT
    DOUT << "slope " <<slope << "left ypos " << left_pos;
    Spanner::do_print();
#endif
}

void
Beam::do_substitute_dependent (Score_elem*o,Score_elem*n)
{
    if (o->is_type_b (Stem::static_name())) {
	stems.substitute ((Stem*)o->item(),  n?(Stem*) n->item ():0);
    }
}

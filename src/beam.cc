#include "varray.hh"

#include "dimen.hh"
#include "beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "pcol.hh"
#include "stem.hh"
#include "paperdef.hh"
#include "lookup.hh"
#include "grouping.hh"

NAME_METHOD(Beam);

struct Stem_info {
    Real x;
    Real idealy;
    Real miny;
    int no_beams;

    
    Stem_info(){}
    Stem_info(const Stem*);
};

Stem_info::Stem_info(const Stem*s)
{
    x = s->hindex();
    int dir = s->dir;
    idealy  = max(dir*s->top, dir*s->bot);
    miny = max(dir*s->minnote, dir*s-> maxnote);
    assert(miny <= idealy);

}

/* *************** */

Offset
Beam::center()const
{
    assert(status >= POSTCALCED);

    Real w=(paper()->note_width() + width().length())/2.0;
    return Offset(w, (left_pos + w* slope)*paper()->internote());
}


Beam::Beam()
{
    slope = 0;
    left_pos = 0.0;
}

void
Beam::add(Stem*s)
{
    stems.bottom().add(s);
    s->add_depedency(this);
    s->print_flag = false;
}

void
Beam::set_default_dir()
{
    int dirs[2];
    dirs[0]=0; dirs[1] =0;
    for (iter_top(stems,i); i.ok(); i++) {
	int d = i->get_default_dir();
	dirs[(d+1)/2] ++;
    }
    dir_i_ =  (dirs[0] > dirs[1]) ? -1 : 1;
    for (iter_top(stems,i); i.ok(); i++) {
	i->dir = dir_i_;
    }
}

/*
  should use minimum energy formulation (cf linespacing)
  */
void
Beam::solve_slope()
{
    Array<Stem_info> sinfo;
    for (iter_top(stems,i); i.ok(); i++) {
	i->set_default_extents();
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

				// URG
    Real sl = slope*paper()->internote();
    paper()->lookup_p_->beam(sl, 20 PT);
    slope = sl /paper()->internote();
}

void
Beam::set_stemlens()
{
    iter_top(stems,s);
    Real x0 = s->hindex();    
    for (; s.ok() ; s++) {
	Real x =  s->hindex()-x0;
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
	iter_top(stems,s);
	Array<int> flags;
	for (; s.ok(); s++) {
	    int f = intlog2(abs(s->flag))-2;
	    assert(f>0);
	    flags.push(f);
	}
	int fi =0;
	b= cur.generate_beams(flags, fi);
	b.insert(0,0);
	b.push(0);
	assert(stems.size() == b.size()/2);
    }

    iter_top(stems,s);
    for (int i=0; i < b.size() && s.ok(); i+=2, s++) {
	s->beams_left = b[i];
	s->beams_right = b[i+1];
    }
}


// todo.
Spanner *
Beam::do_break_at( PCol *, PCol *) const
{
    Beam *beam_p= new Beam(*this);
    
    return beam_p;
}

void
Beam::do_pre_processing()
{
    left  = (*stems.top())   ->pcol_l_;
    right = (*stems.bottom())->pcol_l_;    
    assert(stems.size()>1);
    if (!dir_i_)
	set_default_dir();

}


Interval
Beam::width() const
{
    Beam * me = (Beam*) this;	// ugh
    return Interval( (*me->stems.top()) ->hindex(),
		     (*me->stems.bottom()) ->hindex() );
}

/*
  beams to go with one stem.
  */
Molecule
Beam::stem_beams(Stem *here, Stem *next, Stem *prev)const
{
    assert( !next || next->hindex() > here->hindex()  );
    assert( !prev || prev->hindex() < here->hindex()  );
    Real dy=paper()->internote()*2;
    Real stemdx = paper()->rule_thickness();
    Real sl = slope*paper()->internote();
    paper()->lookup_p_->beam(sl, 20 PT);

    Molecule leftbeams;
    Molecule rightbeams;

    /* half beams extending to the left. */
    if (prev) {
	int lhalfs= lhalfs = here->beams_left - prev->beams_right ;
	int lwholebeams= here->beams_left <? prev->beams_right ;
	Real w = (here->hindex() - prev->hindex())/4;
	Symbol dummy;
	Atom a(dummy);
	if (lhalfs)		// generates warnings if not
	    a =  paper()->lookup_p_->beam(sl, w);
	a.translate(Offset (-w, -w * sl));
	for (int j = 0; j  < lhalfs; j++) {
	    Atom b(a);
	    b.translate(Offset(0, -dir_i_ * dy * (lwholebeams+j)));
	    leftbeams.add( b );
	}
    }
	
    if (next){
	int rhalfs = here->beams_right - next->beams_left;
	int rwholebeams = here->beams_right <? next->beams_left; 

	Real w = next->hindex() - here->hindex();
	Atom a = paper()->lookup_p_->beam(sl, w + stemdx);
	
	int j = 0;
	for (; j  < rwholebeams; j++) {
	    Atom b(a);
	    b.translate(Offset(0, -dir_i_ * dy * j));
	    rightbeams.add( b ); 
	}

	w /= 4;
	if (rhalfs)
	    a = paper()->lookup_p_->beam(sl, w);
	
	for (; j  < rwholebeams + rhalfs; j++) {
	    Atom b(a);
	    b.translate(Offset(0, -dir_i_ * dy * j));
	    rightbeams.add(b ); 
	}
	
    }
    leftbeams.add(rightbeams);
    return leftbeams;
}


Molecule*
Beam::brew_molecule_p() const return out;
{
    Real inter=paper()->internote();
    out = new Molecule;
    Real x0 = stems.top()->hindex();
    
    for (iter_top(stems,i); i.ok(); i++) {
	PCursor<Stem*> p(i-1);
	PCursor<Stem*> n(i+1);
	Stem * prev = p.ok() ? p.ptr() : 0;
	Stem * next = n.ok() ? n.ptr() : 0;

	Molecule sb = stem_beams(i, next, prev);
	Real  x = i->hindex()-x0;
	sb.translate(Offset(x, (x * slope  + left_pos)* inter));
	out->add(sb);
    }
    out->translate(Offset(x0 - left->hpos,0));
}

void
Beam::do_print()const
{
#ifndef NPRINT
    mtor << "slope " <<slope << "left ypos " << left_pos;
    Spanner::print();
#endif
}

Beam::~Beam()
{

}

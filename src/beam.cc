#include "beam.hh"
#include "misc.hh"
#include "debug.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "pcol.hh"
#include "stem.hh"
#include "paper.hh"
#include "lookup.hh"
#include "scalar.hh"


struct Stem_info {
    Real x;
    Real idealy;
    Real miny;
    int no_beams;

    ///////////////
    
    Stem_info(){}
    Stem_info(const Stem*);
};
Stem_info::Stem_info(const Stem*s)
{
    x = s->hpos();
    int dir = s->dir;
    idealy  = MAX(dir*s->top, dir*s->bot);
    miny = MAX(dir*s->minnote, dir*s-> maxnote);
    assert(miny <= idealy);
    no_beams = s->flag;
}

/****************/

Beam::Beam()
{
    slope = 0;
    left_pos = 0.0;
    dir =0;
}

void
Beam::add(Stem*s)
{
    stems.bottom().add(s);
    s->print_flag = false;
}

void
Beam::set_default_dir()
{
    int dirs[2];
    dirs[0]=0; dirs[1] =0;
    for (PCursor<Stem*> sc(stems); sc.ok(); sc++) {
	sc->set_default_dir();
	dirs[(sc->dir+1)/2] ++;
    }
    dir =  (dirs[0] > dirs[1]) ? -1 : 1;
    for (PCursor<Stem*> sc(stems); sc.ok(); sc++) {
	sc->dir = dir;
    }
}

/*
  should use minimum energy formulation (cf linespacing)
  */
void
Beam::solve_slope()
{
    svec<Stem_info> sinfo;
    for (PCursor<Stem* >sc(stems); sc.ok(); sc++) {
	sc->set_default_extents();
	Stem_info i(sc);
	sinfo.add(i);
    }
    Real leftx = sinfo[0].x;
    Least_squares l;
    for (int i=0; i < sinfo.sz(); i++) {
	sinfo[i].x -= leftx;
	l.input.add(Offset(sinfo[i].x, sinfo[i].idealy));
    }

    l.minimise(slope, left_pos);
    Real dy = 0.0;
    for (int i=0; i < sinfo.sz(); i++) {
	Real y = sinfo[i].x * slope + left_pos;
	Real my = sinfo[i].miny;

	if (my - y > dy)
	    dy = my -y;	
    }
    left_pos += dy;
    left_pos *= dir;    
    slope *= dir;
    
}

void
Beam::set_stemlens()
{
    PCursor<Stem*> s(stems);
    Real x0 = s->hpos();    
    for (; s.ok() ; s++) {
	Real x =  s->hpos()-x0;
	s->set_stemend(left_pos + slope * x);	
    }
}

void
Beam::calculate()
{
    assert(stems.size()>1);
    if (!dir)
	set_default_dir();

    solve_slope();
}


void
Beam::process()
{
    calculate();

    /*
      quick and dirty!
      */
    for (PCursor<Stem*> i(stems); i.ok(); i++)
	i->beams_left = i->beams_right = intlog2(ABS(i->flag)) - 2;

    stems.top()->beams_left = 0;
    stems.bottom()->beams_right = 0;

    brew_molecule();
    set_stemlens();
}


// todo.
Spanner *
Beam::broken_at(const PCol *, const PCol *) const
{
    return new Beam(*this);
}

void
Beam::preprocess()
{
    left  = (*stems.top())   ->pcol_;
    right = (*stems.bottom())->pcol_;    
}

Interval
Beam::height() const
{
    return output->extent().y;
}

Interval
Beam::width() const
{
    Beam * me = (Beam*) this;	// ugh
    return Interval( (*me->stems.top()) ->hpos(),
		     (*me->stems.bottom()) ->hpos() );
}
/*
  beams to go with one stem.
  */
Molecule
Beam::stem_beams(Stem *here, Stem *next, Stem *prev)
{
    assert( !next || next->hpos() > here->hpos()  );
    assert( !prev || prev->hpos() < here->hpos()  );
    Real dy=paper()->internote()*2;
    Real stemdx = paper()->rule_thickness();
    Real sl = slope*paper()->internote();
    Molecule leftbeams;
    Molecule rightbeams;

    /* half beams extending to the left. */
    if (prev) {
	int lhalfs= lhalfs = here->beams_left - prev->beams_right ;
	int lwholebeams= here->beams_left <? prev->beams_right ;
	Real w = (here->hpos() - prev->hpos())/4;
	Atom a =  paper()->lookup_->beam(sl, w);
	a.translate(Offset (-w, -w * sl));
	for (int j = 0; j  < lhalfs; j++) {
	    Atom b(a);
	    b.translate(Offset(0, -dir * dy * (lwholebeams+j)));
	    leftbeams.add( b );
	}
    }
	
    if (next){
	int rhalfs = here->beams_right - next->beams_left;
	int rwholebeams = here->beams_right <? next->beams_left; 

	Real w = next->hpos() - here->hpos();
	Atom a = paper()->lookup_->beam(sl, w + stemdx);
	
	int j = 0;
	for (; j  < rwholebeams; j++) {
	    Atom b(a);
	    b.translate(Offset(0, -dir * dy * j));
	    rightbeams.add( b ); 
	}
	w /= 4;
	a = paper()->lookup_->beam(sl, w);
	
	for (; j  < rwholebeams + rhalfs; j++) {
	    Atom b(a);
	    b.translate(Offset(0, -dir * dy * j));
	    rightbeams.add(b ); 
	}
	
    }
    leftbeams.add(rightbeams);
    return leftbeams;
}


void
Beam::brew_molecule()
{
    assert(left->line == right->line);
    Real inter=paper()->internote();
    Real sl = slope*inter;

    output = new Molecule;
    slope = sl / inter;
    Real x0 = stems.top()->hpos();
    
    for (PCursor<Stem*> i(stems); i.ok(); i++) {
	PCursor<Stem*> p(i-1);
	PCursor<Stem*> n(i+1);
	Stem * prev = p.ok() ? p.ptr() : 0;
	Stem * next = n.ok() ? n.ptr() : 0;

	Molecule sb = stem_beams(i, next, prev);
	Real  x = i->hpos()-x0;
	sb.translate(Offset(x, (x * slope  + left_pos)* inter));
	output->add(sb);
    }
    output->translate(Offset(x0 - left->hpos,0));
}

void
Beam::print()const
{
#ifndef NPRINT
    mtor << "{ slope " <<slope << "left ypos " << left_pos;
    Spanner::print();
    mtor << "}\n";
#endif
}


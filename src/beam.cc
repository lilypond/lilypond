#include "beam.hh"
#include "debug.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "leastsquares.hh"
#include "pcol.hh"
#include "stem.hh"
#include "paper.hh"
#include "lookup.hh"


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
    
    {
	Real inter =paper()->internote();
	Real unitslope = slope*inter;

	// set beamslope, for setting stems correctly
	// ignoring return.
	Symbol sy = paper()->lookup_->beam(unitslope, width().length());
	slope =unitslope / inter;
    }
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
    set_stemlens();
}

void
Beam::process()
{
    calculate();
    brew_molecule();
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

void
Beam::brew_molecule()
{
    assert(left->line == right->line);
    Real inter=paper()->internote();
    Real sl = slope*inter;
    Real w =  width().length() + paper()->rule_thickness();
    Symbol s = paper()->lookup_->beam(sl,w);

    Atom a(s);
    
    Real dx = width().min -left->hpos;
    a.translate(Offset(dx,left_pos*inter));
    output = new Molecule(a);
}

void
Beam::print()const
{
    mtor << "Beam, slope " <<slope << "left ypos " << left_pos<<'\n';    
}


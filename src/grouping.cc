#include "debug.hh"
#include "grouping.hh"
#include "interval.hh"

void
Rhythmic_grouping::OK()const
{
     for (int i= 1; i < divisions.sz(); i++) {
	 Real dt = divisions[i] - divisions[i-1];
	 assert(dt>0);	 
     }
     Interval u;
     for (int i= 1; i < children.sz(); i++) {
	 children[i]->OK();
	 u.unite(children[i]->time());
     }
     
}

Real
Rhythmic_grouping::length() const
{
    return divisions.last() - divisions[0];
}

void
Rhythmic_grouping::split(Rhythmic_grouping r)
{
    svec<Real> ir = r.interior();
    split(ir);
    
    for (int i= 0; i < children.sz(); i++) {
	Rhythmic_grouping here(r.partial_grouping(children[i]->time()));
	if (here.divisions.sz() == 2)
	    here.split(2);
	children[i]->split(here);
    }
}

svec<Real>
Rhythmic_grouping::interior()
{
    svec<Real> r(divisions);
    r.del(0);
    r.pop();
    return r;
}

void
Rhythmic_grouping::split(int n)
{
    assert(divisions.sz() == 2 && children.sz() == 0);
    Real dt =(divisions.last()-divisions[0] )/n;
    svec<Real> r;
    for (int i= 0; i <= n; i++)
	r.add(divisions[0] +dt *i);
    divisions = r;
}

void
Rhythmic_grouping::intersect(Interval t)
{
    svec<Real> r;
    for  (int i=0; i < divisions.sz(); i++)
	if (t.elt_q(divisions[i]))
	    r.add(divisions[i]);
    if (r[0] > t.min )		// todo
	r.insert( t.min,0);
    if (r.last() < t.max)
	r.add(t.max);
    
    divisions = r;
    svec<Rhythmic_grouping*> nc;
    for (int i=0; i < children.sz(); i++) {
	Interval inter = intersection(t, children[i]->time());
	if (!inter.empty()) {
	    Rhythmic_grouping*p =new Rhythmic_grouping(*children[i]);
	    nc.add(p);
	    p->intersect(inter);
	}
	delete children[i];
    }
    children = nc;
}

Rhythmic_grouping
Rhythmic_grouping::partial_grouping(Interval t)
{
    Rhythmic_grouping r(*this);
    r.intersect(t);
    return r;
}

void
Rhythmic_grouping::split(svec<Real> splitpoints)
{
    assert(!children.sz());
    svec<Real> child;
    int j = 0, i=0;
    while (1) {
	if  ( i >= divisions.sz() || j >= splitpoints.sz())
	    break;
	
	child.add(divisions[i]);
	if (divisions[i] < splitpoints[j]) {
	    i++;
	} else if (divisions[i] > splitpoints[j]) {
	    j ++;	
	} else {
	    children.add(new Rhythmic_grouping(child));
	    child.set_size(1);
	    child[0] = divisions[i];
	}
    }
}

Rhythmic_grouping::Rhythmic_grouping(svec<Real> d)
    :divisions(d)
{
}
Rhythmic_grouping::Rhythmic_grouping()
{
}
Interval
Rhythmic_grouping::time()const
{
    return  Interval(divisions[0], divisions.last());
}
Rhythmic_grouping::Rhythmic_grouping(Interval h)
{
    divisions.add(h.min);
    divisions.add(h.max);
}

Rhythmic_grouping::~Rhythmic_grouping()
{
    for (int i=0; i < children.sz(); i++)
	delete children[i];
}


Rhythmic_grouping::Rhythmic_grouping(Rhythmic_grouping const&s)
{
    divisions = s.divisions;    
    for (int i=0; i < s.children.sz(); i++)
       children.add(new Rhythmic_grouping(*s.children[i]));
}
    
void
Rhythmic_grouping::print()const    
{
#ifndef NPRINT
    mtor << "{ ";
    for (int i=0; i < divisions.sz(); i++) {
	mtor << divisions[i] << ',';
    }
    for (int i=0; i < children.sz(); i++) {
	children[i]->print();
    }
    mtor << "}";
#endif
}



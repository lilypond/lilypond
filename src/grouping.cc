#include "grouping.hh"
#include "debug.hh"

Interval
vec_union(svec<Interval> notes)
{
    Interval u;
    u.set_empty();
    for (int i =0 ; i < notes.sz() ; i++) {
	u.unite(notes[i]);
    }
    return u;
}

svec<Real>
default_bounds(Interval t)
{
    svec<Real> bounds;
    Real dt = t.length();
    bounds.add(t.min);
    bounds.add(t.min + dt/2);    
    return bounds;
}

svec<Real>
Rhythmic_grouping::get_bounds()
{
    svec<Real> bounds;
    if (children.sz()) {
	for (int i=0; i < children.sz(); i++) {
	    bounds.add(children[i]->t.min);
	}
    } else {
	default_bounds(t);
    }
//	bounds.add(t.max );
    return bounds;
}

Real
Rhythmic_grouping::last()
{
    return t.max;
}

void
Rhythmic_grouping::split_grouping(svec<Real> bounds)
{
    int lasti =0;
    svec<Rhythmic_grouping*> newgrp;
    for (int i=0, j = 1; i < children.sz() && j < bounds.sz(); ) {
	if ( children[i]->t.max < bounds[j]) {
	    i ++;
	    continue;
	} else if (children[i]->t.max > bounds[j]) {
	    j ++;
	    continue;
	}

        assert( children[lasti]->t.min == bounds[j-1] );
	assert( children[i]->t.max == bounds[j] );

	Rhythmic_grouping * n = new Rhythmic_grouping(Interval(
	    bounds[j-1], bounds[j]));
	for (int k = lasti ; k < i; k++)
	    n->children.add(children[k]);
	newgrp.add(n);

	i = lasti = i+1;
    }
    if (newgrp.sz() <= 1) {
	newgrp[0]->children.set_size(0);
	delete newgrp[0];
	return;
    }
    children = newgrp;    
}

void
Rhythmic_grouping::split_half()
{
    svec<Real> bounds = default_bounds(t);
    bounds.add(t.max);
    split_grouping(bounds);
    
    for (int i=0; i < children.sz(); i++) {
	if (children[i]->children.sz())
	    children[i]->split_half();
    }
}

Rhythmic_grouping*
Rhythmic_grouping::sub_grouping(Interval v)
{
    return 0;			// todo!
}
void
Rhythmic_grouping::split_grouping(Rhythmic_grouping &initial_grouping)
{
    svec<Rhythmic_grouping*> newgrp;
    svec<Real> bounds = initial_grouping.get_bounds();
    bounds.add(initial_grouping.last());
    split_grouping(bounds);
    for (int i=0; i < children.sz(); i++) {
	Interval h = children[i]->t;
	Rhythmic_grouping*r = initial_grouping.sub_grouping(h);
	if (children[i]->children.sz()) {
	    if (r)
		children[i]->split_grouping(*r);
	    else
		children[i]->split_half();
	}
    }
}

Rhythmic_grouping::Rhythmic_grouping(Interval i)
{
    t=i;
}

Rhythmic_grouping::Rhythmic_grouping(svec<Interval> notes,
				svec<Real> initial_grouping)
{
    t = vec_union(notes);
    for (int i=0; i < notes.sz(); i++) {
	children.add(new Rhythmic_grouping(notes[i]));
    }
    split_grouping(initial_grouping);
}

Rhythmic_grouping::~Rhythmic_grouping()
{
    for (int i=0; i < children.sz(); i++) {
	delete children[i];
    }
}

void
Rhythmic_grouping::print()const    
{
    mtor << "{ " << t << "\n";
    for (int i=0; i < children.sz(); i++) {
	children[i]->print();
    }
    mtor << "}";
}



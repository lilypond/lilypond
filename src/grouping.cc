#include "debug.hh"
#include "grouping.hh"
#include "interval.hh"

void
Rhythmic_grouping::OK()const
{
    assert(bool(children.sz()) != bool(interval_));

    for (int i= 0; i < children.sz(); i++) {
	children[i]->OK();
	if (i>0)
	    assert(children[i-1]->interval().max ==
		   children[i]->interval().min);
    }
}

Real
Rhythmic_grouping::length() const
{
    return interval().length();
}

Interval
Rhythmic_grouping::interval()const
{
    if (interval_)
	return *interval_;
    else
	return
	    Interval(children[0]->interval().min,
		     children.last()->interval().max);
}

void
Rhythmic_grouping::split(Rhythmic_grouping r)
{
    if (interval_)
	return ;
    
    r.intersect(interval());
    split(r.intervals());
    
    for (int i= 0; i < children.sz(); i++) {
	if (!children[i]->interval_) {
	    Rhythmic_grouping here(r);	
	    children[i]->split(here);
	}
    }
}


svec<Interval>
Rhythmic_grouping::intervals()
{
    svec<Interval> r;
    if (interval_ || children.sz() == 1) {
	Interval i(interval());
	Interval r1(i), r2(i);
	r1.max = r2.min = i.center();
	r.add(r1); r.add(r2);
    } else {
	for (int i=0; i < children.sz(); i++)
	    r.add(children[i]->interval());
    }
    return r;
}

Rhythmic_grouping::Rhythmic_grouping(Interval t, int n)
{
    if (n == 1 || !n) {
	interval_ = new Interval(t);
	return;
    }
    Real dt = t.length()/n;
    Interval basic = Interval(t.min, t.min+dt);
    for (int i= 0; i < n; i++)
	children.add(new Rhythmic_grouping( dt*i + basic ));
}

void
Rhythmic_grouping::intersect(Interval t)
{
    if (interval_) {
	interval_->intersect(t);
	return;
    }
    
    for (int i=0; i < children.sz(); i++) {
	Interval inter = intersection(t, children[i]->interval());
	if (inter.empty() || inter.length() < 1e-8) {
	    delete children[i];
	    children[i] =0;
	} else {
	    children[i]->intersect(t);
	}
    }
    for (int i=0; i < children.sz(); ) {
	if (!children[i])
	    children.del(i);
	else
	    i++;
    }

}

void
Rhythmic_grouping::split(svec<Interval> splitpoints)
{
    //check on splitpoints..
    int j = 0, i=0, starti = 0, startj = 0;
    
    svec<Rhythmic_grouping*> ch;
    while (1) {
	if  ( i >= children.sz() || j >= splitpoints.sz())
	    break;
	
	assert( distance(
	    children[starti]->interval().min, splitpoints[startj].min)<1e-8);
		if (children[i]->interval().max < splitpoints[j].max - 1e-8) {
	    i ++;
	} else if (children[i]->interval().max > splitpoints[j].max + 1e-8) {
	    j ++;
	} else {

	    if (i == starti) {
		ch.add(children[i]);
	    } else {
		Rhythmic_grouping *newchild=new Rhythmic_grouping(
		    children.subvec(starti, i+1));

		ch.add(newchild);
	    }
	    i ++;
	    j++;
	    starti = i;
	    startj = j;


	}
    }
    if (ch.size() != 1)
	children = ch;
    }

Rhythmic_grouping::Rhythmic_grouping(svec<Rhythmic_grouping*> r)
    :children(r)
{
    interval_ =0;
}

Rhythmic_grouping::~Rhythmic_grouping()
{
    junk();
}

void
Rhythmic_grouping::copy(Rhythmic_grouping const&s)
{
    interval_ =  (s.interval_)? new Interval(*s.interval_) : 0;
    for (int i=0; i < s.children.sz(); i++)
       children.add(new Rhythmic_grouping(*s.children[i]));
}

void
Rhythmic_grouping::operator=(Rhythmic_grouping const &s)
{
    junk();
    copy(s);
}

Rhythmic_grouping::Rhythmic_grouping(Rhythmic_grouping const&s)
{
    copy(s);
}

void
Rhythmic_grouping::junk()
{
    delete interval_;
    interval_ = 0;
    for (int i=0; i < children.sz(); i++)
	delete children[i];
    children.set_size(0);
}

void
Rhythmic_grouping::print()const    
{
#ifndef NPRINT
    mtor << "{ \n";
    if (interval_)
	mtor<<" Interval "<< interval_->str();
    for (int i=0; i < children.sz(); i++) {
	children[i]->print();
    }
    mtor << "}\n";
#endif
}

void
Rhythmic_grouping::add_child(Real start, Real len)
{
    Real stop = start+len;
    for (int i=0; i < children.sz(); i ++) {
	Interval j=children[i]->interval();
	if (distance (j.min, start)<1e-8 && distance (j.max, stop)<1e-8) {
	    return;
	}
    }
    
    if (children.sz())
	assert ( distance(children.last()->interval().max , start) < 1e-8);

    children.add(new Rhythmic_grouping(Interval(start, stop)));
}

Rhythmic_grouping::Rhythmic_grouping()
{
    interval_ =0;
}

int
min_elt(svec<int> v)
{
    int i = 1000;		// ugh
    for (int j = 0 ; j <  v.sz(); j++)
	i = i <? v[j];
    return i;
}

svec<int>
Rhythmic_grouping::generate_beams(svec<int> flags, int &flagidx)
{
    
    assert (!interval_) ;
    
    svec< svec<int> > children_beams;
    for (int i=0; i < children.sz(); i++) {
	svec<int> child_beams;
	if (children[i]->interval_) {
	    int f = flags[flagidx++];
	    child_beams.add(f);
	} else {
	    child_beams = children[i]->
		generate_beams(flags, flagidx);
	}
	children_beams.add(child_beams);
    }
    svec<int> beams;
    int lastm, m, nextm;
    for (int i=0; i  < children_beams.sz(); i++) {
	bool add_left =  (i >0);
	bool add_right = (i  < children_beams.sz() -1);

	if (!i)
	    m =  min_elt(children_beams[i]);
	if (add_right)
	    nextm = min_elt(children_beams[i+1]);
	
	if (children_beams[i].sz() == 1) {
	    if (add_right)
		beams.add(m);
	    if (add_left)
		beams.add(m);
	} else {
	    if (add_left) 
		beams.add(lastm <? m);
	    beams.concat(children_beams[i]);
	    if (add_right)
		beams.add(m <? nextm);
	}
	lastm = m;
	m = nextm;	
    }
    assert(!(beams.sz()%2));
    return beams;
}


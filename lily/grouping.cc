/*
  grouping.cc -- implement Rhythmic_grouping

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "grouping.hh"
#include "interval.hh"

void
Rhythmic_grouping::init()
{
  interval_ = 0;
  children.clear();     
}

void
Rhythmic_grouping::OK() const
{
#ifndef NDEBUG
  assert (bool (children.size()) != bool (interval_));

  for (int i= 0; i < children.size(); i++) 
    {
      children[i]->OK();
      if (i>0)
	assert (children[i-1]->interval().right ==
		children[i]->interval().left);
    }
#endif
}

Moment
Rhythmic_grouping::length() const
{
  return interval().length ();
}

MInterval
Rhythmic_grouping::interval() const
{
  if (interval_)
    return *interval_;
  else
    return
      MInterval (children[0]->interval().left,
		 children.top()->interval ().right);
}

void
Rhythmic_grouping::split (Rhythmic_grouping r)
{
  if (interval_)
    return ;
  
  r.intersect (interval());
  split (r.intervals());
  
  for (int i= 0; i < children.size(); i++) 
    {
      if (!children[i]->interval_) 
	{
	  Rhythmic_grouping here (r);	
	  children[i]->split (here);
	}
    }
}


Array<MInterval>
Rhythmic_grouping::intervals()
{
  Array<MInterval> r;
  if (interval_ || children.size() == 1) 
    {
      MInterval i (interval());
      MInterval r1(i), r2(i);
      r1.right = r2.left = i.center();
      r.push (r1); r.push (r2);
    }
  else 
    {
      for (int i=0; i < children.size(); i++)
	r.push (children[i]->interval());
    }
  return r;
}

void
Rhythmic_grouping::intersect (MInterval t)
{
  if (interval_) 
    {
      interval_->intersect (t);
      return;
    }
  
  for (int i=0; i < children.size(); i++) 
    {
      MInterval inter = intersection (t, children[i]->interval());
      if (inter.empty_b() || inter.length () <= Rational (0)) 
	{
	  delete children[i];
	  children[i] =0;
	}
      else 
	{
	  children[i]->intersect (t);
	}
    }
  for (int i=0; i < children.size();) 
    {
      if (!children[i])
	children.del (i);
      else
	i++;
    }

}

/**
  Put our children in branches of #this#.
  The min and max time intervals coincide with elements of #splitpoints#

  I really should be documenting what is happening here, but I find
  that difficult, since I don't really understand what's going on here.

  */
void
Rhythmic_grouping::split (Array<MInterval> splitpoints)
{
  //check on splitpoints..
  int j = 0, i = 0, starti = 0, startj = 0;
  
  Array<Rhythmic_grouping*> ch;
  while (1) 
    {
      if  (i >= children.size() || j >= splitpoints.size ())
	break;
	
      assert (
	      children[starti]->interval().left== splitpoints[startj].left);
      if (children[i]->interval().right < splitpoints[j].right) 
	{
	  i ++;
	}
      else if (children[i]->interval().right > splitpoints[j].right) 
	{
	  j ++;
	}
      else 
	{

	  if (i == starti) 
	    {
	      ch.push (children[i]);
	    }
	  else 
	    {
	      Rhythmic_grouping *newchild=new Rhythmic_grouping (
								 children.slice (starti, i+1));

	      ch.push (newchild);
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


Rhythmic_grouping::Rhythmic_grouping (MInterval t, int n)
{
  init();
  if (n == 1 || !n) 
    {
      interval_ = new MInterval (t);
      return;
    }
  Moment dt = t.length()/Rational (n);
  MInterval basic = MInterval (t.left, t.left+dt);
  for (int i= 0; i < n; i++)
    children.push (new Rhythmic_grouping (dt*Rational (i) + basic));
}


Rhythmic_grouping::Rhythmic_grouping (Array<Rhythmic_grouping*> r)
  :children (r)
{
  interval_ =0;
}

Rhythmic_grouping::~Rhythmic_grouping()
{
  junk();    
}

void
Rhythmic_grouping::copy (Rhythmic_grouping const&s)
{
  interval_ =  (s.interval_)? new MInterval (*s.interval_) : 0;
  for (int i=0; i < s.children.size(); i++)
    children.push (new Rhythmic_grouping (*s.children[i]));
}

void
Rhythmic_grouping::operator=(Rhythmic_grouping const &s)
{
  junk();
  copy (s);
}

Rhythmic_grouping::Rhythmic_grouping (Rhythmic_grouping const&s)
{
  init();
  copy (s);
}

void
Rhythmic_grouping::junk()
{
  delete interval_;
  for (int i=0; i < children.size(); i++)
    delete children[i];
  init();
}

void
Rhythmic_grouping::print() const    
{
#ifndef NPRINT
  DOUT << "{ \n";
  if (interval_)
    DOUT <<" Interval "<< interval_->str();
  for (int i=0; i < children.size(); i++) 
    {
      children[i]->print();
    }
  DOUT << "}\n";
#endif
}

bool
Rhythmic_grouping::child_fit_b (Moment start)
{
  if (children.size())
    return (children.top()->interval ().right== start);

  return true;
}  

void
Rhythmic_grouping::add_child (Moment start, Moment len)
{
  Moment stop = start+len;
  assert (child_fit_b (start));
  children.push (new Rhythmic_grouping (MInterval (start, stop)));
}

Rhythmic_grouping::Rhythmic_grouping()
{
  interval_ =0;
}

int
min_elt (Array<int> v)
{
  int i = 1000;		// ugh
  for (int j = 0 ; j <  v.size(); j++)
    i = i <? v[j];
  return i;
}

Array<int>
Rhythmic_grouping::generate_beams (Array<int> flags, int &flagidx)
{
  assert (!interval_) ;
  
  Array< Array<int> > children_beams;
  for (int i=0; i < children.size(); i++) 
    {
      Array<int> child_beams;
      if (children[i]->interval_) 
	{
	  int f = flags[flagidx++];
	  child_beams.push (f);
	}
      else 
	{
	  child_beams = children[i]->
	    generate_beams (flags, flagidx);
	}
      children_beams.push (child_beams);
    }
  Array<int> beams;
  int lastm, m, nextm;
  for (int i=0; i  < children_beams.size(); i++) 
    {
      bool add_left =  (i >0);
      bool add_right = (i  < children_beams.size() -1);

      if (!i)
	m =  min_elt (children_beams[i]);
      if (add_right)
	nextm = min_elt (children_beams[i+1]);
	
      if (children_beams[i].size() == 1) 
	{
	  if (add_right)
	    beams.push (m);
	  if (add_left)
	    beams.push (m);
	}
      else 
	{
	  if (add_left) 
	    beams.push (lastm <? m);
	  beams.concat (children_beams[i]);
	  if (add_right)
	    beams.push (m <? nextm);
	}
      lastm = m;
      m = nextm;	
    }
  assert (!(beams.size()%2));
  return beams;
}

void
Rhythmic_grouping::translate (Moment m)
{
  if (interval_)
    *interval_ += m;
  else
    for (int i=0; i < children.size(); i++)
      children[i]->translate (m);
}

void
Rhythmic_grouping::extend (MInterval m) const
{    
  assert (m.left >= interval().left);
  while (m.right  >interval().right) 
    {
      Array<Rhythmic_grouping*> a (children);
      for (int i=0; i < a.size(); i++) 
	{
	  a[i] =new Rhythmic_grouping (*children[i]);
	  a[i]->translate (children.top()->interval ().right);	    
	}
      ((Rhythmic_grouping*)this)->children.concat (a);
    }
  assert (m.right <= interval().right);
  OK();
}

Rhythmic_grouping
parse_grouping (Array<int> beat_i_arr, Array<Moment> elt_length_arr)
{
  Moment here =0;
  assert (beat_i_arr.size() == elt_length_arr.size ());
  
  Array<Rhythmic_grouping*> children;
  for (int i=0; i < beat_i_arr.size(); i++) 
    {
      Moment last = here;
      here += elt_length_arr[i] * Moment (beat_i_arr[i]);
      children.push (
		     new Rhythmic_grouping (MInterval (last, here),
					    beat_i_arr[i]));
    }
  return Rhythmic_grouping (children);
}


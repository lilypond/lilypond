/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "stem.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "note-head.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "misc.hh"

const int STEMLEN=7;

int
Stem::min_head_i()const
{
  int m = 1000;
  for (int i =0; i < head_l_arr_.size(); i++)
	m = m <? head_l_arr_[i]->position_i_;
  return m;
}

int
Stem::max_head_i() const
{
  int m = -1000;
  for (int i =0; i < head_l_arr_.size(); i++)
	m = m >? head_l_arr_[i]->position_i_;
  return m;
  
}

Stem::Stem (int c) 
{
  beams_left_i_ = 0;
  beams_right_i_ = 0;

  stem_bottom_f_ = stem_top_f_ = 0;
  flag_i_ = 4;
  dir_i_ =0;
  staff_size_i_ = c;

  print_flag_b_=true;
  stem_xoffset_f_ =0;
}


IMPLEMENT_IS_TYPE_B1(Stem,Item);

void
Stem::do_print() const
{
#ifndef NPRINT
  DOUT << "flag "<< flag_i_ << " print_flag_b_ " << print_flag_b_;
#endif
}

Real 
Stem::stem_length_f()const
{
  return stem_top_f_-stem_bottom_f_ ;
}

Real
Stem::stem_start_f()const
{
  return (dir_i_ < 0)? stem_top_f_ : stem_bottom_f_;
}

Real
Stem::stem_end_f() const
{
  return (dir_i_ < 0)? stem_bottom_f_ : stem_top_f_;
}


void
Stem::set_stemend (Real se)
{
  // todo: margins
  if (!  ((dir_i_ > 0 && se >= max_head_i()) || 
	    (se <= min_head_i() && dir_i_ <0)) )	
 	warning ("Weird stem size; check for narrow beams");

  stem_top_f_  = (dir_i_ < 0) ? max_head_i()           : se;
  stem_bottom_f_  = (dir_i_ < 0) ? se  : min_head_i();
}

void
Stem::add (Note_head *n)
{
  n->add_dependency (this);
  if (n->rest_b_) 
    {
	rest_l_arr_.push (n);
    }
  else if (n->balltype_i_ == 0) 
    {
	whole_l_arr_.push (n);
	return;
    }
  else
    {
	head_l_arr_.push (n);
    }
}

bool
Stem::invisible_b()const
{
  return !head_l_arr_.size();
}

// if dir_i_ is set we return fake values.

int
Stem::get_center_distance_from_top()
{
  if (dir_i_)
	return (dir_i_ > 0) ? 0 : 1;

  int staff_center = staff_size_i_ / 2;
  int max = max_head_i() - staff_center;
  return max >? 0;
}

// if dir_i_ is set we return fake values.
int
Stem::get_center_distance_from_bottom()
{
  if (dir_i_)
	return (dir_i_ > 0) ? 1 : 0;

  int staff_center = staff_size_i_ / 2;
  int min = staff_center - min_head_i();
  return min >? 0;
}

int
Stem::get_default_dir()
{
  if (dir_i_)
	return dir_i_;
  return (get_center_distance_from_top() >=
	get_center_distance_from_bottom()) ? -1 : 1;
}


void
Stem::set_default_dir()
{
  dir_i_ = get_default_dir();
}

void
Stem::set_default_stemlen()
{
  if (!dir_i_)
	set_default_dir();

  
  // ugh... how about non 5-line staffs?
  if ((max_head_i() < -2 && dir_i_ == 1)
	||(min_head_i() > staff_size_i_ && dir_i_ == -1))
	  {
	set_stemend (staff_size_i_ /2 -1);
    }
  else 
    {
	set_stemend ((dir_i_ > 0) ? max_head_i() + STEMLEN : 
				     min_head_i() - STEMLEN);

    }
}


void
Stem::set_default_extents()
{
  if (!stem_length_f())
	set_default_stemlen();

  set_stemend ((dir_i_< 0) ? 
		max_head_i()-stem_length_f (): min_head_i () +stem_length_f ());
  if (dir_i_ > 0){	
	stem_xoffset_f_ = paper()->note_width ()-paper ()->rule_thickness ();
    }
  else
	stem_xoffset_f_ = 0;
}

/*
  TODO
  
  move into note_column.cc

  */
void
Stem::set_noteheads()
{
  if (!head_l_arr_.size())
	return;
  head_l_arr_.sort (Note_head::compare);
  if (dir_i_ < 0) 
	head_l_arr_.reverse();
  
  head_l_arr_[0]->extremal_i_ = -1;
  head_l_arr_.top()->extremal_i_ = 1;
  int parity=1;
  int lastpos = head_l_arr_[0]->position_i_;
  for (int i=1; i < head_l_arr_.size(); i ++) 
    {
	int dy =abs (lastpos- head_l_arr_[i]->position_i_);
	
	if (dy <= 1) 
	  {
	    if (parity)
		head_l_arr_[i]->x_dir_i_ = (stem_xoffset_f_>0) ? 1:-1;
	    parity = !parity;
	  }
	else
	    parity = 0;
	lastpos = head_l_arr_[i]->position_i_;
    }
}

void
Stem::do_pre_processing()
{
  if (stem_bottom_f_== stem_top_f_)
	set_default_extents();
  set_noteheads();
  flag_i_ = dir_i_*abs (flag_i_);
  transparent_b_ = invisible_b();
  empty_b_ = invisible_b();
}


Interval
Stem::do_width()const
{
  if (!print_flag_b_ || abs (flag_i_) <= 4)
	return Interval (0,0);	// TODO!
  Paper_def*p= paper();
  Interval r (p->lookup_l()->flag (flag_i_).dim.x ());
  r+= stem_xoffset_f_;
  return r;
}

Molecule*
Stem::brew_molecule_p()const 
{
  Molecule *out =0;
      
  Real bot  = stem_bottom_f_;
  Real top = stem_top_f_;
  
  assert (bot!=top);
 
  Paper_def *p =paper();

  Real dy = p->internote_f();
  Symbol ss =p->lookup_l()->stem (bot*dy,top*dy);

  out = new Molecule (Atom (ss));

  if (print_flag_b_&&abs (flag_i_) > 4)
    {
	Symbol fl = p->lookup_l()->flag (flag_i_);
	Molecule m (fl);
	if (flag_i_ < -4){		
	    out->add_bottom (m);
	  }
	else if (flag_i_ > 4) 
	  {
	    out->add_top (m);
	  }
	else
	    assert (false); 
    }

  out->translate (stem_xoffset_f_, X_AXIS);
  return out;
}

Real
Stem::hpos_f()const
{
  return Item::hpos_f() + stem_xoffset_f_;
}


void
Stem::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  Item * o_l = o->item();
  Item * n_l = n? n->item():0;
  whole_l_arr_.substitute ((Note_head*)o_l, (Note_head*)n_l);
  head_l_arr_.substitute ((Note_head*)o_l, (Note_head*)n_l);
  rest_l_arr_.substitute ((Note_head*)o_l, (Note_head*)n_l);
}

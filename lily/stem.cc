/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  TODO: This is way too hairy
*/

#include "stem.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "note-head.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "misc.hh"
#include "beam.hh"
#include "rest.hh"

Stem::~Stem ()
{
}

Stem::Stem ()
{
  beams_i_drul_[LEFT] = beams_i_drul_[RIGHT] = -1;
  mult_i_ = 0;

  yextent_drul_[DOWN] = yextent_drul_[UP] = 0;
  flag_i_ = 2;
  dir_ = CENTER;
  stem_xdir_ = LEFT;

  beam_gap_i_ = 0;
  beam_l_ = 0;
}

Interval_t<int>
Stem::head_positions () const
{
  /* 
    Mysterious FreeBSD fix by John Galbraith.  Somehow, the empty intervals 
    trigger FP exceptions on FreeBSD.  Fix: do not return infinity 

   */
  if (!head_l_arr_.size ())
    {
      return Interval_t<int> (100,-100);	
    }

  Interval_t<int> r;
  for (int i =0; i < head_l_arr_.size (); i++)
    {
      int p = head_l_arr_[i]->position_i_;
      r[BIGGER] = r[BIGGER] >? p;
      r[SMALLER] = r[SMALLER] <? p;
    }
  return r;
}

void
Stem::do_print () const
{
#ifndef NPRINT
  DOUT << "flag "<< flag_i_;
  if (beam_l_)
    DOUT << "beamed";
#endif
}

Real
Stem::stem_length_f () const
{
  return yextent_drul_[UP]-yextent_drul_[DOWN] ;
}

Real
Stem::stem_begin_f () const
{
  return yextent_drul_[Direction(-dir_)];
}

Real
Stem::chord_start_f () const
{
  return head_positions()[dir_] * staff_line_leading_f ()/2.0;
}

Real
Stem::stem_end_f () const
{
  return yextent_drul_[dir_];
}

void
Stem::set_stemend (Real se)
{
  // todo: margins
  if (dir_ && dir_ * head_positions()[dir_] >= se*dir_)
    warning (_ ("weird stem size; check for narrow beams"));

  
  yextent_drul_[dir_]  =  se;
  yextent_drul_[Direction(-dir_)] = head_positions()[-dir_];
}

int
Stem::type_i () const
{
  return head_l_arr_[0]->balltype_i_;
}

void
Stem::add_head (Rhythmic_head *n)
{
  n->add_dependency (this);	// ?
  if (Note_head *nh = dynamic_cast<Note_head *> (n))
    {
      head_l_arr_.push (nh);
    }
  else if (Rest *r = dynamic_cast<Rest *> (n))
    {
      rest_l_arr_.push (r);
    }
}

bool
Stem::invisible_b () const
{
  return (!head_l_arr_.size () ||
    head_l_arr_[0]->balltype_i_ <= 0);
}

int
Stem::get_center_distance (Direction d) const
{
  int staff_center = 0;
  int distance = d*(head_positions()[d] - staff_center);
  return distance >? 0;
}

Direction
Stem::get_default_dir () const
{
  return (get_center_distance (UP) >
	  get_center_distance (DOWN)) 
    ? DOWN 
    : UP;
}

Direction
Stem::get_dir () const
{
  return dir_;
}

void
Stem::set_default_dir ()
{
  dir_ = get_default_dir ();
}

void
Stem::set_default_stemlen ()
{
  Real internote_f = staff_line_leading_f ()/2.0;
  Real length_f = paper_l ()->get_var ("stem_length0") / internote_f;
  Real shorten_f = paper_l ()->get_var ("forced_stem_shorten0") / internote_f;

  if (!dir_)
    set_default_dir ();
  /* 
    stems in unnatural (forced) direction should be shortened, 
    accoding to [Roush & Gourlay]
   */
  if (((int)chord_start_f ())
      && (dir_ != get_default_dir ()))
    length_f -= shorten_f;

  if (flag_i_ >= 5)
    length_f += 2.0;
  if (flag_i_ >= 6)
    length_f += 1.0;
  
  set_stemend ((dir_ > 0) ? head_positions()[BIGGER] + length_f:
	       head_positions()[SMALLER] - length_f);

  if (dir_ * stem_end_f () < 0)
    set_stemend (0);
}

//xxx
void
Stem::set_default_extents ()
{
  if (!stem_length_f ())
    set_default_stemlen ();


  if (dir_ == UP)
    stem_xdir_ = RIGHT;
  if (invisible_b ())
    stem_xdir_ = CENTER;
}

/*
  TODO

  move into note_column.cc

  */
void
Stem::set_noteheads ()
{
  if (!head_l_arr_.size ())
    return;
  head_l_arr_.sort (Note_head::compare);
  if (dir_ < 0)
    head_l_arr_.reverse ();

  head_l_arr_[0]->extremal_i_ = -1;
  head_l_arr_.top ()->extremal_i_ = 1;
  int parity=1;
  int lastpos = head_l_arr_[0]->position_i_;
  for (int i=1; i < head_l_arr_.size (); i ++)
    {
      int dy =abs (lastpos- head_l_arr_[i]->position_i_);

      if (dy <= 1)
	{
	  if (parity)
	    head_l_arr_[i]->x_dir_ = (stem_xdir_ == LEFT) ? LEFT : RIGHT;
	  parity = !parity;
	}
      else
	parity = 1;
      lastpos = head_l_arr_[i]->position_i_;
    }
}

void
Stem::do_pre_processing ()
{
  if (yextent_drul_[DOWN]== yextent_drul_[UP])
    set_default_extents ();
  set_noteheads ();
  flag_i_ = flag_i_;
  if (invisible_b ())
    {
      set_elt_property (transparent_scm_sym, SCM_BOOL_T);
    }
  set_empty (invisible_b ());
}


Interval
Stem::do_width () const
{
  Interval r (0, 0);
  if (beam_l_ || abs (flag_i_) <= 2)
    ;	// TODO!
  else
    {
      r = lookup_l ()->flag (flag_i_, dir_).dim_.x ();
      r += note_delta_f ();
    }
  return r;
}




const Real ANGLE = 20* (2.0*M_PI/360.0); // ugh!

Molecule*
Stem::do_brew_molecule_p () const
{
  Molecule *mol_p =new Molecule;
  Drul_array<Real> stem_y = yextent_drul_;
  Real dy = staff_line_leading_f ()/2.0;

  Real head_wid = 0;
  if (head_l_arr_.size ())
    head_wid = head_l_arr_[0]->extent (X_AXIS).length ();
  stem_y[Direction(-dir_)] += dir_ * head_wid * tan(ANGLE)/(2*dy);
  
  if (!invisible_b ())
    {
      Molecule ss =lookup_l ()->stem (stem_y[DOWN]*dy,
				     stem_y[UP]*dy);
      mol_p->add_molecule (ss);
    }

  if (!beam_l_ && abs (flag_i_) > 2)
    {
      Molecule fl = lookup_l ()->flag (flag_i_, dir_);
      fl.translate_axis(stem_y[dir_]*dy, Y_AXIS);
      mol_p->add_molecule (fl);
    }

  if (head_l_arr_.size())
    {
      mol_p->translate_axis (note_delta_f (), X_AXIS);
    }
  return mol_p;
}

Real
Stem::note_delta_f () const
{
  Real r=0;
  if (head_l_arr_.size())
    {
      Interval head_wid(0,  head_l_arr_[0]->extent (X_AXIS).length ());
      Real rule_thick(paper_l ()->rule_thickness ());
      Interval stem_wid(-rule_thick/2, rule_thick/2);
      if (stem_xdir_ == CENTER)
	r = head_wid.center ();
      else
	r = head_wid[stem_xdir_] - stem_wid[stem_xdir_];
    }
  return r;
}

Real
Stem::hpos_f () const
{
  return note_delta_f () + Item::hpos_f ();
}

/*
  TODO:  head_l_arr_/rest_l_arr_ in  
 */
void
Stem::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (Note_head*h=dynamic_cast<Note_head*> (o))
  head_l_arr_.substitute (h, dynamic_cast<Note_head*>(n));
  if (Rest *r=dynamic_cast<Rest*> (o))
    rest_l_arr_.substitute (r, dynamic_cast<Rest*>(n));
  if (Beam* b = dynamic_cast<Beam*> (o))
    {
      if (b == beam_l_) 
	{
	  beam_l_ = dynamic_cast<Beam*> (n);
	  if (!beam_l_)
	    {
	      beams_i_drul_[LEFT] = 0;
	      beams_i_drul_[RIGHT] = 0;
	      mult_i_ = 0;
	    }
	}
    }
  Staff_symbol_referencer::do_substitute_element_pointer (o,n);
      
}

/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO: This is way too hairy
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
#include "beam.hh"
#include "rest.hh"

const int STEMLEN = 7;

IMPLEMENT_IS_TYPE_B1 (Stem,Item);

Stem::Stem ()
{
  /*
    TODO: staff-size
    */
  beam_l_ = 0;
  beams_left_i_ = 0;
  beams_right_i_ = 0;
  mult_i_ = 0;

  yextent_drul_[DOWN] = yextent_drul_[UP] = 0;
  flag_i_ = 2;
  dir_ = CENTER;
  stem_xdir_ = LEFT;
  staff_size_i_ = 8;

  beam_gap_i_ = 0;
}

Interval_t<int>
Stem::head_positions () const
{
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
  DOUT << "flag "<< flag_i_ ;
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
  return head_positions()[dir_] * paper ()->internote_f ();
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
    warning (_("Weird stem size; check for narrow beams"));

  
  yextent_drul_[dir_]  =  se;
  yextent_drul_[Direction(-dir_)] = head_positions()[-dir_];
}

int
Stem::type_i () const
{
  return head_l_arr_[0]->balltype_i_;
}

void
Stem::add (Rhythmic_head *n)
{
  n->add_dependency (this);	// ?
  if (n->is_type_b (Note_head::static_name ()))
    {
      head_l_arr_.push ((Note_head*)n);
    }
  else if (n->is_type_b (Rest::static_name ()))
    {
      rest_l_arr_.push ((Rest*)n);
    }
}

bool
Stem::invisible_b () const
{
  return (!head_l_arr_.size () ||
    head_l_arr_[0]->balltype_i_ <= 0);
}

int
Stem::get_center_distance (Direction d)
{
  int staff_center = 0;
  int distance = d*(head_positions()[d] - staff_center);
  return distance >? 0;
}

Direction
Stem::get_default_dir ()
{
  return (get_center_distance (UP) >
	  get_center_distance (DOWN)) 
    ? DOWN 
    : UP;
}

Direction
Stem::get_dir ()
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
  Real len = STEMLEN;
  Real dy = paper ()->interbeam_f ();

  if (!dir_)
    set_default_dir ();

  /* If the stem points in the "wrong" direction, it should be
     shortened, according to [Roush & Gourlay].  Their suggestion to knock off
     a whole staffspace is a bit drastical though.
     */
  else if (dir_ != get_default_dir ())
    len  -= 1.0;

  if (flag_i_ >= 5)
    len += 2.0;
  if (flag_i_ >= 6)
    len += 1.0;
  
  set_stemend ((dir_ > 0) ? head_positions()[BIGGER] + len :
	       head_positions()[SMALLER] - len);


  if (dir_ * stem_end_f () < 0)
    {
      set_stemend (0);
    }
}

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
  transparent_b_ = invisible_b ();
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
      Paper_def*p= paper ();
      r = p->lookup_l ()->flag (flag_i_, dir_).dim_.x ();
      r += note_delta_f ();
    }
  return r;
}




const Real ANGLE = 20* (2.0*M_PI/360.0); // ugh!

Molecule*
Stem::brew_molecule_p () const
{
  Molecule *mol_p =new Molecule;
  Paper_def *p =paper ();
  Drul_array<Real> stem_y = yextent_drul_;
  Real dy = p->internote_f ();
  

  Real head_wid = 0;
  if (head_l_arr_.size ())
    head_wid = head_l_arr_[0]->width ().length ();
  stem_y[Direction(-dir_)] += dir_ * head_wid * tan(ANGLE)/(2*dy);
  
  if (!invisible_b ())
    {
      Atom ss =p->lookup_l ()->stem (stem_y[DOWN]*dy,
				     stem_y[UP]*dy);
      mol_p->add (Atom (ss));
    }

  if (!beam_l_ &&abs (flag_i_) > 2)
    {
      Atom fl = p->lookup_l ()->flag (flag_i_, dir_);
      fl.translate_axis(stem_y[dir_]*dy, Y_AXIS);
      mol_p->add(fl);
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
      Interval head_wid(0,  head_l_arr_[0]->width ().length ());
      Real rule_thick(paper ()->rule_thickness ());
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
  return note_delta_f () +Item::hpos_f ();
}

/*
  TODO:  head_l_arr_/rest_l_arr_ in  do_substitute_dependent ()
 */
void
 Stem::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  Item * o_l = o->item ();
  Item * n_l = n? n->item () : 0;
  head_l_arr_.substitute ((Note_head*)o_l, (Note_head*)n_l);
  rest_l_arr_.substitute ((Rest*)o_l, (Rest*)n_l);
}

/*
  commandrequest.cc -- implement Nonmusical reqs

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"

void
Command_script_req::do_print() const
{}
Command_script_req::Command_script_req(){}
Command_script_req::~Command_script_req(){}

IMPLEMENT_IS_TYPE_B1(Command_script_req,Command_req);


IMPLEMENT_IS_TYPE_B1(Cadenza_req, Timing_req);

void
Cadenza_req::do_print() const
{
#ifndef NPRINT
  DOUT << on_b_;
#endif
}

bool
Cadenza_req::do_equal_b (Request*r) const
{
  Cadenza_req*cad =  r->access_Command_req ()->access_Timing_req ()->access_Cadenza_req ();

  return cad->on_b_ == on_b_;
}

Cadenza_req::Cadenza_req (bool b)
{
  on_b_ =b;
}



bool
Bar_req::do_equal_b (Request*r) const
{
  Bar_req * b = r->access_Command_req ()->access_Bar_req ();
  return type_str_ == b->type_str_;
}


IMPLEMENT_IS_TYPE_B1(Bar_req,Command_req);

IMPLEMENT_IS_TYPE_B2(Bracket_req, Command_req, Span_req);

void
Bar_req::do_print() const
{
  DOUT << type_str_;
}

void
Bracket_req::do_print () const
{
  Span_req::do_print ();
}

Bar_req::Bar_req (String s)
{
  type_str_ = s;
}

Partial_measure_req::Partial_measure_req (Moment m)
{
  duration_ =m;
}

bool
Partial_measure_req::do_equal_b (Request* r) const
{
  Partial_measure_req *p = r->access_Command_req ()->access_Timing_req ()->access_Partial_measure_req ();

  return p->duration_ == duration_;
}


IMPLEMENT_IS_TYPE_B1(Timing_req,Command_req);

void
Timing_req::do_print() const{}


IMPLEMENT_IS_TYPE_B1(Command_req,Request);

void
Command_req::do_print() const{}

IMPLEMENT_IS_TYPE_B1(Barcheck_req,Timing_req);

void
Barcheck_req::do_print() const{}

bool
Barcheck_req::do_equal_b (Request*) const
{
  return true;
}


IMPLEMENT_IS_TYPE_B1(Clef_change_req,Command_req);

void
Clef_change_req::do_print() const
{
#ifndef NPRINT
  DOUT << clef_str_ ;
#endif
}

Clef_change_req::Clef_change_req (String s)
{
  clef_str_ = s;
}




IMPLEMENT_IS_TYPE_B1(Partial_measure_req,Timing_req);

void
Partial_measure_req::do_print() const
{
  DOUT << duration_;
}


IMPLEMENT_IS_TYPE_B1(Time_signature_change_req,Timing_req);

void
Time_signature_change_req::do_print() const
{
#ifndef NPRINT
  DOUT << beats_i_ << "/" << one_beat_i_;
#endif
}

bool
Time_signature_change_req::do_equal_b (Request * r) const
{
  Time_signature_change_req * m = r->access_Command_req ()->access_Timing_req ()->access_Time_signature_change_req ();

  return m->beats_i_ == beats_i_
    && one_beat_i_ == m->one_beat_i_;
}

Time_signature_change_req::Time_signature_change_req()
{
  beats_i_ = 0;
  one_beat_i_ =0;
}


Tempo_req::Tempo_req()
{
  metronome_i_ = 60;
  dur_. durlog_i_ = 2;
}

void
Tempo_req::do_print() const
{
  DOUT << dur_.str() << " = " << metronome_i_;
}
IMPLEMENT_IS_TYPE_B1(Tempo_req, Timing_req);

bool
Tempo_req::do_equal_b (Request *r) const
{
  Tempo_req *t = r->access_Command_req ()->access_Timing_req ()->access_Tempo_req ();

  return t->dur_.length()== dur_.length () && metronome_i_ == t->metronome_i_;
}




IMPLEMENT_IS_TYPE_B1(Measure_grouping_req,Timing_req);

void
Measure_grouping_req::do_print() const
{
  for (int i=0; i < elt_length_arr_.size(); i++)
    {
      DOUT << beat_i_arr_[i] << "*" << elt_length_arr_[i].str () << " ";
    }
}


bool
Measure_grouping_req::do_equal_b (Request*) const
{
  return false;		// todo
}

void
Key_change_req::transpose (Musical_pitch d) 
{
  if (ordinary_key_b_ ) 
    { 
      if (pitch_arr_.size () > 0) 
        pitch_arr_[0].transpose(d);
      else
        {
          warning(_ ("don't know how handle empty keys")); // TODO 
        }
    }
  else
    {
      Array<Musical_pitch> old_pitch_arr_;
      for (int i = 0; i < pitch_arr_.size(); i++)
        {
          old_pitch_arr_.push(pitch_arr_[i]);
        }
      // set accidentals for \key d (as in Key_engraver::read_req)
      // (later called "new accidentals")
      int p = d.semitone_pitch ();
      /* Solve the equation 7*no_of_acc mod 12 = p, -6 <= no_of_acc <= 5 */
      int no_of_acc = (7*p) % 12;
      no_of_acc = (no_of_acc + 18) % 12 -6;

      /* Correct from flats to sharps or vice versa */
      if (no_of_acc * d.accidental_i_ < 0)
      no_of_acc += 12 * sign (d.accidental_i_);
    
      pitch_arr_.clear ();
      if (no_of_acc < 0) 
        {
	  int accidental = 6 ; // First accidental: bes
          for ( ; no_of_acc < 0 ; no_of_acc++ ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = -1;
	      m.notename_i_ = accidental;
	      pitch_arr_.push(m);
     
 	      accidental = (accidental + 3) % 7 ;
	    }
	}
      else 
	{ 
	  int accidental = 3 ; // First accidental: fis
	  for ( ; no_of_acc > 0 ; no_of_acc-- ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = 1;
	      m.notename_i_ = accidental;
	      pitch_arr_.push(m);
   
	      accidental = (accidental + 4) % 7 ;
	    }
	}
      // Check if transposed old accidentals and the new ones coincide
      no_of_acc = pitch_arr_.size();
      int acc_found;
      Musical_pitch mm;
      for (int i=0; i < old_pitch_arr_.size(); i++)
        {
          acc_found = 0;
          mm = old_pitch_arr_[i];
	  mm.transpose(d);
          for (int j=0; ((j < no_of_acc) && (acc_found == 0)); j++)
            {
              if (pitch_arr_[j].notename_i_ == mm.notename_i_)
                {
                  if (mm.accidental_i_ == 0)
                    {
                      // remove new accidental 
                      pitch_arr_.del(j);
                      no_of_acc--;
                      acc_found = 1;
	            }
		  else
                    {
                      // change new accidental 
                      pitch_arr_[j].accidental_i_ = mm.accidental_i_;
                      acc_found = 1;
		    }
                }
            }
          if (acc_found == 0)
            {
              // add transposed old accidental 
	      pitch_arr_.push(mm);
            }
        }
    }
}

IMPLEMENT_IS_TYPE_B1(Key_change_req,Command_req);
void
Key_change_req::squash_octaves()
{
  for (int i=0; i < pitch_arr_.size(); i++)
    {
      pitch_arr_[i].octave_i_ = 0;
    }
}

void
Key_change_req::do_print() const
{
#ifndef NPRINT
  for (int i=0; i < pitch_arr_.size(); i++)
    {
      pitch_arr_[i].print();
    }
#endif
}

Key_change_req::Key_change_req()
{
  minor_b_ = false;
  ordinary_key_b_= false;
}

IMPLEMENT_IS_TYPE_B1(Break_req, Command_req);

Break_req::Break_req ()
{
  penalty_i_ = 0;
}

void
Break_req::do_print () const
{
}

IMPLEMENT_IS_TYPE_B1 (Mark_req, Command_req);

Mark_req::Mark_req (String s)
{
  str_ = s;
}

void
Mark_req::do_print () const
{
  DOUT << str_;
}

int
Key_change_req::flats_i()
{
  int flats_i = 0;
  for (int i = 0; i < pitch_arr_.size(); i++)
    {
      if (pitch_arr_[i].accidental_i_ < 0)
	flats_i -= pitch_arr_[i].accidental_i_;
    }
  return flats_i;
}

bool
Key_change_req::minor_b() const
{
  return minor_b_;
}

int
Key_change_req::sharps_i()
{
  int sharps_i = 0;
  for (int i = 0; i < pitch_arr_.size(); i++)
    {
      if (pitch_arr_[i].accidental_i_ > 0)
	sharps_i += pitch_arr_[i].accidental_i_;
    }
  return sharps_i;
}

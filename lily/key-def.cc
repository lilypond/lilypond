/*   
  key-def.cc --  implement Key_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "key-def.hh"
#include "debug.hh"

Key_def::Key_def ()
{
  modality_i_ = 0;
  ordinary_key_b_ = false;
}
 
int
Key_def::ordinary_accidentals_i () const
{
  if (!ordinary_key_b_) 
    {
      programming_error ("Accidentals requested for non-conventional key");
      return 0;
    }

  int p;
  if (pitch_arr_.size () < 1) 
    {
      warning (_ ("No key name: assuming `C'"));
      p = 0;
    }
  else
    {
      p = pitch_arr_[0].semitone_pitch ();
      p += modality_i_;
    }
  /* Solve the equation 7*accidentals_i mod 12 = p, -6 <= accidentals_i <= 5 */
  int accidentals_i = (7*p) % 12;
  accidentals_i = (accidentals_i + 18) % 12 -6;
  
  /* Correct from flats to sharps or vice versa */
  if (accidentals_i * pitch_arr_[0].accidental_i_ < 0)
    accidentals_i += 12 * sign (pitch_arr_[0].accidental_i_);
  return accidentals_i;
}

int
Key_def::flats_i () const
{
  if (ordinary_key_b_) 
    return 0 >? -ordinary_accidentals_i ();
  int flats_i = 0;
  for (int i = 0; i < pitch_arr_.size (); i++)
    {
      if (pitch_arr_[i].accidental_i_ < 0)
	flats_i -= pitch_arr_[i].accidental_i_;
    }
  return flats_i;
}

bool
Key_def::minor_b () const
{
  return modality_i_ == 3;
}

int
Key_def::sharps_i () const
{
  if (ordinary_key_b_) 
    return 0 >? ordinary_accidentals_i ();
  int sharps_i = 0;
  for (int i = 0; i < pitch_arr_.size (); i++)
    {
      if (pitch_arr_[i].accidental_i_ > 0)
	sharps_i += pitch_arr_[i].accidental_i_;
    }
  return sharps_i;
}

void
Key_def::transpose (Musical_pitch d) 
{
  if (ordinary_key_b_ ) 
    { 
      if (pitch_arr_.size () > 0) 
        pitch_arr_[0].transpose (d);
      else
        {
          warning (_ ("don't know how handle empty keys")); // TODO 
        }
    }
  else
    {
      Array<Musical_pitch> old_pitch_arr_;
      for (int i = 0; i < pitch_arr_.size (); i++)
        {
          old_pitch_arr_.push (pitch_arr_[i]);
        }
      // set accidentals for \key d (as in Key_engraver::read_req)
      // (later called "new accidentals")
      int p = d.semitone_pitch ();
      /* Solve the equation 7*accidentals_i mod 12 = p, -6 <= accidentals_i <= 5 */
      int accidentals_i = (7*p) % 12;
      accidentals_i = (accidentals_i + 18) % 12 -6;

      /* Correct from flats to sharps or vice versa */
      if (accidentals_i * d.accidental_i_ < 0)
      accidentals_i += 12 * sign (d.accidental_i_);
    
      pitch_arr_.clear ();
      if (accidentals_i < 0) 
        {
	  int accidental = 6 ; // First accidental: bes
          for ( ; accidentals_i < 0 ; accidentals_i++ ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = -1;
	      m.notename_i_ = accidental;
	      pitch_arr_.push (m);
     
 	      accidental = (accidental + 3) % 7 ;
	    }
	}
      else 
	{ 
	  int accidental = 3 ; // First accidental: fis
	  for ( ; accidentals_i > 0 ; accidentals_i-- ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = 1;
	      m.notename_i_ = accidental;
	      pitch_arr_.push (m);
   
	      accidental = (accidental + 4) % 7 ;
	    }
	}
      // Check if transposed old accidentals and the new ones coincide
      accidentals_i = pitch_arr_.size ();
      int acc_found;
      Musical_pitch mm;
      for (int i=0; i < old_pitch_arr_.size (); i++)
        {
          acc_found = 0;
          mm = old_pitch_arr_[i];
	  mm.transpose (d);
          for (int j=0; ( (j < accidentals_i) && (acc_found == 0)); j++)
            {
              if (pitch_arr_[j].notename_i_ == mm.notename_i_)
                {
                  if (mm.accidental_i_ == 0)
                    {
                      // remove new accidental 
                      pitch_arr_.del (j);
                      accidentals_i--;
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
	      pitch_arr_.push (mm);
            }
        }
    }
}

void
Key_def::squash_octaves ()
{
  for (int i=0; i < pitch_arr_.size (); i++)
    {
      pitch_arr_[i].octave_i_ = 0;
    }
}

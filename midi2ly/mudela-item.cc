//
// mudela-item.cc -- implement Mudela_item
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include <assert.h>
#include "midi2ly-global.hh"
#include "string-convert.hh"
#include "duration-convert.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-stream.hh"
#include "mudela-score.hh"

Mudela_item::Mudela_item (Mudela_column* mudela_column_l)
{
  mudela_column_l_ = mudela_column_l;
}

Mudela_item::~Mudela_item ()
{
}

Moment
Mudela_item::at_mom ()
{
  return mudela_column_l_->at_mom ();
}

Moment
Mudela_item::duration_mom ()
{
  return Moment (0);
}

void
Mudela_item::output (Mudela_stream& mudela_stream_r)
{
  mudela_stream_r << str () << " ";
}

Mudela_key::Mudela_key (int accidentals_i, int minor_i)
  : Mudela_item (0)
{
  accidentals_i_ = accidentals_i;
  minor_i_ = minor_i;
}

char const *accname[] = {"eses", "es", "", "is" , "isis"};

String
Mudela_key::str ()
{
  int key_i = accidentals_i_ >= 0
    ? ((accidentals_i_ % 7) ["cgdaebf"] - 'a' - 2 -2 * minor_i_ + 7) % 7
    : ((-accidentals_i_ % 7) ["cfbeadg"] - 'a' - 2 -2 * minor_i_ + 7) % 7;
  
  String notename_str = !minor_i_
    ? to_str ((char) ((key_i + 2) % 7 + 'A'))
    : to_str ((char) ((key_i + 2) % 7 + 'a'));

  // fis cis gis dis ais eis bis
  static int sharps_i_a [7] = { 2, 4, 6, 1, 3, 5, 7 };
  // bes es as des ges ces fes
  static int flats_i_a [7] = { 6, 4, 2, 7, 5, 3, 1 };
  int accidentals_i = accidentals_i_ >= 0
			      ? sharps_i_a [key_i] <= accidentals_i_ ? 1 : 0
			      : flats_i_a [key_i] <= -accidentals_i_ ? -1 : 0;
			       
  if (accidentals_i)
    notename_str += String (accname [accidentals_i + 2]);

  return "\\key " + notename_str  + (minor_i_ ? "\\minor" : "") + ";\n";
}

String
Mudela_key::notename_str (int pitch_i)
{
  // this may seem very smart,
  // but it-s only an excuse not to read a notename table

  // major scale: do-do
  // minor scale: la-la  (= + 5)
  static int notename_i_a [12] = { 0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6 };
  int notename_i = notename_i_a [pitch_i % 12];

  static int accidentals_i_a [12] = { 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0 };
  int accidental_i = accidentals_i_a [(minor_i_ * 5 + pitch_i) % 12];
  if (accidental_i &&  (accidentals_i_ < 0))
    {
      accidental_i *= -1;
      notename_i =  (notename_i + 1) % 7;
    }

  String notename_str = to_str ((char)(((notename_i + 2) % 7) + 'a'));
  if (accidental_i)
    notename_str += String (accname [accidental_i + 2]);

  /*
    By tradition, all scales now consist of a sequence of 7 notes each
    with a distinct name, from amongst a b c d e f g.  But, minor scales
    have a wide second interval at the top - the 'leading note' is
    sharped. (Why? it just works that way! Anything else doesn't sound as
    good and isn't as flexible at saying things. In medieval times,
    scales only had 6 notes to avoid this problem - the hexachords.)
    
    So, the d minor scale is d e f g a b-flat c-sharp d - using d-flat
    for the leading note would skip the name c and duplicate the name d.
    Why isn't c-sharp put in the key signature? Tradition. (It's also
    supposedly based on the Pythagorean theory of the cycle of fifths,
    but that really only applies to major scales...)
    
    Anyway, g minor is g a b-flat c d e-flat f-sharp g, and all the other
    flat minor keys end up with a natural leading note. And there you
    have it.
    
    John Sankey <bf250@freenet.carleton.ca>

    Let's also do a-minor: a b c d e f gis a

    --jcn

   */

  /* ok, bit ugly, but here we go -- jcn */


  if (minor_i_)
    {
     if ((accidentals_i_ == 0) && (notename_str == "as"))
       notename_str = "gis";
     else if ((accidentals_i_ == -1) && (notename_str == "des"))
       notename_str = "cis";
     else if ((accidentals_i_ == -2) && (notename_str == "ges"))
      notename_str = "fis";
     else if ((accidentals_i_ == 5) && (notename_str == "g"))
       notename_str = "fisis";
     else if ((accidentals_i_ == 6) && (notename_str == "d"))
      notename_str = "cisis";
     else if ((accidentals_i_ == 7) && (notename_str == "a"))
      notename_str = "gisis";

     if ((accidentals_i_ <= -6) && (notename_str == "b"))
      notename_str = "ces";
     if ((accidentals_i_ <= -7) && (notename_str == "e"))
      notename_str = "fes";
    }
	      
  String de_octavate_str = to_str (',',  (Mudela_note::c0_pitch_i_c_ + 11 - pitch_i) / 12);
  String octavate_str = to_str ('\'',  (pitch_i - Mudela_note::c0_pitch_i_c_) / 12);
  return notename_str +de_octavate_str  + octavate_str;
}

Mudela_time_signature::Mudela_time_signature (int num_i, int den_i, int clocks_4_i, int count_32_i)
  : Mudela_item (0)
{
  sync_dur_.durlog_i_ = 3;
  sync_f_ = 1.0;
  if (count_32_i != 8)
    warning (_f ("#32 in quarter: %d", count_32_i));
  num_i_ = num_i;
  den_i_ = den_i;
  clocks_1_i_ = clocks_4_i * 4;
}

Moment
Mudela_time_signature::bar_mom ()
{
  Duration d;
  d.durlog_i_ = den_i_;
  return Moment (num_i_) * Duration_convert::dur2_mom (d);
}

int
Mudela_time_signature::clocks_1_i ()
{
  return clocks_1_i_;
}

int
Mudela_time_signature::den_i ()
{
  return den_i_;
}

int
Mudela_time_signature::num_i ()
{
  return num_i_;
}

String
Mudela_time_signature::str ()
{
  String str = "\\time "
    + to_str (num_i_) + "/" + to_str (1 << den_i_)
    + ";\n";
  return str;
}


// statics Mudela_note
/*
  this switch can be used to write simple plets like
  c4*2/3
  as
  \plet 2/3; c4 \plet 1/1;
 */
/*
  UGH: .hh says false, .cc says true.
  FIXME.
 */
bool const Mudela_note::simple_plet_b_s;

Mudela_note::Mudela_note (Mudela_column* mudela_column_l,
			  int channel_i, int pitch_i, int dyn_i)
  : Mudela_item (mudela_column_l)
{
  // junk dynamics
  (void)dyn_i;
  channel_i_ = channel_i;
  pitch_i_ = pitch_i;
  end_column_l_ = 0;
}

Duration
Mudela_note::duration ()
{
  assert (end_column_l_);
  Moment mom = end_column_l_->at_mom () - at_mom ();
  return Duration_convert::mom2_dur (mom);
}

Moment
Mudela_note::duration_mom ()
{
  assert (end_column_l_);
  return end_column_l_->at_mom () - at_mom ();
}

String
Mudela_note::str ()
{
  Duration dur = duration ();
  if (dur.durlog_i_ < -10)
    return "";

  String name_str
    = mudela_column_l_->mudela_score_l_->mudela_key_l_->notename_str (pitch_i_);

  if (simple_plet_b_s)
    return name_str + Duration_convert::dur2_str (dur) + " ";

  String str;

  //ugh
  if (dur.plet_b ())
    str += String ("\\times ")
      + String_convert::i2dec_str (dur.plet_.iso_i_, 0, 0)
      + "/"
      + String_convert::i2dec_str (dur.plet_.type_i_, 0, 0)
      + " { ";
  

  str += name_str;

  Duration tmp = dur;
  tmp.set_plet (1,1);
  str += Duration_convert::dur2_str (tmp);

  if (dur.plet_b ())
    str += String (" }");

  /* 
     note of zero duration is nonsense, 
     but let's output anyway for convenient debugging
  */
  if (!duration_mom ())
    return String ("\n% ") + str + "\n";

  return str + " ";
}

Mudela_skip::Mudela_skip (Mudela_column* mudela_column_l, Moment skip_mom)
  : Mudela_item (mudela_column_l)
{
  mom_ = skip_mom;
}

Duration
Mudela_skip::duration ()
{
  return Duration_convert::mom2_dur (mom_);
}

Moment
Mudela_skip::duration_mom ()
{
  return Duration_convert::dur2_mom (duration ());
}

String
Mudela_skip::str ()
{
  if (!mom_)
    return String ("");

  Duration dur = duration ();
  if (dur.durlog_i_<-10)
    return "";

  String str = "\\skip ";
  str += Duration_convert::dur2_str (dur) + "; ";

  return str;
}

Mudela_tempo::Mudela_tempo (int useconds_per_4_i)
  : Mudela_item (0)
{
  useconds_per_4_i_ = useconds_per_4_i;
  seconds_per_1_mom_ = Moment(useconds_per_4_i_ *4, 1e6);
}

String
Mudela_tempo::str ()
{
  String str = "\\tempo 4=";
  str += to_str (get_tempo_i (Moment (1, 4)));
  str += ";\n";
  return str;
}

int
Mudela_tempo::useconds_per_4_i ()
{
  return useconds_per_4_i_;
}

int
Mudela_tempo::get_tempo_i (Moment moment)
{
  Moment m1 = Moment (60) / moment;
  Moment m2 = seconds_per_1_mom_;
  return m1 / m2;
}

Mudela_text::Mudela_text (Mudela_text::Type type, String text_str)
  : Mudela_item (0)
{
  type_ = type;
  text_str_ = text_str;
}

String
Mudela_text::str ()
{
  if (!text_str_.length_i ()
      ||  (text_str_.length_i () != (int)strlen (text_str_.ch_C ())))
    return "";

  return "% " + text_str_ + "\n";
}

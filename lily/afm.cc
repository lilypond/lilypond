/*   
  afm.cc --  implement Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "afm.hh"
#include "box.hh"
#include "direction.hh"
#include "debug.hh"

Box &
Adobe_font_char_metric::bbox ()
{
  return B_;
}

String &
Adobe_font_char_metric::name ()
{
  return N_;
  
}

int &
Adobe_font_char_metric::code ()
{
  return C_;
}

Real &
Adobe_font_char_metric::width ()
{
  return WX_;
}

Adobe_font_char_metric::Adobe_font_char_metric ()
{
  B_ = Box( Interval(0,0), Interval (0,0));
  WX_ = 0.0;
  C_ = 0;
  C_ = -1;
}

Adobe_font_metric::Adobe_font_metric ()
{
  ItalicAngle_ = 0.0;
  IsFixedPitch_ = false;
  UnderlinePosition_ =0.;
  UnderlineThickness_=0.;
}


Box
Adobe_font_char_metric::dimensions () const
{
  Box b= B_;
  
  b[X_AXIS] *= size_ / 1000.0;
  b[Y_AXIS] *= size_ / 1000.0;

  return b;
}



#define APPEND_CHAR_METRIC_ELT(k)  outstr += to_str (#k) + " "  + to_str (k ## _)  + "; "

String
box_str (Box b)
{
  return to_str (b[X_AXIS][SMALLER]) + " " +
    to_str(b[Y_AXIS][SMALLER]) + " " +
    to_str (b[X_AXIS][BIGGER]) + " "+
    to_str (b[Y_AXIS][BIGGER]);
}

#define APPEND_BOX(k)  outstr += to_str (#k) + " "  + box_str (k ## _)  + ";"

String
Adobe_font_char_metric::str () const
{
  String outstr ;

  APPEND_CHAR_METRIC_ELT (C);
  APPEND_CHAR_METRIC_ELT(N);
  APPEND_CHAR_METRIC_ELT(WX);
  
  APPEND_BOX(B);
  return outstr + "\n";
}

#define WRITESTRING(k)  outstr += String (#k) + " "  + to_str (k ## _)  + "\n"

String
Adobe_font_metric::str () const
{
  String outstr;
  WRITESTRING(FontName);
  WRITESTRING(FullName);
  WRITESTRING(FamilyName);
  WRITESTRING(Weight);
  WRITESTRING(Version);
  WRITESTRING(Notice);
  WRITESTRING(EncodingScheme);
  WRITESTRING(ItalicAngle);
  WRITESTRING(UnderlineThickness);
  WRITESTRING(UnderlinePosition);
  outstr += "FontBBox " +  box_str (FontBBox_) +  "\n";

  for (int i=0; i < char_metrics_.size (); i++)
    outstr += char_metrics_[i].str ();
  
  return outstr;
}

Adobe_font_char_metric dummy_static_char_metric;

Adobe_font_char_metric const &
Adobe_font_metric::find_char (String nm, bool warn) const
{
  if (!name_to_metric_dict_.elem_b (nm))
    {
      if (warn)
	{
	  warning (_f ("can't find character called `%s'", nm.ch_C()));
	}
      return dummy_static_char_metric;
    }
  
  return char_metrics_[name_to_metric_dict_ [nm]];
}


Character_metric *
Adobe_font_metric::get_char (int code, bool warn) const
{
  return &find_ascii (code,warn);
}

Adobe_font_char_metric const &
Adobe_font_metric::find_ascii (int a , bool warn) const
{
  int  code = ascii_to_metric_idx_[a];
  if (code>=0)
    {
      return char_metrics_[code];
    }
  else if (warn )
    {
      warning (_f ("can't find character number %d", a));
    }
  return dummy_static_char_metric;
}

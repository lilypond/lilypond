/*   
  afm.cc --  implement Adobe_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  C_ = -1;
}

Adobe_font_metric::Adobe_font_metric ()
{
  ItalicAngle_ = 0.0;
  IsFixedPitch_ = false;
  UnderlinePosition_ =0.;
  UnderlineThickness_=0.;
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

/*
  UGH. should have hashtable.
 */
Adobe_font_char_metric
Adobe_font_metric::find_char (String nm, bool warn) const
{
  for (int i=0; i < char_metrics_.size (); i++)
    if (char_metrics_[i].N_ == nm)
      return char_metrics_[i];
  if (warn)
    warning (_f ("can't find character called `%s'", nm.ch_C()));

 Adobe_font_char_metric a;
 return a;
}

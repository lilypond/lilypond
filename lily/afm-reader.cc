/*   
  afm-reader.cc --  implement Adobe_font_metric_file
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "direction.hh"
#include "afm.hh"
#include "data-file.hh"
#include "string-convert.hh"


Box
parse_box (Array<String> a)
{
  Box b;
  int i=0;
  b[X_AXIS][SMALLER] = a[i++].value_f ();  
  b[Y_AXIS][SMALLER] = a[i++].value_f ();
  b[X_AXIS][BIGGER] = a[i++].value_f ();
  b[Y_AXIS][BIGGER] = a[i++].value_f ();
  return b;
}

String
strip_leading_white (String c)
{
  int i=0;
  while (c[i] == ' ')
    i++;
  c = c.cut_str (i, INT_MAX);
  return c;
}

Adobe_font_char_metric
read_char_metric (String s)
{
  Adobe_font_char_metric char_metric;
  Array<String> a= String_convert::split_arr (s, ';');
  for (int i=0; i < a.size (); i++)
    {
      String c = strip_leading_white (a[i]);

      Array<String> b = String_convert::split_arr (c, ' ');
      if (b[0] == "C")
	char_metric.C_ = b[1].value_i ();
      if (b[0] == "WX")
	char_metric.WX_ = b[1].value_f ();
      if (b[0] == "N")
	char_metric.N_ = strip_leading_white (b[1]);
      if (b[0] == "B")
	char_metric.B_ = parse_box (b.slice (1, b.size()));
    }
  return char_metric;
}

void
read_char_metrics (Array<Adobe_font_char_metric> &mets, Data_file &input)
{
  while (!input.eof_b ())
    {
      input.gobble_leading_white ();
      String s= input.get_line ();
      if (s == "EndCharMetrics")
	return ;
      mets.push (read_char_metric (s));
    }
}

#define READSTRING(k)  if (key == #k) { \
  afm.k ## _ = input.get_line (); continue; }
#define READBOX(b) if (key == #b) { \
  afm.b ## _ = read_box (input); continue; }
#define READREAL(r) if (key == #r) { \
  afm.r ## _ = read_real (input); continue; }

Real
read_real(Data_file &d)
{
  String s = d.get_word ();
  d.gobble_white ();
  return s.value_f ();
}


Box
read_box ( Data_file &d)
{
  Box b;
  b[X_AXIS][SMALLER] = read_real (d);  
  b[Y_AXIS][SMALLER] = read_real (d);
  b[X_AXIS][BIGGER] = read_real (d);
  b[Y_AXIS][BIGGER] = read_real (d);
  return b;
}

Adobe_font_metric
read_afm (String fn)
{
  Data_file input (fn);

  assert (!input.eof_b ());
  
  Adobe_font_metric afm;
  
  while (!input.eof_b ())
    {
      input.gobble_leading_white ();
      String w = input.get_word ();
      if (w == "StartFontMetrics")
	break;
      input.get_line ();
    }
  
  while (!input.eof_b ())
    {
      input.gobble_leading_white ();
      String key = input.get_word ();
      if (key == "Comment")
	continue;

      READSTRING(FontName);
      READSTRING(FullName);
      READSTRING(FamilyName);
      READSTRING(Weight);
      READSTRING(Version);
      READSTRING(Notice);
      READSTRING(EncodingScheme);
      READREAL(ItalicAngle);
      READREAL(UnderlineThickness);
      READREAL(UnderlinePosition);
      READBOX(FontBBox);
      if (key == "StartCharMetrics")
	{
	  input.get_line ();
  	  read_char_metrics (afm.char_metrics_, input);
	}
      if (key == "EndFontMetrics")
	break;

    }

  /*
    read to EOF
  */
  input.gulp ();
 
  return afm;
}



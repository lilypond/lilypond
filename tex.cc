
#include "tex.hh"
#include "symbol.hh"
#include "const.hh"
/*
    #TeXstring# should generate a TeX string to typeset the object in
  a hbox or vbox of exactly the objects' dimension.
*/


/// #h# is in points
String
vstrut(Real h)
{
    return String("\\vrule height ") + h + "pt depth 0pt width 0pt";
}


/// the staff with five lines.
 struct  Fiveline_staff: Stretchable_symbol {
     String operator()(Real width) {
 	String s("\\normalebalk{ ");
	s+=width * HOR_TO_PT;
	s+= "pt}";
	return s;
    }    
};


#!@PYTHON@
# -*-python-*-

'''

Remove (ont-) LilyPond specific kind of HunGA(a)Rian notation.


   ontgaar $(find flower lily python -name '*cc' -o -name '*hh' -o -name '*ll' -o -name '*yy')

   for i in $(find flower lily python -name '*.ontgaar'); do diff -u $(dirname $i)/$(basename $i .ontgaar) $i; done > pats



When done, move this to Coding Standads doco.


Mandatory suffixes:
  _       :  _       member var


Optional suffixes:
  _b      :  _b,       bool
  _p      :  _p        as in lispy pair_p ()?
  _x      :  _x,       x-coor
  _y      :  _y,       y-coor

  _byte   :  _byte
  _char   :  _char
  _count  :  _count    counter
  _drul   :  _drul     Drul_array
  _global :  _global   global var
  _grob   :  _grob     Grob
  _req    :  _req      Request
  _scm    :  _scm      SCM
  _str   :   _str      C string
  _str0   :  _str0     C string
  _string :  _string   C++ string

Prefixes:
  get_    :  
  gh_     :  Do not use: part of deprecated Guile api
  ly_     :
  scm_    :


The Grand Ontgaaring (some may remain):

  _str    -> _string
  _ch   -> _str0
  _ch_C -> _str0
  _sz   -> _str0
  
  _ch    -> _char
  
  _C     : junk
  _c     : junk
  _f     : junk
  _i     : junk
  _l     : junk
  _p     : junk, except for lispy is_foo_p ()
  _arr   : junk
  _array : junk




Voor alle suffixen waar het funcienamen betreft, als ik zeg maar wat,
multiplicity_i (), wordt naam dan zoveel mogelijk: get_mulitiplity ().
Als je dat niet doet, krijg je naam clashes met variabelen.




'''
		  
import re
import sys

files = sys.argv[1:]

for f in files:
	print f
	s = open (f).read ()
	
	# shield stuff
	s = re.sub (r'equal_p', r'equal_pX', s)
	s = re.sub (r'less_p', r'less_pX', s)
	s = re.sub (r'pair_p', r'pair_pX', s)
	s = re.sub (r'smob_p', r'smob_pX', s)
	s = re.sub (r'list_p(\W)', r'list_pX\1', s)

	s = re.sub (r'(gh_\w*_(p|str)) *\(', r'\1X (', s)
	s = re.sub (r'(ly_\w*_(p|str)) *\(', r'\1X (', s)
	s = re.sub (r'(scm_\w*_(p|str)) *\(', r'\1X (', s)
	
	s = re.sub (r'to_c(\W)', r'to_cX\1', s)
	s = re.sub (r'P_C', r'P_XC', s)

	s = re.sub (r'(\W)get_music(\W)', r'\1Yget_pending_events\2', s)

	s = re.sub (r'2_i(\W)', r'2int\1', s)
	s = re.sub (r'2_u(\W)', r'2unsigned\1', s)
	s = re.sub (r'2_f(\W)', r'2double\1', s)

	s = re.sub (r'(\w+)_str *\(', r'\1_string (', s)


	# member vars or multipart names
	s = re.sub (r'(\w+)_(c|f|i|l|p)_(\W)', r'\1_\3', s)
	s = re.sub (r'(\w+)_(c|f|i|l|p)_arr(_|\W)', r'\1_array\3', s)
	s = re.sub (r'(\w+)_arr_', r'\1_array_', s)
	s = re.sub (r'(\w+)_str_', r'\1_string_', s)
	s = re.sub (r'(\w+)_sz', r'\1_str0', s)
	
	# functions
	s = re.sub (r'(\W)ch_C *\(', r'\1Yto_str0 (', s)
	s = re.sub (r'(\W)byte_C *\(', r'\1Yto_bytes (', s)
	s = re.sub (r'(\W)byte_l *\(', r'\1Yget_bytes (', s)
	s = re.sub (r'(\W)value_i *\(', r'\1Yto_int (', s)
	s = re.sub (r'(\W)value_f *\(', r'\1Yto_double (', s)
	s = re.sub (r'find_i *\(', r'Yfind_index (', s)
	s = re.sub (r'compare_i *\)', r'compare)', s)

	
	s = re.sub (r'(\w+)_(c|f|i|l|p) *\(', r'Yget_\1 (', s)
	
	s = re.sub (r'(\w+)_arr *\(', r'\1_array (', s)
	s = re.sub (r'(\w+)_ch *\(', r'\1_str0 (', s)
	s = re.sub (r'(\w+)_str *\(', r'\1_string (', s)
	
	s = re.sub (r'(\W)str *\(', r'\1string (', s)
	s = re.sub (r'(\W)arr *\(', r'\1array (', s)
	
	s = re.sub (r'(\w+)_ch_C *\(', r'\1_str0 (', s)
	s = re.sub (r'(\w+)_ch *\(', r'\1_str0 (', s)

	# more member vars or multipart names
	s = re.sub (r'(\w+)_ch_C', r'\1_str0', s)
	s = re.sub (r'(\w+)_ch_', r'\1_char_', s)
	s = re.sub (r'(\W)ch_C(\W)', r'\1str0\2', s)

	# plain vars -- can't do, as we have
	# Real foo_f, int foo_i, SCM foo constructs
	# s = re.sub (r'(\w+)_(c|f|i|l|p)(\W)', r'\1_\3', s)

	
	# but these will go
	s = re.sub (r'(\W)arr_(l|p)(\W)', r'\1array\3', s)
	s = re.sub (r'new_(l|p)', r'new_pX', s)
	s = re.sub (r'(\w+)_(l|p)(\W)', r'\1\3', s)

	s = re.sub (r'(\w+)_arr(\W)', r'\1_array\2', s)
	s = re.sub (r'(\w+)_str(\W)', r'\1_string\2', s)
	
	s = re.sub (r'(\w+)_ch_C(\W)', r'\1_str0\2', s)
	s = re.sub (r'(\w+)_ch(\W)', r'\1_char\2', s)

	s = re.sub (r'(\w+)_C(\W)', r'\1\2', s)
	
	# fixups
	s = re.sub (r'Yfind', 'find', s)
	s = re.sub (r'Yget_argument_to', 'get_argument_index', s)
	s = re.sub (r'Yget_compare', 'compare', s)
	s = re.sub (r'Yget_cons', 'cons', s)
	s = re.sub (r'Yget_create', 'create', s)
	s = re.sub (r'Yget_find', 'find', s)
	s = re.sub (r'Yget_hex', 'hex', s)
	s = re.sub (r'Yget_index', 'index', s)
	s = re.sub (r'Yget_length', 'length', s)
	s = re.sub (r'Yget_remove', 'remove', s)
	s = re.sub (r'Yget_report', 'report', s)
	s = re.sub (r'Yget_size', 'size', s)
	s = re.sub (r'Yget_get', 'get', s)
	s = re.sub (r'Yget', 'get', s)
	s = re.sub (r'Yto', 'to', s)

	
	s = re.sub (r'(bin2dec|bin2hex|dec2bin|hex2bin)_string', r'\1', s)
	s = re.sub (r'i2hex_string', 'int2hex', s)
	s = re.sub (r'u2hex_string', 'unsigned2hex', s)
	s = re.sub (r'i2dec_string', 'int2dec', s)

	# Would this work?
	s = re.sub (r'split_array', 'split', s)
	s = re.sub (r'custos_array', 'custodes', s)
	s = re.sub (r'primitives_array', 'primitives', s)
	s = re.sub (r'(Pointer|Link|Drul|get|heap|_of|remove)_array',
		    r'\1_Xarray', s)
	s = re.sub (r'([a-rt-zA-RT-Z])_array', r'\1s', s)
	s = re.sub (r'([sS])_array', r'\1es', s)
	s = re.sub (r'_Xarray', '_array', s)
	
	# shields down
	s = re.sub (r'_pX', r'_p', s)
	s = re.sub (r'_cX', r'_c', s)
	s = re.sub (r'_strX', r'_str', s)
	s = re.sub (r'P_XC', 'P_C', s)
	s = re.sub (r'Xget_music', 'get_music', s)

	h = open (f + '.ontgaar', 'w')
	h.write (s)
	h.close ()
	

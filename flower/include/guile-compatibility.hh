/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef GUILE_COMPATIBILITY_HH
#define GUILE_COMPATIBILITY_HH

#if SCM_MINOR_VERSION < 7
/* guile-1.6.x compatibility */

inline SCM scm_cdr (SCM x)
{
  if (SCM_NCONSP (x))
    abort ();
  return SCM_CDR (x);
}
inline SCM scm_car (SCM x)
{
  if (SCM_NCONSP (x))
    abort ();
  return SCM_CAR (x);
}
#define SCM_I_CONSP(x) SCM_CONSP (x)
inline SCM scm_caar (SCM x) { return SCM_CAAR (x); }
inline SCM scm_cdar (SCM x) { return SCM_CDAR (x); }
inline SCM scm_cadr (SCM x) { return SCM_CADR (x); }
inline SCM scm_cddr (SCM x) { return SCM_CDDR (x); }
inline SCM scm_caddr (SCM x) { return SCM_CADDR (x); }
inline SCM scm_cdadr (SCM x) { return SCM_CDADR (x); }
inline SCM scm_caadr (SCM x) { return SCM_CAADR (x); }
inline SCM scm_cadar (SCM x) { return SCM_CADAR (x); }
#define scm_gc_unregister_collectable_memory(a, b, c) scm_done_free (b)
#define scm_gc_register_collectable_memory(a, b, c) scm_done_malloc (b)
#define scm_is_vector(x) (SCM_VECTORP ((SCM) x))
#define SCM_HASHTABLE_P(x) (SCM_VECTORP ((SCM) x))
#define SCM_VECTOR_REF(v, i) (SCM_VELTS ((v))[ (i)])
#define scm_from_bool(x) (x ? SCM_BOOL_T : SCM_BOOL_F)
#define scm_from_int(x) SCM_MAKINUM (x)
#define scm_from_unsigned_integer(x) scm_uint2num (x)
#define scm_from_unsigned(x) scm_uint2num (x)
#define scm_from_uint32(x) scm_uint2num (x)
#define scm_is_integer(x) SCM_INUMP (x)
#define scm_is_string(x) SCM_STRINGP (x)
#define scm_hash_table_p scm_vector_p
#define scm_from_locale_stringn(s, n) scm_mem2string (s, n)
#define scm_from_locale_string(x) scm_makfrom0str (x)
#define scm_i_string_chars(x) SCM_STRING_CHARS (x)
#define scm_i_string_length(x) SCM_STRING_LENGTH (x)
inline int ly_c_number_p (SCM x) { return SCM_NUMBERP (x); }
#define scm_is_number(x) (scm_number_p (x) == SCM_BOOL_T)
inline int ly_scm2int (SCM x) { return scm_num2int (x, 0, "ly_scm2int"); }
#define scm_to_int(x) (ly_scm2int (x))
inline int ly_scm2unsigned (SCM x) { return scm_num2uint (x, 0, "ly_scm2unsigned"); }
#define scm_to_unsigned(x) (ly_scm2unsigned (x))
inline int ly_c_symbol_p (SCM x) { return SCM_SYMBOLP (x); }
#define scm_is_symbol(x) ly_c_symbol_p (x)
inline int ly_c_boolean_p (SCM x) { return SCM_BOOLP (x); }
#define scm_is_bool(x) ly_c_boolean_p (x)
inline int ly_c_eq_p (SCM x, SCM y) { return SCM_EQ_P (x, y); }
#define scm_is_eq(x, y) (SCM_EQ_P ((x), (y)))

#define scm_c_string_length(x) SCM_STRING_LENGTH (x)
#define scm_is_pair(x) (SCM_CONSP (x))

#define scm_c_vector_length(x) SCM_VECTOR_LENGTH (x)
#define scm_c_vector_ref(x, y) SCM_VECTOR_REF (x, y)

inline double ly_scm2double (SCM x) { return scm_num2dbl (x, "ly_scm2double"); }
#define scm_to_double(x) (ly_scm2double (x))
#define scm_from_double(x) (scm_make_real (x))

#else /* !SCM_MINOR_VERSION < 7 */

#define scm_to_unsigned(x) scm_to_uint32 (x)
#define scm_from_unsigned(x) scm_from_unsigned_integer (x)

#endif /* !SCM_MINOR_VERSION < 7 */

#endif /* GUILE_COMPATIBILITY_HH */

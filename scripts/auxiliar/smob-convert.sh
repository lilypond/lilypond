#!/bin/sh
for i in $(git grep -l '^\s\+DECLARE_SIMPLE_SMOBS' lily)
do
    sed -i -n '1h
1!H
${g
  s/^\(\(class\|struct\)\s\+\([A-Za-z_]\+\)\)\(\n\(.*\n\)\+\)\s*DECLARE_SIMPLE_SMOBS\s*(\3)\s*;\n/\1 : public Simple_smob<\3>\4/gm
  s/^\(\(public:\|protected:\|private:\)\n\+\)\+\(public:\|protected:\|private:\|}\)/\3/gm
  s/^\(\(public:\|protected:\|private:\)\n\(\([ \t].*\)\?\n\)\+\)\2\n/\1/gm
  p
  d}' "$i"
done

# Now the tough stuff: we need to add in-class declarations for the rest

add_public_def ()
{
    # $1 is indented and line-delimited def, $2 is class, $3 is file
    sed -i -n '1h
1!H
${g
  s/^\(class '$2'\b\([^{};]\|\n\)*{\n\)\n*public:\n/\1public:\n'"$1"'/gmp
  t
  s/^\(class '$2'\b\([^{};]\|\n\)*{\n\)\n*\(protected\|private\):\n/\1public:\n'"$1"'\3:\n/gmp
  t
  s/^\(class '$2'\b\([^{};]\|\n\)*{\n\)\n*/\1public:\n'"$1"'private:\n/gmp
  t
  s/^\(struct '$2'\b\([^{};]\|\n\)*{\n\)\n*\(public:\n\+\)\?/\1'"$1"'/gmp
  t
  s/^\(struct '$2'\b\([^{};]\|\n\)*{\n\)\n*\(protected\|private\):\n/\1public:\n'"$1"'\3:\n/gm
  p
  d}' "$3"
}

git grep '^\s\+DECLARE_SMOBS' lily|sed -n 's/^\([^:]\+\):\s*DECLARE_SMOBS\s*(\([^)]\+\));\?\s*$/\1 \2/p'|
    while read file class
    do
	echo "Implement destructor for $class in $file" >&2
	sed -i -n '1h
1!H
${g
  s/^\(\(class\|struct\)\s\+\([A-Za-z_]\+\)\)\(\n\(.*\n\)\+\)\s*DECLARE_SMOBS\s*(\3)\s*;\n/\1 : public Smob<\3>\4/gm
  s/^\(\(class\|struct\)\s\+\([A-Za-z_]\+\)\s\+:\)\(.*\n\(.*\n\)\+\)\s*DECLARE_SMOBS\s*(\3)\s*;\n/\1 public Smob<\3>,\4/gm
  s/^\(\(public:\|protected:\|private:\)\n\+\)\+\(public:\|protected:\|private:\|}\)/\3/gm
  s/^\(\(public:\|protected:\|private:\)\n\(\([ \t].*\)\?\n\)\+\)\2\n/\1/gm
  p
  d}' "$file"
	add_public_def "  virtual ~$class ();\\n" "$class" "$file"
    done

for i in $(git grep -l '^IMPLEMENT\(_SIMPLE\)\?_SMOBS' lily)
do
    sed -i '/^IMPLEMENT\(_SIMPLE\)\?_SMOBS\s*(.*);\s*$/d' $i
done

git grep '^IMPLEMENT_TYPE_P' lily|sed -n 's/^\([^:]\+\):IMPLEMENT_TYPE_P (\([^,]*\), \("[^"]*"\));\s*/\1 \2 \3/p'|
    while read file class pred
    do
	echo "Implement predicate $pred for $class in $file" >&2
	sed -i '/^IMPLEMENT_TYPE_P/c\
const char '"$class"'::type_p_name_[] = '"$pred"';' "$file"
	for i in $(git grep -l '^\(class\|struct\)\s\+'"$class"'\s' lily)
	do
	    add_public_def '  static const char type_p_name_[];\n' "$class" "$i"
	done
  done

for i in $(git grep -l '^IMPLEMENT_DEFAULT_EQUAL_P' lily)
do
    sed -i '/^IMPLEMENT_DEFAULT_EQUAL_P/d' "$i"
done

# Get rid of stock mark_smob/print_smob/equal_p definitions: those are
# implemented by fallback in Smob_base

for i in $(git grep -l '^\([_a-zA-Z]\+::mark_smob\|print_smob\|equal_p\)\s*(SCM\s*\(\/\*[^*]*\*\/\s*\)\?[,)]' lily)
do
    sed -i -n '1h
1!H
${g
  s/^\n\?\(\(SCM\|int\)\s\+\)\([_a-zA-Z]\+\)::\(mark_smob\|equal_p\|print_smob\)\s*(SCM\s*\(\/\*[^*]*\*\/\s*\)\?[,)]\(\n*[^}]\)*\n}//gm
  p
  d}' "$i"
done

git grep '^\([_a-zA-Z]\+\)::\(mark_smob\|print_smob\|equal_p\)\b' lily|sed -n 's/^\([^:]\+\):\([_a-zA-Z]\+\)::\([_a-zA-Z]\+\)\b.*$/\1 \2 \3/p'|
    while read file class pred
    do
	echo "Declare $class::$pred in $file" >&2
	for i in $(git grep -l '^\(struct\|class\) '$class'\s' lily)
	do
	    case "$pred" in
		mark_smob)
		    add_public_def "  static SCM mark_smob (SCM);\n" "$class" "$i"
		    ;;
		print_smob)
		    add_public_def "  static int print_smob (SCM, SCM, scm_print_state *);\n" "$class" "$i"
		    ;;
		equal_p)
		    add_public_def "  static SCM equal_p (SCM, SCM);\n" "$class" "$i"
	    esac
	done
    done

for i in $(git grep -l '^#include "ly-smobs.icc"$' lily)
do
    sed -i '/^#include "ly-smobs.icc"$/d' "$i"
done

# Clean up trailing newlines
for i in $(git diff --name-only lily)
do
    sed -i -n '1h
1!H
${g
  s/\n\+$//
  p
  d}' "$i"
done

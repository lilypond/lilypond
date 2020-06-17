#!/bin/sh

# mf2pt1 pollutes CWD, so run it in a tmp dir.

# realpath doesn't exist on OSX
realpath() {
  python -c "import os; print(os.path.realpath('$1'))"
}

set -eu
mf2pt1="$1"
src="$(realpath $2)"
target="$3"
target_path="$(realpath ${target})"
srcdir="$(dirname ${src})"
name="$(basename ${src} .mf)"

tmp="$(dirname ${target_path})/tmp.$(basename ${target_path})"
rm -rf $tmp
mkdir $tmp
cd $tmp

export MFINPUTS="${srcdir}:..::"
export max_print_line=1000

${mf2pt1} --rounding=0.0001 \
  --family=$name \
  --fullname=$name \
  --name=$name $src

printf %s "${target} : " > ${name}.pfb.dep
grep '^INPUT.*mf$' ${name}.fls | sed "s|INPUT||;s|${srcdir}/||" | tr -d '\n' >> ${name}.pfb.dep

mv *.pfb *.tfm *.log *.dep ..
cd ..
rm -rf ${tmp}

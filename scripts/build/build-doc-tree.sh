#!/bin/bash

DEST="$1"
shift
LANGS="$1"
shift
LYBOOK_DB="$1"
shift
DOCS="$1"

set -eu

rm -rf ${DEST}
mkdir -p ${DEST}

if [[ -n "${LYBOOK_DB}" ]] ; then
    # This command is complicated, but it's very fast.
    rsync -r \
          --exclude='*.count' \
          --exclude='*.doctitle*' \
          --exclude='*.eps' \
          --exclude='*.log' \
          --exclude='*.pdf' \
          --exclude='*.signature' \
          --exclude='*.tex' \
          --exclude='*.texi' \
          --exclude='*.texidoc*' \
          --link-dest=${LYBOOK_DB}/  \
          ${LYBOOK_DB}/ ${DEST}/
fi

for DOC in ${DOCS}
do
        mkdir -p ${DEST}/${DOC}
done

for LANG in ${LANGS}
do
    html_suffix="${LANG}.html"
    pdf_suffix="${LANG}.pdf"
    if [[ "${LANG}" = "en" ]] ; then
        html_suffix="html"
        pdf_suffix="pdf"
    fi
    for DOC in ${DOCS}
    do
        if [[ ! -f "${LANG}/${DOC}-big-page.html" ]]
        then
            continue
        fi
   	sed -e 's#<!-- Created on .*by texi2html#<!-- Created by texi2html#g' \
	    < ${LANG}/${DOC}-big-page.html >  ${DEST}/${DOC}-big-page.${html_suffix}
	if [[ -f ${LANG}/${DOC}.pdf ]]
        then
            cp ${LANG}/${DOC}.pdf ${DEST}/${DOC}.${pdf_suffix}
        fi
        find ${LANG}/${DOC}/ -type f -name '*.html'  | while read fn
        do
            if grep --quiet "^<p>UNTRANSLATED NODE: IGNORE ME" $fn ; then
                continue
            fi
            dst="${DEST}/${DOC}/$(basename $fn .html).${html_suffix}"

            sed -e 's#\(href\|src\)="\([a-f0-9]*/lily-[a-f0-9]*\)\.\(ly\|png\)"#\1="../\2.\3"#g' \
		-e 's#\(href\|src\)="\(pictures\|ly-examples\|css\)/#\1="../\2/#g' \
		-e 's#<!-- Created on .*by texi2html#<!-- Created by texi2html#g' \
		< $fn \
		> $dst
        done
    done &
done
wait

cp -a misc/ ${DEST}
cp -a pictures/ ${DEST}
cp -a ly-examples/ ${DEST}/
cp -a css/ ${DEST}/

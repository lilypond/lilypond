#!/bin/sh

DEST="$1"
shift
LANGS="$1"
shift
LYBOOK_DB="$1"
shift
DOCS="$1"

PERL=${PERL:-perl}

set -eu

rm -rf ${DEST}
mkdir -p ${DEST}

if [ -n "${LYBOOK_DB}" ]; then
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

# Don't use the locale-specific variable name `LANG` for a local variable.
for LNG in ${LANGS}
do
    html_suffix="${LNG}.html"
    pdf_suffix="${LNG}.pdf"
    if [ "${LNG}" = "en" ]; then
        html_suffix="html"
        pdf_suffix="pdf"
    fi
    for DOC in ${DOCS}
    do
        if [ ! -f "${LNG}/${DOC}-big-page.html" ]; then
            continue
        fi
        cp ${LNG}/${DOC}-big-page.html ${DEST}/${DOC}-big-page.${html_suffix}
        if [ -f ${LNG}/${DOC}.pdf ]; then
            cp ${LNG}/${DOC}.pdf ${DEST}/${DOC}.${pdf_suffix}
        fi
        find ${LNG}/${DOC}/ -type f -name '*.html' | while read fn
        do
            if grep --quiet "^<p>UNTRANSLATED NODE: IGNORE ME" $fn ; then
                continue
            fi
            dst="${DEST}/${DOC}/$(basename $fn .html).${html_suffix}"

            $PERL -p \
              -e 's#(href|src)="([a-f0-9]*/lily-[a-f0-9]*)\.(ly|png)"#\1="../\2.\3"#g;' \
              -e 's#(href|src)="(pictures|ly-examples|css)/#\1="../\2/#g;' \
              < $fn \
              > $dst
        done
    done &
done
wait

rsync -a misc ${DEST}
rsync -a pictures ${DEST}
rsync -a ly-examples ${DEST}
rsync -a css ${DEST}

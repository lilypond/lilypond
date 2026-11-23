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

# For collecting worker process ids
pids=

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
        find ${LNG}/${DOC}/ -type f -name '*.html' \
          | $PERL -e '
              my ($dest, $suffix) = @ARGV;
              while (my $fn = <STDIN>) {
                  chomp $fn;
                  my $s = do {
                      open my $in, "<", $fn or die "$fn: $!";
                      local $/;
                      <$in>;
                  };
                  next if $s =~ /^<p>UNTRANSLATED NODE: IGNORE ME/m;
                  $s =~ s#(href|src)="([a-f0-9]*/lily-[a-f0-9]*)\.(ly|png)"#$1="../$2.$3"#g;
                  $s =~ s#(href|src)="(pictures|ly-examples|css)/#$1="../$2/#g;
                  (my $b = $fn) =~ s{.*/}{};
                  $b =~ s/\.html\z//;
                  open my $out, ">", "$dest/$b.$suffix" or die "$dest/$b.$suffix: $!";
                  print $out $s;
                  close $out;
              }' "${DEST}/${DOC}" "${html_suffix}"
    done &
    pids="$pids $!"
done
# A single wait does not convey failed execution exit states for the sake of set -e
for pid in $pids
do
    wait "$pid"
done

rsync -a misc ${DEST}
rsync -a pictures ${DEST}
rsync -a ly-examples ${DEST}
rsync -a css ${DEST}

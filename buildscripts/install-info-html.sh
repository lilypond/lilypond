#!@BASH@

dir=$1
index=$dir/index.html
shift

rm -f $index
cat > $index <<EOF
<html> 
<body>
You want to be <a href=$1/$1.html>here</a>
<p>
<ul>
EOF

for i in $*; do
cat >> $index <<EOF
<li> <a href=$i/$i.html>$i</a>
EOF
done

cat >> $index <<EOF
</ul>
</body>
</html>
EOF


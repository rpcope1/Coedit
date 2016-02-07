ver=`cat version.txt`
fld=coedit-x86
cd nux32
mkdir $fld/
cp * $fld/
zip -9 \
../output/coedit.$ver.linux32.zip \
$fld/dcd.license.txt $fld/coedit.license.txt \
$fld/coedit $fld/cetodo $fld/cesyms \
$fld/coedit.ico $fld/coedit.png \
$fld/dcd-server $fld/dcd-client
rm -rf coedit-x86

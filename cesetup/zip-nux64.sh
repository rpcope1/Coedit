ver=`cat version.txt`
cd nux64
zip -9 -j \
../output/coedit.$ver.linux64.zip \
dcd.license.txt coedit.license.txt \
coedit cetodo cesyms \
coedit.ico coedit.png \
dcd-server dcd-client

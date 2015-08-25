ver=$(<version.txt)
cd nux32
zip -9 -j \
../output/coedit.$ver.linux32.zip \
dcd.license.txt coedit.license.txt \
coedit cetodo cesyms \
coedit.ico coedit.png \
dcd-server dcd-client
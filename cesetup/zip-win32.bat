cd win32
:: assuming 7zip binary folder is somewhere in PATH
7z a -tzip -mx9^
 ..\coedit.1update2.win32.zip^
 dcd.license.txt coedit.license.txt^
 coedit.exe cetodo.exe cesyms.exe^
 coedit.ico coedit.png^
 dcd-server.exe dcd-client.exe
# basic installer for gnome-games-windows
# written on July, 27 2007 by Tom Bass (tom.bass@gmx.net)
# changes:
# version 0.0.1.0 - TB - initial coding done; special directory setting needed right now
#                        should be updated corresponding to final project dirs
#
# 

# includes
!addincludedir .\include
!include win_environment_functions.nsh

# defines
!define VERSION			"0.0.1.0"
!define BASEBINPATH 		".\..\gnome-games\bin\"
!define BASEGTKPATH		".\..\GTK\2.0\"
!define BASERESPATH	 	"res\"

# setup configuration
Name "gnome-games"
OutFile "bin\gnome-games.exe"
InstallDir $PROGRAMFILES\gnome-games
SetCompressor /SOLID lzma ; use best compression (but slow during compression)
XPStyle on
Icon ${BASERESPATH}setupicon.ico
UninstallIcon ${BASERESPATH}uninstallicon.ico

# file version info
VIProductVersion ${VERSION}
VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "Gnome-Games Setup"
VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalCopyright" "© 2007"
VIAddVersionKey /LANG=${LANG_ENGLISH} "FileDescription" "Setup for gnome-games on windows"
VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" ${VERSION}

# setup pages displayed to user
PageEx license
	LicenseData readme.txt
#	LicenseForceSelection checkbox ; if we have a license file someone has to agree to, he should make it explicitly
PageExEnd 

PageEx directory
PageExEnd

Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

# install sections
Section "uninstall information"
	WriteUninstaller $INSTDIR\uninstaller.exe
SectionEnd

Section "bin folder" 
	SetOutPath $INSTDIR\bin
	
	File ${BASEBINPATH}sol.exe
	File ${BASEBINPATH}libgmp-3.dll
	File ${BASEBINPATH}libguile-17.dll
	File ${BASEBINPATH}libiconv-2.dll
	File ${BASEBINPATH}libltdl-7.dll
	File ${BASEBINPATH}libxml2-2.dll
	File ${BASERESPATH}aisleriot.ico
SectionEnd

Section "gtk files"
	SetOutPath $INSTDIR\bin
	File ${BASEGTKPATH}bin\libcairo-2.dll
	File ${BASEGTKPATH}bin\freetype6.dll
	File ${BASEGTKPATH}bin\iconv.dll
	File ${BASEGTKPATH}bin\intl.dll
	File ${BASEGTKPATH}bin\libatk-1.0-0.dll
	File ${BASEGTKPATH}bin\libcairo-2.dll
	File ${BASEGTKPATH}bin\libexpat.dll
	File ${BASEGTKPATH}bin\libfontconfig-1.dll
	File ${BASEGTKPATH}bin\libgdk-win32-2.0-0.dll
	File ${BASEGTKPATH}bin\libgdk_pixbuf-2.0-0.dll
	File ${BASEGTKPATH}bin\libglib-2.0-0.dll
	File ${BASEGTKPATH}bin\libgmodule-2.0-0.dll
	File ${BASEGTKPATH}bin\libgobject-2.0-0.dll
	File ${BASEGTKPATH}bin\libgthread-2.0-0.dll
	File ${BASEGTKPATH}bin\libgtk-win32-2.0-0.dll
	File ${BASEGTKPATH}bin\libpango-1.0-0.dll
	File ${BASEGTKPATH}bin\libpangocairo-1.0-0.dll
	File ${BASEGTKPATH}bin\libpangoft2-1.0-0.dll
	File ${BASEGTKPATH}bin\libpangowin32-1.0-0.dll
	File ${BASEGTKPATH}bin\libpng13.dll
	File ${BASEGTKPATH}bin\zlib1.dll
	
	SetOutPath $INSTDIR\etc\gtk-2.0
	File ${BASEGTKPATH}etc\gtk-2.0\gdk-pixbuf.loaders

	SetOutPath $INSTDIR\lib\gtk-2.0\2.10.0\loaders
	File ${BASEGTKPATH}lib\gtk-2.0\2.10.0\loaders\libpixbufloader-png.dll
SectionEnd

Section "share folders"
	SetOutPath $INSTDIR\share
	File /r .\..\gnome-games\share\*.*
SectionEnd

Section "guile files"
	SetOutPath $INSTDIR\guile
	File /r .\..\guile\*.*
	
	Push "GUILE_LOAD_PATH"
	Push $INSTDIR\guile
	Call AddToEnvVar
SectionEnd

Section "Start menu entries"
	CreateDirectory "$SMPROGRAMS\gnome-games"
	CreateShortCut "$SMPROGRAMS\gnome-games\aisleriot.lnk" "$INSTDIR\bin\sol.exe" "" "$INSTDIR\bin\aisleriot.ico" 0 SW_SHOWNORMAL "" "Play Aisleriot"
	CreateShortCut "$SMPROGRAMS\gnome-games\uninstall.lnk" "$INSTDIR\uninstaller.exe" "" $INSTDIR\uninstaller.exe 0 SW_SHOWNORMAL "" "Uninstall gnome-games"
SectionEnd

# uninstall section
Section "Uninstall"
  # clean up start menu
  Delete $SMPROGRAMS\gnome-games\aisleriot.lnk
  Delete $SMPROGRAMS\gnome-games\uninstall.lnk
  RMDir $SMPROGRAMS\gnome-games
  
  # removing environment variables
  Push "GUILE_LOAD_PATH"
  Push $INSTDIR\guile
  Call un.RemoveFromEnvVar
 
  # clean install folder
  Delete $INSTDIR\uninstaller.exe
  # the next steps are relativly safe
  # if the user installs gnome-games directly into "c:\program files" a RMDir /r $INSTDIR would delete the complete folder
  SetOutPath $INSTDIR
  RMDir /r $INSTDIR\lib
  RMDir /r $INSTDIR\etc
  RMDir /r $INSTDIR\guile
  RMDir /r $INSTDIR\bin
  RMDir /r $INSTDIR\share
SectionEnd

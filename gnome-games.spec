# Note that this is NOT a relocatable package
%define ver      0.30
%define rel      SNAP
%define prefix   /usr

Summary: GNOME games
Name: gnome-games
Version: %ver
Release: %rel
Copyright: LGPL
Group: Games
Source: ftp://ftp.gnome.org/pub/gnome-games-%{ver}.tar.gz
BuildRoot: /tmp/gnome-games-root
Obsoletes: gnome
Packager: Marc Ewing <marc@redhat.com>
URL: http://www.gnome.org
Docdir: %{prefix}/doc
Summary(es): Juegos Gnome
Summary(fr): Jeux Gnome

%description
GNOME games.

GNOME is the GNU Network Object Model Environment.  That's a fancy
name but really GNOME is a nice GUI desktop environment.  It makes
using your computer easy, powerful, and easy to configure.

%description -l es
Juegos GNOME.

GNOME (GNU Network Object Model Environment) es un entorno gráfico
orientado escritorio. Con él el uso de su computadora es más fácil,
agradable y eficaz.

Este paquete contiene varios juegos para el entorno Gnome.

%description -l fr
Jeux GNOME.

GNOME (GNU Network Object Model Environment) est un environnement graphique
de type bureau. Il rends l'utilisation de votre ordinateur plus facile,
agréable et eficace, et est facile à configurer.

Ce paquetage contiens plusieurs petits jeux variés pour l'environnement
Gnome.

%package devel
Summary: GNOME games development libs
Group: X11/Libraries
Requires: gnome-games
Summary(es): Bibliotecas de desarollo para juegos Gnome
Summary(fr): Bibliothèques de developpement pour jeux de Gnome

%description devel
GNOME games development libs.

Right now this is just stuff to develop care games.  I think.

%description devel -l es
Bibliotecas de desarollo para juegos Gnome

Este paquete contiene varias bibliotecas de funciones que le facilitarán
el crear juegos para el entorno Gnome

%description devel -l fr
Bibliothèques de developpement pour jeux de Gnome

Ce paquetage contiens plusieures bibliothèques de fonctions qui rendent
plus facile la création de jeux pour l'environnement Gnome

%changelog

* Sat Nov 21 1998 Pablo Saratxaga <srtxg@chanae.alphanet.ch>

- Completed %files section
- added spanish and french translations for rpm

* Fri Nov 20 1998 Pablo Saratxaga <srtxg@chanae.alphanet.ch>

- use --localstatedir=/var/lib in config state (score files for games
  for exemple will go there).

* Mon Mar 16 1998 Marc Ewing <marc@redhat.com>

- Integrate into gnome-games CVS source tree

%prep
%setup

%build
# Needed for snapshot releases.

# --localstatedir=/var/lib to put score files in /var/lib/games; you
# need to ensure that you configured gnome-libs the same way
if [ ! -f configure ]; then
  CFLAGS="$RPM_OPT_FLAGS" ./autogen.sh --prefix=%prefix --localstatedir=/var/lib
else
  CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%prefix --localstatedir=/var/lib
fi

if [ "$SMP" != "" ]; then
  (make "MAKE=make -k -j $SMP"; exit 0)
  make
else
  make
fi

%install
rm -rf $RPM_BUILD_ROOT

make prefix=$RPM_BUILD_ROOT%{prefix} install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)

%doc AUTHORS COPYING ChangeLog NEWS README
%{prefix}/bin/*
%{prefix}/share/locale/*/*/*
%{prefix}/share/pixmaps/*
%{prefix}/share/apps/Games/*
%{prefix}/share/gnome/help/*
%{prefix}/share/sol-games
%{prefix}/share/gnome-stones
%{prefix}/share/gnome/help/*
%{prefix}/lib/lib*.so.*
%{prefix}/lib/gnome-stones
%ghost /var/lib/games/*

%files devel
%defattr(-, root, root)

%{prefix}/lib/lib*.so
%{prefix}/lib/*.a
%{prefix}/include/*


# Note that this is NOT a relocatable package
%define ver      0.99.8
%define rel      1
%define prefix   /usr

Summary: GNOME games
Name: gnome-games
Version: %ver
Release: %rel
Copyright: LGPL
Group: X11/Libraries
Source: ftp://ftp.gnome.org/pub/gnome-games-%{PACKAGE_VERSION}.tar.gz
BuildRoot: /var/tmp/gnome-games-%{PACKAGE_VERSION}-root
Obsoletes: gnome
Packager: Marc Ewing <marc@redhat.com>
URL: http://www.gnome.org
Docdir: %{prefix}/doc

%description
GNOME games.

GNOME is the GNU Network Object Model Environment.  That's a fancy
name but really GNOME is a nice GUI desktop environment.  It makes
using your computer easy, powerful, and easy to configure.

%package devel
Summary: GNOME games development libs
Group: X11/Libraries
Requires: gnome-games

%description devel
GNOME games development libs.

Right now this is just stuff to develop care games.  I think.

%changelog

* Sat Nov 21 1998 Michael Fulbright <drmike@redhat.com>

- updated for 0.30 tree

* Fri Nov 20 1998 Pablo Saratxaga <srtxg@chanae.alphanet.ch>

- use --localstatedir=/var/lib in config state (score files for games
  for exemple will go there).

* Mon Mar 16 1998 Marc Ewing <marc@redhat.com>

- Integrate into gnome-games CVS source tree

%prep
%setup

%build
# Needed for snapshot releases.
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
%attr(-, root, games) %{prefix}/bin/*
%{prefix}/share/apps/Games/*
%{prefix}/share/gnome/help/*
%{prefix}/share/gnome-stones/*
%{prefix}/share/locale/*/*/*
%{prefix}/share/pixmaps/*

#%{prefix}/share/sol-games

%{prefix}/lib/lib*.so.*
%{prefix}/lib/gnome-stones/objects/lib*.so.*
#%ghost /var/lib/games/*

%files devel
%defattr(-, root, root)

%{prefix}/lib/lib*.so
%{prefix}/lib/*a
%{prefix}/lib/gnome-stones/objects/lib*.so
%{prefix}/lib/gnome-stones/objects/lib*a
%{prefix}/include/*


# Note that this is NOT a relocatable package
%define ver      0.13
%define rel      SNAP
%define prefix   /usr

Summary: GNOME games
Name: gnome-games
Version: %ver
Release: %rel
Copyright: LGPL
Group: X11/Libraries
Source: ftp://ftp.gnome.org/pub/gnome-games-%{ver}.tar.gz
BuildRoot: /tmp/gnome-games-root
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

* Mon Mar 16 1998 Marc Ewing <marc@redhat.com>

- Integrate into gnome-games CVS source tree

%prep
%setup

%build
# Needed for snapshot releases.
if [ ! -f configure ]; then
  CFLAGS="$RPM_OPT_FLAGS" ./autogen.sh --prefix=%prefix
else
  CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%prefix
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
%{prefix}/share/apps/*
%{prefix}/share/sol-games

%files devel
%defattr(-, root, root)

%{prefix}/lib/lib*.so
%{prefix}/lib/*a
%{prefix}/include/*


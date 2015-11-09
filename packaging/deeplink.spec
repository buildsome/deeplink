Name:           deeplink
Version:        PKG_VERSION
Release:        PKG_RELEASE%{?dist}
Summary:        Summary

Group:          System Environment/Libraries
License:        GPLv2
URL:            https://github.com/ElastiLotem/deeplink
Source0:        %{name}-%{version}.tar.gz
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

# NOTE: This spec does not adhere to GHC packaging under Fedora.
# Instead, it gets some of its dependencies from a special 'deeplink-deps'.
# This can be rectified in the future.

BuildRequires:  gmp-devel
Requires:       gmp

%description
Deeplink is an innovative build system, meant to both ease the
declaration of the build steps, and give better guarantees to users.

%global debug_package %{nil}

%prep
%setup -q

mkdir -p ~/.local/{bin,stack}
export PATH=~/.local/bin:$PATH
curl -L https://github.com/commercialhaskell/stack/releases/download/v0.1.6.0/stack-0.1.6.0-linux-x86_64.tar.gz | \
  tar -zxf - -C ~/.local/stack && \
  ln -s ~/.local/stack/stack-0.1.6.0-linux-x86_64/stack ~/.local/bin/stack

~/.local/bin/stack --no-terminal setup

%build

# It depends on git, we need to fix that. For now:
export DEEPLINK_BUILT_REVISION=%{version}-%{release}
~/.local/bin/stack --no-terminal build

%install

echo rpm build root: $RPM_BUILD_ROOT
echo prefix is: %{_prefix}

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/doc/deeplink-0.1.0.0
cp LICENSE $RPM_BUILD_ROOT/%{_prefix}/share/doc/deeplink-0.1.0.0/LICENSE

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
cp .stack-work/install/*/*/*/bin/deeplink $RPM_BUILD_ROOT/%{_prefix}/bin/deeplink

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/include
cp include/*.h $RPM_BUILD_ROOT/%{_prefix}/include

%files
%{_prefix}/bin/deeplink
# FIXME: Version needs to be automated here.
%{_prefix}/share/doc/deeplink-0.1.0.0/LICENSE
%{_prefix}/include/deeplink_private.h
%{_prefix}/include/deeplink.h

%changelog

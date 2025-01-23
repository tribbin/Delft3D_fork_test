Steps to build and install esmf 8.8.0 through cygwin:
* Install cygwin by following the steps on https://www.cygwin.com/install.html
* Mark the following packages for installation in cygwin's setup-x86_64.exe (* means unclear if necessary):
    gcc-core 12.4.0-3
    gcc-fortran 12.4.0-3
    gcc-g++ 12.4.0-3
    make 4.4.1-2
    libnetcdf-cxx4-devel 4.3.1-2
    libnetcdf-devel 4.9.2-2
    libnetcdf-fortran-devel 4.5.4-1
    libcurl4 8.11.1-1
    libhdf5_310 1.14.5-1
    libhdf5hl_310 1.14.5-1
    zlib-devel 1.3.1-1
    libhwloc-devel 2.10.0-1
    libevent-devel 2.1.12-1
    perl 5.40.1-1 *
    autoconf 2.72-1 *
    automake 1.17-1 *
* Download the sources of esmf https://earthsystemmodeling.org/download/ (links to github release) and unzip, e.g., in C:\Checkouts\esmf-8.8.0
* In cygwin console, move to the esmf directory with the makefile, e.g., cd /cygdrive/c/Checkouts/esmf-8.8.0
* Place the setenv.sh script (found next to this readme) in this directory, update the path in there to this directory, and source it in cygwin: 'source setenv.sh'
* Run 'make' (runs ~2h in cygwin on my machine)
* Run 'make install'
* Copy the necessary artifacts to your destination bin folder:
    C:\Checkouts\esmf-8.8.0\install\bin\binO\Cygwin.gfortran.64.mpiuni.default\ESMF_RegridWeightGen.exe
    C:\Checkouts\esmf-8.8.0\install\lib\libO\Cygwin.gfortran.64.mpiuni.default\libesmf.dll.a
* Copy the necessary cygwin runtime dlls from C:\cygwin64\bin to your destination bin folder. I did this by trying to run the exe in a command prompt and copying all dlls that were missing according to the popups.

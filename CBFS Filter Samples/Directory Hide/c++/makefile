#
# CBFS Filter 2024 .NET Edition - Sample Project
#
# This sample project demonstrates the usage of CBFS Filter in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
#
# www.callback.com/cbfsfilter
#
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
#

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lcbfsfilter.24.0 -L../../lib64/ -lresolv
LD_FLAGS_SRC = -lz -lresolv
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o dirhide dirhide.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/cbfsfilter.o
	g++ $(OS_CFLAGS) -o dirhide ../../src/cbfsfilter.o dirhide.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/cbfsfilter.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/cbfsfilter.o ../../src/cbfsfilter.cpp

else # LINUX
ifeq ($(shell uname -m), x86_64)
  LIB=lib64
else
  LIB=lib
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lcbfsfilter  -L../../$(LIB)/ 
LD_FLAGS_SRC = -lz 

all:
	g++ -D UNIX -o dirhide dirhide.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH) 

src: ../../src/cbfsfilter.o
	g++ -D UNIX -o dirhide ../../src/cbfsfilter.o dirhide.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/cbfsfilter.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/cbfsfilter.o ../../src/cbfsfilter.cpp

clean:
	rm -f ../../src/*.o
	rm -f dirhide *.o
endif
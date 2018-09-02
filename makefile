# Set the F90 Compiler
FC     = mpif90
FFLAGS = -march=native -O3 -pipe -ffree-form -cpp \
			-Wall -Wextra -Wtabs -Wintrinsics-std -Wsurprising -Waliasing \
			-Wno-unused-parameter -Wno-unused-dummy-argument \
			-Wno-unused-variable \
                        -I$(PETSC_DIR)/include \


INC    = -I$(METIS_DIR)/include
LDFLAGS = -lm -lmetis -L$(METIS_DIR)/lib
LDFLAGS += -lpetsc -L$(PETSC_DIR)/lib

ifeq ($(openmp),yes)
	FCFLAG += -fopenmp -DOPENMP
	LDFLAGS += -fopenmp
endif

OS := $(shell uname)

ifeq ($(OS),Darwin)
	ifdef HDF5_DIR
		INC    += -I$(HDF5_DIR)/include -DHDF5
		LDFLAGS := $(HDF5_DIR)/lib/libhdf5.a $(LDFLAGS)
		LDFLAGS += -lz -ldl
		ifdef SZ_DIR
         LDFLAGS += -lsz -L$(SZ_DIR)/lib
      endif
		ifdef SZ_DIR
			LDFLAGS += -lsz -L$(SZ_DIR)/lib
		endif
	endif
endif

ifeq ($(OS),Linux)
	LDFLAGS += -Wl,-rpath=$(METIS_DIR)/lib
	LDFLAGS += -Wl,-rpath=$(PETSC_DIR)/lib

	ifdef HDF5_DIR
		INC    += -I$(HDF5_DIR)/include -DHDF5
		ifeq ($(shell test -e $(HDF5_DIR)/lib/libhdf5.a && echo -n yes),yes)
			LDFLAGS := $(HDF5_DIR)/lib/libhdf5.a $(LDFLAGS)
			#LDFLAGS := $(HDF5_DIR)/lib/libhdf5_cpp.a $(LDFLAGS)
			LDFLAGS += -lz -ldl -L$(HDF5_DIR)/lib
			LDFLAGS += -Wl,-rpath=$(HDF5_DIR)/lib
		else
			LDFLAGS := $(HDF5_DIR)/lib64/libhdf5.a $(LDFLAGS)
			#LDFLAGS := $(HDF5_DIR)/lib64/libhdf5_cpp.a $(LDFLAGS)
			LDFLAGS += -lz -ldl -L$(HDF5_DIR)/lib64
			LDFLAGS += -Wl,-rpath=$(HDF5_DIR)/lib64
		endif
		ifdef SZ_DIR
			LDFLAGS += -lsz -L$(SZ_DIR)/lib64 -Wl,-rpath=$(SZ_DIR)/lib64
		endif
	endif
endif

HDR = $(wildcard *.h)
SRC = $(wildcard *.F90)
OBJ = $(patsubst %.F90,%.o,$(SRC))

# Modules
MOD = dtype.mod mgrid.mod comdata.mod  mpetsc.mod  mtsdata.mod 
OMOD = $(patsubst %.mod,%.o,$(MOD))


TARGET  = ug3preF90

all:     $(TARGET)

dtype.mod: precision/dtype.F90
	$(FC) -c $(FFLAGS) precision/dtype.F90

%.mod: types/%.F90 dtype.mod
	$(FC) -c $(FFLAGS) types/$*.F90

%.o: %.F90 $(HDR) $(MOD) $(OMOD)
	$(FC) -c $(FFLAGS) $(INC)  $*.F90 -o $*.o

$(TARGET): $(OBJ) $(HDR) $(MOD) $(OMOD)
	$(FC) -o $(TARGET) $(OBJ) $(OMOD) $(LDFLAGS)

clean:
	rm -f *.o *.mod $(TARGET)

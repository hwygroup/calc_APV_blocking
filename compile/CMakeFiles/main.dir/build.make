# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.4

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.4.3/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.4.3/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/hwy_pro/git_saves/calc_APV_blocking

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/hwy_pro/git_saves/calc_APV_blocking/compile

# Include any dependencies generated for this target.
include CMakeFiles/main.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/main.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/main.dir/flags.make

CMakeFiles/main.dir/src/area_wgt_manager.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/area_wgt_manager.F90.o: ../src/area_wgt_manager.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/main.dir/src/area_wgt_manager.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/area_wgt_manager.F90 -o CMakeFiles/main.dir/src/area_wgt_manager.F90.o

CMakeFiles/main.dir/src/area_wgt_manager.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/area_wgt_manager.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/area_wgt_manager.F90 > CMakeFiles/main.dir/src/area_wgt_manager.F90.i

CMakeFiles/main.dir/src/area_wgt_manager.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/area_wgt_manager.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/area_wgt_manager.F90 -o CMakeFiles/main.dir/src/area_wgt_manager.F90.s

CMakeFiles/main.dir/src/area_wgt_manager.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/area_wgt_manager.F90.o.requires

CMakeFiles/main.dir/src/area_wgt_manager.F90.o.provides: CMakeFiles/main.dir/src/area_wgt_manager.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/area_wgt_manager.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/area_wgt_manager.F90.o.provides

CMakeFiles/main.dir/src/area_wgt_manager.F90.o.provides.build: CMakeFiles/main.dir/src/area_wgt_manager.F90.o


CMakeFiles/main.dir/src/array_manager.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/array_manager.F90.o: ../src/array_manager.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object CMakeFiles/main.dir/src/array_manager.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/array_manager.F90 -o CMakeFiles/main.dir/src/array_manager.F90.o

CMakeFiles/main.dir/src/array_manager.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/array_manager.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/array_manager.F90 > CMakeFiles/main.dir/src/array_manager.F90.i

CMakeFiles/main.dir/src/array_manager.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/array_manager.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/array_manager.F90 -o CMakeFiles/main.dir/src/array_manager.F90.s

CMakeFiles/main.dir/src/array_manager.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/array_manager.F90.o.requires

CMakeFiles/main.dir/src/array_manager.F90.o.provides: CMakeFiles/main.dir/src/array_manager.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/array_manager.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/array_manager.F90.o.provides

CMakeFiles/main.dir/src/array_manager.F90.o.provides.build: CMakeFiles/main.dir/src/array_manager.F90.o


CMakeFiles/main.dir/src/main.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/main.F90.o: ../src/main.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object CMakeFiles/main.dir/src/main.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/main.F90 -o CMakeFiles/main.dir/src/main.F90.o

CMakeFiles/main.dir/src/main.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/main.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/main.F90 > CMakeFiles/main.dir/src/main.F90.i

CMakeFiles/main.dir/src/main.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/main.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/main.F90 -o CMakeFiles/main.dir/src/main.F90.s

CMakeFiles/main.dir/src/main.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/main.F90.o.requires

CMakeFiles/main.dir/src/main.F90.o.provides: CMakeFiles/main.dir/src/main.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/main.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/main.F90.o.provides

CMakeFiles/main.dir/src/main.F90.o.provides.build: CMakeFiles/main.dir/src/main.F90.o


CMakeFiles/main.dir/src/nc_read_write_interface.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/nc_read_write_interface.F90.o: ../src/nc_read_write_interface.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object CMakeFiles/main.dir/src/nc_read_write_interface.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/nc_read_write_interface.F90 -o CMakeFiles/main.dir/src/nc_read_write_interface.F90.o

CMakeFiles/main.dir/src/nc_read_write_interface.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/nc_read_write_interface.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/nc_read_write_interface.F90 > CMakeFiles/main.dir/src/nc_read_write_interface.F90.i

CMakeFiles/main.dir/src/nc_read_write_interface.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/nc_read_write_interface.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/nc_read_write_interface.F90 -o CMakeFiles/main.dir/src/nc_read_write_interface.F90.s

CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.requires

CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.provides: CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.provides

CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.provides.build: CMakeFiles/main.dir/src/nc_read_write_interface.F90.o


CMakeFiles/main.dir/src/nml_manager.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/nml_manager.F90.o: ../src/nml_manager.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object CMakeFiles/main.dir/src/nml_manager.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/nml_manager.F90 -o CMakeFiles/main.dir/src/nml_manager.F90.o

CMakeFiles/main.dir/src/nml_manager.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/nml_manager.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/nml_manager.F90 > CMakeFiles/main.dir/src/nml_manager.F90.i

CMakeFiles/main.dir/src/nml_manager.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/nml_manager.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/nml_manager.F90 -o CMakeFiles/main.dir/src/nml_manager.F90.s

CMakeFiles/main.dir/src/nml_manager.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/nml_manager.F90.o.requires

CMakeFiles/main.dir/src/nml_manager.F90.o.provides: CMakeFiles/main.dir/src/nml_manager.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/nml_manager.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/nml_manager.F90.o.provides

CMakeFiles/main.dir/src/nml_manager.F90.o.provides.build: CMakeFiles/main.dir/src/nml_manager.F90.o


CMakeFiles/main.dir/src/precision_manager.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/precision_manager.F90.o: ../src/precision_manager.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building Fortran object CMakeFiles/main.dir/src/precision_manager.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/precision_manager.F90 -o CMakeFiles/main.dir/src/precision_manager.F90.o

CMakeFiles/main.dir/src/precision_manager.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/precision_manager.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/precision_manager.F90 > CMakeFiles/main.dir/src/precision_manager.F90.i

CMakeFiles/main.dir/src/precision_manager.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/precision_manager.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/precision_manager.F90 -o CMakeFiles/main.dir/src/precision_manager.F90.s

CMakeFiles/main.dir/src/precision_manager.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/precision_manager.F90.o.requires

CMakeFiles/main.dir/src/precision_manager.F90.o.provides: CMakeFiles/main.dir/src/precision_manager.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/precision_manager.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/precision_manager.F90.o.provides

CMakeFiles/main.dir/src/precision_manager.F90.o.provides.build: CMakeFiles/main.dir/src/precision_manager.F90.o


CMakeFiles/main.dir/src/time_manager.F90.o: CMakeFiles/main.dir/flags.make
CMakeFiles/main.dir/src/time_manager.F90.o: ../src/time_manager.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building Fortran object CMakeFiles/main.dir/src/time_manager.F90.o"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/hwy_pro/git_saves/calc_APV_blocking/src/time_manager.F90 -o CMakeFiles/main.dir/src/time_manager.F90.o

CMakeFiles/main.dir/src/time_manager.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/main.dir/src/time_manager.F90.i"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/hwy_pro/git_saves/calc_APV_blocking/src/time_manager.F90 > CMakeFiles/main.dir/src/time_manager.F90.i

CMakeFiles/main.dir/src/time_manager.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/main.dir/src/time_manager.F90.s"
	/usr/local/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/hwy_pro/git_saves/calc_APV_blocking/src/time_manager.F90 -o CMakeFiles/main.dir/src/time_manager.F90.s

CMakeFiles/main.dir/src/time_manager.F90.o.requires:

.PHONY : CMakeFiles/main.dir/src/time_manager.F90.o.requires

CMakeFiles/main.dir/src/time_manager.F90.o.provides: CMakeFiles/main.dir/src/time_manager.F90.o.requires
	$(MAKE) -f CMakeFiles/main.dir/build.make CMakeFiles/main.dir/src/time_manager.F90.o.provides.build
.PHONY : CMakeFiles/main.dir/src/time_manager.F90.o.provides

CMakeFiles/main.dir/src/time_manager.F90.o.provides.build: CMakeFiles/main.dir/src/time_manager.F90.o


# Object files for target main
main_OBJECTS = \
"CMakeFiles/main.dir/src/area_wgt_manager.F90.o" \
"CMakeFiles/main.dir/src/array_manager.F90.o" \
"CMakeFiles/main.dir/src/main.F90.o" \
"CMakeFiles/main.dir/src/nc_read_write_interface.F90.o" \
"CMakeFiles/main.dir/src/nml_manager.F90.o" \
"CMakeFiles/main.dir/src/precision_manager.F90.o" \
"CMakeFiles/main.dir/src/time_manager.F90.o"

# External object files for target main
main_EXTERNAL_OBJECTS =

main: CMakeFiles/main.dir/src/area_wgt_manager.F90.o
main: CMakeFiles/main.dir/src/array_manager.F90.o
main: CMakeFiles/main.dir/src/main.F90.o
main: CMakeFiles/main.dir/src/nc_read_write_interface.F90.o
main: CMakeFiles/main.dir/src/nml_manager.F90.o
main: CMakeFiles/main.dir/src/precision_manager.F90.o
main: CMakeFiles/main.dir/src/time_manager.F90.o
main: CMakeFiles/main.dir/build.make
main: CMakeFiles/main.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Linking Fortran executable main"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/main.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/main.dir/build: main

.PHONY : CMakeFiles/main.dir/build

CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/area_wgt_manager.F90.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/array_manager.F90.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/main.F90.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/nc_read_write_interface.F90.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/nml_manager.F90.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/precision_manager.F90.o.requires
CMakeFiles/main.dir/requires: CMakeFiles/main.dir/src/time_manager.F90.o.requires

.PHONY : CMakeFiles/main.dir/requires

CMakeFiles/main.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/main.dir/cmake_clean.cmake
.PHONY : CMakeFiles/main.dir/clean

CMakeFiles/main.dir/depend:
	cd /Users/hwy_pro/git_saves/calc_APV_blocking/compile && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/hwy_pro/git_saves/calc_APV_blocking /Users/hwy_pro/git_saves/calc_APV_blocking /Users/hwy_pro/git_saves/calc_APV_blocking/compile /Users/hwy_pro/git_saves/calc_APV_blocking/compile /Users/hwy_pro/git_saves/calc_APV_blocking/compile/CMakeFiles/main.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/main.dir/depend


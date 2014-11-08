# Install script for directory: /home/casper/Work/AED_PUBLIC_2.0.0alpha6/FABM-new/fabm-git/src

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/home/casper/local/fabm/gotm")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "Release")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "1")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  IF(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libfabm.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libfabm.so")
    FILE(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libfabm.so"
         RPATH "")
  ENDIF()
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES "/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/libfabm.so")
  IF(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libfabm.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libfabm.so")
    IF(CMAKE_INSTALL_DO_STRIP)
      EXECUTE_PROCESS(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libfabm.so")
    ENDIF(CMAKE_INSTALL_DO_STRIP)
  ENDIF()
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/modules/${BUILD_TYPE}/")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/casper/Work/AED_PUBLIC_2.0.0alpha6/FABM-new/fabm-git/src/../include/fabm.h"
    "/home/casper/Work/AED_PUBLIC_2.0.0alpha6/FABM-new/fabm-git/src/drivers/gotm/fabm_driver.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/aed/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/au/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/bb/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/examples/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/gotm/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/hzg/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/iow/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/klimacampus/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/metu/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/msi/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/niva/cmake_install.cmake")
  INCLUDE("/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/models/pml/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

IF(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
ELSE(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
ENDIF(CMAKE_INSTALL_COMPONENT)

FILE(WRITE "/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/${CMAKE_INSTALL_MANIFEST}" "")
FOREACH(file ${CMAKE_INSTALL_MANIFEST_FILES})
  FILE(APPEND "/home/casper/Work/AED_PUBLIC_2.0.0alpha6/fabm-git/build/${CMAKE_INSTALL_MANIFEST}" "${file}\n")
ENDFOREACH(file)

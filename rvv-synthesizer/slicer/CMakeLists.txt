cmake_minimum_required(VERSION 3.8)
project(slicer)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS "-Wall -march=native -fPIC ${CMAKE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_DEBUG "-g -Og ${CMAKE_CXX_FLAGS_DEBUG}")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 ${CMAKE_CXX_FLAGS_RELEASE}")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO
    "${CMAKE_CXX_FLAGS_RELEASE} -g ${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

add_definitions(${LLVM_DEFINITIONS})

message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(include)

add_library(
  slice
  "lib/Converter/Converter.cpp"
  "lib/Operator/Common.cpp"
  "lib/Operator/Compare.cpp"
  "lib/Operator/ElementIndex.cpp"
  "lib/Operator/Extract.cpp"
  "lib/Operator/Insert.cpp"
  "lib/Operator/MaskReduction.cpp"
  "lib/Operator/Merge.cpp"
  "lib/Operator/MoveScalarFromVector.cpp"
  "lib/Operator/MoveToVector.cpp"
  "lib/Operator/NarrowingFixedPointClip.cpp"
  "lib/Operator/NarrowingRightShift.cpp"
  "lib/Operator/Reinterpret.cpp"
  "lib/Operator/ScalarIntegerBinary.cpp"
  "lib/Operator/ScalarIntegerTrunc.cpp"
  "lib/Operator/SetMaskBit.cpp"
  "lib/Operator/SetVectorLength.cpp"
  "lib/Operator/SingleWidthIntegerBinary.cpp"
  "lib/Operator/Slide.cpp"
  "lib/Operator/VectorRegisterByteLength.cpp"
  "lib/Operator/WideningIntegerBinary.cpp"
  "lib/Operator/WideningIntegerMultiplyAdd.cpp"
  "lib/Slicer/Filter/NoSingleUseScalar.cpp"
  "lib/Slicer/Filter/NoVectorLengthInput.cpp"
  "lib/Slicer/Slice.cpp"
  "lib/Type/Ratio.cpp")
target_link_libraries(slice PRIVATE ${LLVM_LIBS})

get_property(
  dirs
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  PROPERTY INCLUDE_DIRECTORIES)
foreach(dir ${dirs})
  message(STATUS "dir='${dir}'")
endforeach()

add_llvm_executable(slicer "tools/slicer.cpp")
target_link_libraries(slicer PRIVATE slice ${LLVM_LIBS})
add_llvm_executable(converter "tools/converter.cpp")
target_link_libraries(converter PRIVATE slice ${LLVM_LIBS})

add_subdirectory(test)

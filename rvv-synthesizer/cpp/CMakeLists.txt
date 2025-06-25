project(highway-rvv-instantiation CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

find_package(HWY 1.0.7 REQUIRED)

include(CheckCXXSourceCompiles)
check_cxx_source_compiles(
  "int main() {
      #if !defined(__riscv)
      static_assert(false, \"__riscv is not defined\");
      #endif
      return 0;
    }"
  BUILD_RISCV)

set(FLAGS)
if(BUILD_RISCV)
  list(
    APPEND
    FLAGS
    -march=rv64gcv1p0
    -O3
    -mllvm
    -enable-misched
    -mcpu=sifive-x280
    # -save-temps
  )
endif()

add_library(odd_even_blocks STATIC src/odd_even_blocks.cpp src/insert_lane.cpp)
target_compile_options(odd_even_blocks PRIVATE ${FLAGS})
target_link_libraries(odd_even_blocks PRIVATE hwy)

find_package(Boost)

macro(add_ir_generation target)
  add_custom_command(
    OUTPUT ${target}.i
    COMMAND
      ${CMAKE_CXX_COMPILER} ${FLAGS} -E ${PROJECT_SOURCE_DIR}/src/${target}.cpp
      -I ${Boost_INCLUDE_DIR} -o ${PROJECT_BINARY_DIR}/${target}.i
    DEPENDS ${PROJECT_SOURCE_DIR}/src/${target}.cpp)
  add_custom_command(
    OUTPUT ir/${target}.ll
    COMMAND
      ${CMAKE_CXX_COMPILER} ${FLAGS} -S -emit-llvm
      ${PROJECT_SOURCE_DIR}/src/${target}.cpp -I ${Boost_INCLUDE_DIR} -o
      ${PROJECT_BINARY_DIR}/ir/${target}.ll
    COMMAND ${PROJECT_SOURCE_DIR}/post_process.sh
            ${PROJECT_BINARY_DIR}/ir/${target}.ll
    DEPENDS ${PROJECT_SOURCE_DIR}/src/${target}.cpp)
  add_custom_target(${target}_ir ALL DEPENDS ir/${target}.ll ${target}.i)
endmacro()

add_ir_generation(odd_even_blocks)
add_ir_generation(insert_lane)
add_ir_generation(insert_block)
add_ir_generation(add_sub)
add_ir_generation(mul_even_odd)
add_ir_generation(zero_extend_resize_bit_cast)
add_ir_generation(min_max_128)
add_ir_generation(compare)
add_ir_generation(concat)
add_ir_generation(vqsort)

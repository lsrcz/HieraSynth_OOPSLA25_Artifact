#!/usr/bin/env bash

./build/converter ../add_sub/add_sub.ll > ../add_sub/add_sub.sir
./build/converter ../concat/concat.ll > ../concat/concat.sir
./build/converter ../insert_lane/insert_lane.ll > ../insert_lane/insert_lane.sir
./build/converter ../lt128/compare.ll > ../lt128/compare.sir
./build/converter ../min_max_128/min_max_128.ll > ../min_max_128/min_max_128.sir
./build/converter ../mul_even_odd/mul_even_odd.ll > ../mul_even_odd/mul_even_odd.sir
./build/converter ../odd_even_blocks/odd_even_blocks.ll > ../odd_even_blocks/odd_even_blocks.sir
./build/converter ../zero_extend_resize_bit_cast/zero_extend_resize_bit_cast.ll > ../zero_extend_resize_bit_cast/zero_extend_resize_bit_cast.sir
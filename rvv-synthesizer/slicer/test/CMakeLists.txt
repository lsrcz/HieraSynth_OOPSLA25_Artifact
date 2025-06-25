if(LLVM_INCLUDE_TESTS)
  set(TEST_DEPENDS FileCheck converter)

  configure_lit_site_cfg(
    ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in
    ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg MAIN_CONFIG
    ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg)

  add_lit_testsuite(check-rvv-converter-tests "Running RVV Converter tests"
                    ${CMAKE_CURRENT_BINARY_DIR} DEPENDS ${TEST_DEPENDS})

  add_custom_target(check)
  add_dependencies(check check-rvv-converter-tests)
endif()

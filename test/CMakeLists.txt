cmake_minimum_required(VERSION 3.15)

project(e2etest C CXX)

set(CATTLESHED_LISTEN_PORT 50051)
set(CATTLESHED_BASEDIR ${CMAKE_CURRENT_SOURCE_DIR}/_tmp/wandbox)
set(CATTLESHED_STOREDIR ${CMAKE_CURRENT_SOURCE_DIR}/_tmp/log)
set(CATTLESHED_BINDIR ${CMAKE_CURRENT_BINARY_DIR}/cattleshed)

add_subdirectory(../cattleshed cattleshed)
add_subdirectory(../kennel kennel)

# ---- テストの設定

enable_testing()

if (ENABLE_TSAN)
  set(_E2E_ARGS --tsan)
endif()
if (ENABLE_ASAN)
  set(_E2E_ARGS --asan)
endif()

add_test(
  NAME test_e2e
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_e2e.sh ${_E2E_ARGS}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

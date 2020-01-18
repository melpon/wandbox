find_package(Spdlog REQUIRED)

# 頑張って探したりせず、単純に GGRPC_ROOT_DIR を見る
if(NOT TARGET Ggrpc::Ggrpc)
  add_library(Ggrpc::Ggrpc INTERFACE IMPORTED)
  set_target_properties(Ggrpc::Ggrpc PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${GGRPC_ROOT_DIR}/include")
target_link_libraries(Ggrpc::Ggrpc INTERFACE Spdlog::Spdlog)
endif()

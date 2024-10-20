find_package(Spdlog REQUIRED)

# 頑張って探したりせず、単純に GGRPC_ROOT_DIR を見る
if(NOT TARGET Ggrpc::ggrpc)
  add_library(Ggrpc::ggrpc INTERFACE IMPORTED)
  set_target_properties(Ggrpc::ggrpc PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${GGRPC_ROOT_DIR}/include")
  target_link_libraries(Ggrpc::ggrpc INTERFACE
    gRPC::grpc++
    protobuf::libprotobuf
    gRPC::utf8_range_lib
    utf8_range::utf8_validity
    absl::log
    absl::cord
    absl::cord_internal
    Spdlog::Spdlog)
endif()

# 頑張って探したりせず、単純に SPDLOG_ROOT_DIR を見る
add_library(Spdlog::Spdlog INTERFACE IMPORTED)
set_target_properties(Spdlog::Spdlog PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${SPDLOG_ROOT_DIR}/include")

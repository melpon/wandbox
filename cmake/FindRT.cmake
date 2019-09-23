find_path(RT_INCLUDE_DIR NAMES aio.h)
find_library(RT_LIBRARY NAMES rt)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(RT DEFAULT_MSG RT_LIBRARY RT_INCLUDE_DIR)

mark_as_advanced(RT_INCLUDE_DIR RT_LIBRARY)

if(RT_FOUND)
  if(NOT TARGET RT::RT)
    add_library(RT::RT UNKNOWN IMPORTED)
    set_target_properties(RT::RT PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${RT_INCLUDE_DIR}"
      IMPORTED_LOCATION "${RT_LIBRARY}")
  endif()
endif()


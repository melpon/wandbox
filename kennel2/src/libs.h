#ifndef LIBS_H_INCLUDED
#define LIBS_H_INCLUDED

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

#include <cppdb/frontend.h>
#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <cppcms/service.h>
#include <cppcms/http_context.h>
#include <cppcms/http_response.h>
#include <cppcms/serialization.h>
#include <cppcms/json.h>
#include <cppcms/view.h>
#include <cppcms/http_context.h>
#include <cppcms/http_response.h>
#include <booster/system_error.h>
#include <booster/posix_time.h>
#include <booster/shared_ptr.h>
#include <booster/enable_shared_from_this.h>
#include <booster/function.h>
#include <booster/aio/deadline_timer.h>
#include <booster/aio/io_service.h>
#include <booster/aio/stream_socket.h>
#include <booster/aio/buffer.h>

#pragma GCC diagnostic pop

#endif // LIBS_H_INCLUDED

{ "application" :
    { "database": "sqlite3:db=kennel_production.sqlite;@pool_size=10"
    , "static_dir": "static"
    , "scheme": "http"
    , "domain": "melpon.org"
    , "root": "/wandbox/cppcms"
    }
, "service" :
    { "api" : "http"
    , "port" : 8088
    }
, "gzip" :
    { "enable": false
    }
, "cache" :
    { "backend": "thread_shared"
    }
}

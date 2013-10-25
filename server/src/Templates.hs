{-# LANGUAGE QuasiQuotes #-}

module Templates where

import Heredoc

header, info, help :: String
header = [heredoc|

                                  _/  _/
                         _/_/_/  _/  _/    _/_/_/  _/  _/_/
                      _/_/      _/  _/  _/    _/  _/_/
                         _/_/  _/  _/  _/    _/  _/
                    _/_/_/    _/  _/    _/_/_/  _/

                          s    e    r    v    e    r

                            github.com/grsmv/sllar

      -------------------------------------------------------------------

|]


help = [heredoc|
$header
      Example usage:

        sllar-server start


      List of server options:

        start   - starting server (package receiving and outputtting,
                  web-interface)

        stop    - stopping server

        renew   - recollecting information from received packages and
                  updating package database

        info    - getting information about current sllar-server
                  information

        help    - read this splendid information again

|]


info = [heredoc|
$header
                      Current state  :  $state
                               Port  :  $port
               Packages in database  :  $numberOfPackages (omg, we are popular)

                      Resources dir  :  $sharedPath
                           Database  :    ├── database.sqlite
                    Packages folder  :    ├── packages/
                      Configuration  :    ├── config
                           Pid file  :    └── tmp/sllar-server.pid

|]

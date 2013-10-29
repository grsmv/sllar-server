{-# LANGUAGE QuasiQuotes #-}

module Templates where

import Heredoc

info, help :: String
help = [heredoc|
  Usage:

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
        Current state  :  $state
                 Port  :  $port
  Packages registered  :  $numberOfPackages
               Version :  $version

        Resources dir  :  $sharedPath
             Database  :    ├── database.sqlite
      Packages folder  :    ├── packages/
        Configuration  :    ├── config
             Pid file  :    └── tmp/sllar-server.pid
|]

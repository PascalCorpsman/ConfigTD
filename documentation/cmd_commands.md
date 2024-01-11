
# CMD Commands

Typically you start all applications by double clicking on them. Experienced user may also use some additional startparameters. All startparameters are optional, the applications will also run without them.

### Client
| Parameter | Description |
| --- | :--- |
| -f <filename> | enable logging to file
| -l <number> | set loglegel to <number> (0..6)
| -s <folder> | set share folder to <folder>
| -m <folder> | set map folder
| -c  |  enable logging to console [Windows version only]

### Server
Normaly the server would be started from the client, that creates the host game. If you want to run a dedicated server you can start the server by hand using the following parameters

| Parameter | Description |
| --- | :--- |
| -p <Port> | Spezifies the port number to listen to.
| -pw <Password> | Spezifies the password default "" (= none)
| -l <LogLevel> | Sets Loglevel (default = 2) [1 = show everything .. 7 = show only fatal errors]
| -t <Timeout> | Sets automatic close on no user connected, default 3000 ms <br> 0 = Disabled, typing "ESC" in the servers console window will terminate
| -f <Filename> | Logs additional to a file
| -m <Foldername> | Folder to load the maps from (default ./maps)
| -h | Help screen

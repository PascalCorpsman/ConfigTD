# Manual for Config TD


## Folder Descriptions
buildings: All global accessable buildings are stored here

graphics: Lots of sample graphics, if you want to create a map / opponent or building y yourself

maps: All maps that are created by the server

opponents: All global accessable opponents are stored here

heros: All global accessable heros are stored here

savegames: location where the server stores backups and savegames

share: if you connect as client, all downloaded data will be stored here

textures: Textures needed by the program do not modify its content
  
## Application Descriptions

Animation_editor: This is the editor to create and modify .ani files it can be downloaded [here](https://github.com/PascalCorpsman/Examples/tree/master/OpenGL/Animation_Editor)

config_td: This is the client application and the actual game

ctd_server: This is the server application, it will be started through the game, when hosting a game or can be started manually (dedicated mode)
  
image_overview: If you want to browse over all graphics within the "graphics" folder use this program
  
opponent_overview:  If you want to see / compare all global opponents whithin one screen use this program 

## Other files

balancing.ods: Tabelular overview over all buildings / opponents settings during release state.

settings.ini: All by the game collected informations and settings.  
  
## Keyboard commands

### Ingame
| Key | Description |
| --- | :--- |
| ^ | Toggle show/hide side menu
| p | Toggle pause
| q | Select all buildings of same stage and owner and type (as the selected)
| s | Sell selected buildings / stop movement of selected heros
| h | Select all own heros
| u | Upgrade selected buildings
| w | Show player names on map, where opponents get spawned
| b | Overlay which tiles are buildable or not
| "+" | Increase simulation speed
| "-" | Decrease simulation speed
| ALT | Show lifepoints of all opponents
| Return | Open Chat window
| 0..9 | Keyboard shortcuts for buildings/hero in the buy menu
| F1..F8 | if a building/hero is selected, the corresponding building/hero strategy will be set (only on own ones)

### Editor
| Key | Description |
| --- | :--- |
| ^  | Toggle show/hide side menu

## Mouse commands

### Ingame
| Key | Description |
| --- | :--- |
| Left | Select building / Opponent / Placement, place building
| Right | Remove selection
| Left doubleclick | Select all buildings of same stage and owner and type on the visible screen
| CTRL + Left | Display a "!" on the map that can be seen by all players
| SHIFT + Left | Toggle selection of building / Place Building and keep buy selection
| Move + (Middle/Right) | Scroll map   
| Wheel Up | Zoom in
| Wheel Down | Zoom Out

### Editor
| Key | Description |
| --- | :--- |
| Left | Modify terrain, add/del placements, add/del waypoint
| CTRL + Left | Display a "!" on the map that can be seen by all players
| Move + Right | Scroll map   
| Wheel Up | Zoom in
| Wheel Down | Zoom Out

## Chat commands
| Command | Description |
| --- | :--- |
| -air | list all waves that contain damageclass 4 opponents
| -boss | list all waves that contain boss opponents
| -bonus | list all waves that contain bonus opponents
| -clear | clear log
| -? | list all available commands
| -p | show pint times between all players and server
| -kick <playername> | tells the server to kick player <playername> out of the game (ATTENTION use with care!)

## Buildings strategy buttons
| Image | Description |
| --- | :--- |
| ![](textures/first.png) |  Attack first opponent first
| ![](textures/last.png) | Attack last opponent first
| ![](textures/mindistance.png) | Attack nearest opponent first
| ![](textures/maxdistance.png) | Attack farest opponent first
| ![](textures/strongest.png) | Attack strongest opponent first
| ![](textures/weakest.png) | Attack weakest opponent first
| ![](textures/random.png) | Attack Random opponent
| ![](textures/prefer_air.png) | prefer air units over floor units
| ![](textures/prefer_airs.png) | prefer floor units over air units

## Startparameters

All startparameters are optional, the applications will also run without them.

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


# File / Folder descriptions

This document explains the porpuse and reason of every file in this repository.

## Folder Descriptions

| Folder | Description |
| --- | --- |
| [buildings](https://github.com/PascalCorpsman/ConfigTD_Data/tree/main/buildings) | All global accessable buildings are stored here (use ingame editor to access)
| client | sourcecode folder of the client
| documentation| Holds all documentations
| [graphics](https://github.com/PascalCorpsman/ConfigTD_Data/tree/main/graphics) | Lots of sample graphics, if you want to create a map / opponent or building by yourself
| heros | All global accessable heros are stored here (use ingame editor to access)
| logs | Server stores its log files here (for debuging)
| [maps](https://github.com/PascalCorpsman/ConfigTD_Data/tree/main/maps) | All maps that are created by the server
| [opponents](https://github.com/PascalCorpsman/ConfigTD_Data/tree/main/opponents) | All global accessable opponents are stored here (use ingame editor to access)
| savegames | Location where the server stores backups and savegames
| server | sourcecode folder of the server
| share | The client always "downloads" all maps and stores them into the share folder, if you played on a external server you can copy the maps from the share folder to your local maps folder to be able to single play them
| [textures](https://github.com/PascalCorpsman/ConfigTD/tree/main/textures) | Textures needed by the program do not modify its content
| units | sourcecode folder of shared units between server and client

## Application Descriptions

| Application | Description |
| --- | --- |
| Animation_editor | This is the editor to create and modify .ani files it's source can be downloaded [here](https://github.com/PascalCorpsman/Examples/tree/master/OpenGL/Animation_Editor)
| config_td | This is the client application and the actual game, start to play
| ctd_server | This is the server application, it will be started through the game, when hosting a game or can be started manually (dedicated mode)
| image_overview | If you want to browse over all graphics within the "graphics" folder use this program
| opponent_overview | If you want to see / compare all global opponents whithin one screen use this program 

## general file descriptions

| Filename | Description |
| --- | --- |
| *.inc | Sourcecode files (configuration for libs)
| *.lpi | Sourcecode file (Lazarus project file)
| *.lpr | Sourcecode file (same as main.c)
| *.dll | precompiled SSH library (for windows users needed)
| *.lfm | Sourcecode file (Lazarus form module)
| *.pas | Sourcecode file (Free Pascal source code)
| *.md | Documentation / License / ..
| [balancing.ods](https://github.com/PascalCorpsman/ConfigTD_Data/tree/main/balancing.ods) | Tabelular overview over all buildings / opponents settings
| settings.ini | All by the game collected informations and settings




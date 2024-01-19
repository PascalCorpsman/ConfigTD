(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of config_td                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uctd_common;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils, Graphics, uvectormath, Dialogs, ulogger
{$IFDEF Client}
  , uopengl_graphikengine
  , forms, Controls, uopengl_animation
{$ENDIF}
  ;

{$I ../client/updater_settings.inc}

Const
  (*
   * Historie : 0.01    = Initialversion
   *            0.02    = Anpassen aller Zeitbasen auf 64-Bit
   *            0.03    = Ändern Savegame Format
   *            0.04    = Fix Zugriffsverletzung bei TBuilding.GetHint direkt nach dem Create
   *            0.05    = Der Client kann nun Savegame Lösch anfragen stellen.
   *            0.06000 = Support für Highscores
   *            0.07001 = Bugfixes, Main Menü buttons -- Erste Version die via Autoupdater getestet wird.
   * -Released- 0.07002 = Menü Buttons fertig, bessere Übersicht in der Statistik, Opponents sortiert, MapTex Create Button auf Form4
   * -Released- 0.07003 = Multiples Kopieren von Objekten im Opp / Geb Editor
   *                      F1 - F8 Hotkeys für die Strategiebuttons
   *                      Anzeige Versionsinfo, bis die erste Karte geladen wurde.
   *            0.07004 = Fix Bug, wenn Gebäude wärend Speedup erstellt wurden dann hat die Bauzeit 0-Sek gedauert anstatt die Korrekte!
   *                      Fix Rechtschreibfehler in Game Statistik Dialog
   *                      Fix Ausblenden des Geld Senden Buttons im Player Menü war manchmal an der Falsche Position
   * -Released- 0.08000 = Einführen des Codes für die Wegpunktflächen -> benötigt neue Nummer da Server und Client nicht mehr Compatibel !
   *                      Fix GUI-Deadlock
   *                      Verbessern CAM-Jitter Bug
   * -Released- 0.08001 = Fix Unable to build after Load Game
   *                      Fix AV when clicking in "Black" area and not joined
   *                      Fix Air units where not deleted if reached a build over final point
   *            0.08002 = Fix Air units where not able to overpass waypoint 2
   *            0.08003 = Fix Crash on Update
   *            0.08004 = Debugging
   *            0.08005 = Fix Lnet NoDelay Bug for Linux versions
   *            0.08006 = Reaktivieren von NoDelay für den Server
   *            0.08007 = Anzeigen Schwierigkeitslevel im Chat Verlauf, wenn Spiel gestartet wird.
   *                      Im Nicht-Koop Spiel: Türme priorisieren "eigene" Gegner über die der anderen Spieler
   *                      FIX AV, wenn connect mit bereits vergebenen Usernamen aufgerufen wurde
   * -Released- 0.09000 = FIX AV, wenn ein Gebäude Editiert wurde und mit OK bestätigt, konnten keine weitere Gebs mehr editiert werden
   *                      Heros
   *                      Protokolchange zwischen Server und Client, deswegen 8 -> 9 !
   * -Released- 0.09001 = Improve Helden Handling
   * -Released- 0.09002 = Feature "-kick <playername>"
   *                      FIX: Udp-Server Sauber runterfahren, hilft bei häufigem Wiederverbinden, das sich Server und Client schneller finden.
   * -Released- 0.09003 = FIX: DamageClass Graphiken wurden nicht mehr angezeigt
   *                      FIX: DoOnPaint anstatt .Invalidate, warum auch immer ??
   *            0.09004 = FIX: Beim Doppelklick auf ein Gebäude wurden die "obersten" Gebäude nicht selektiert
   *            0.10000 = ADD: DelUnusedOppponents
   *                      ADD: Preview of already connected player
   *                      ADD: ESC deselct cursor in game
   *                      ADD: Option for "Dark" theme
   *                      FIX: Rendering der Karte zwischen den Oberen Elementen wurde als störend empfunden
   *                      FIX: Crash on "Show Highscore" when running from inside the Lazarus-IDE
   *                      ADD: Automatich Restart after Darkmode Change
   * Known Bugs :
   *)
  (*
   * Die Versionsnummer ist immer für Client und Server Gleich, beim Verbinden
   * prüfen die beiden das, der Server Verweigert das Connecten, bei Ungleicher Nummer !
   *)
  Version: uint32 = updater_int_Version; // ACHTUNG die Versionsnummer mus hier und in der Zeile darunter angepasst werden
  defCaption = 'Config TD ver. ' + updater_Version // ACHTUNG die Versionsnummer mus hier und in der Zeile darüber angepasst werden
{$IFDEF DebuggMode}
  + ' build: ' + {$I %DATE%} + '  ' + {$I %TIME%}
{$ENDIF}
  ;
  URL_CheckForUpdate = ''; // TODO: Need to be defined !

  RF_VersionInfo = 'Your version: %s' + LineEnding +
    'Online version: %s' + LineEnding +
    'Release Message: ' + LineEnding + LineEnding + '%s' + LineEnding + LineEnding +
    'If you start the update now, all unsaved data will get lost.' + LineEnding +
    'Would you like to update Config TD now?';

  fontscale: Single = 2.0;

  DefaultSplashHintDelay = 3000; // Anzeigezeit in ms
  DefaultSplashHintColor: TVector3 = (x: 1; y: 1; z: 1); // Schriftfarbe
  DefaultShowToolTippTime = 1000; // Verweilt der Curoser mehr als diese Zeit in ms über einem Object, dann wird sein Hint als Tooltipp angezeigt.

  PlayerDisplayTimeOnStartGame = 5000; // Zeit in ms wie lange die Spielernamen an den Exits gerendert werden

  ServerAutoTimeout = 3000; // Zeit in ms bis der Server sich autoamtisch beendet, wenn keine Spieler verbunden sind.
  MarkLifeTime = 3000; // Zeit wie lange es Dauert bis ein Mark wieder ausgeblendet wird
  OpenGamesTimeout = 2000; // Wenn der Client vom Server 2s lang keine Bestätigung bekommen hat, dass dieser noch ein Einloggen erlaubt, dann wird das Spiel aus der Liste der Verfügbaren genommen
  UDPPingPort = 8005; // Der Port auf welchem Client und Server Lauschen um heraus zu finden ob offene Spiele da sind.

  Speedup: integer = 1; // 1 = Realzeit, 2 = Doppelt so schnell , 4 = viermal so schnell
  FrameRate = 16; // Zeit in ms bis ein neues Frame berechnet wird (Achtung das muss sich Ganzzahlig durch Speedup teilen lassen)
  UpdateRate = 40; // Zeit in ms bis die Clients wieder Aktualisiert werden
  SynchonizeTimeOut = 150; // Kommt mehr als 150ms lang keine Heartbeat Message von allen Clients, dann wird eine Zwangspause eingeleitet.
  HeartBeatTime = 100; // Muss Sinnigerweise << SynchonizeTimeOut sein.

  (*
   * Kachelfarben für die Kombinationen der Kacheln der Karten
   *)
  Begehbar_col: Tcolor = clgreen;
  Bebaubar_col: Tcolor = clblue;
  Nichts_Col: Tcolor = clgray;
  Beides_Col: Tcolor = $00FF8000;
  Grid_color: TColor = $00404040;

  (*
   * Die 3 Farben zeigen die Lebensenergie der Opponents an
   * 0..30 % Bad
   * 31 .. 70 % Middle
   * 71 .. >= 100 % Good
   *)
  Good_Col: Tcolor = cllime;
  Middle_Col: Tcolor = clyellow;
  Bad_col: Tcolor = clred;

  Building_Range_Col: TColor = clwhite;

  (*
   * Namen der Im Spiel benutzten Texturen
   *)
  PlayerStartPointTex = 'playerstartpoint.png';
  BuyKachel = 'buykachel.png';
  BuildBuilding = 'build_in_progress.png';
  SplashMarkTex = 'splashmark.png';
  SelectTex = 'select.png';
  Levelup = 'level_up.png';
  Sell = 'sell.png';
  TabDown = 'tab_down.png';
  TabUp = 'tab_up.png';
  TabLeft = 'tab_left.png';
  TabRight = 'tab_right.png';

  (*
   * Bitmasken für die Raw Data in den Karten
   *)
  Begehbar = 1;
  Bebaubar = 2;

  (*
   * Die Callbacks der Karte, welche dem Hauptspiel Mitteilt, dass
   * Ein Gegner gekillt wurde, oder oder oder..
   *)
  UpdateIdLifeLost = 0;
  UpdateIdOpponentKilled = 1;
  UpdateIdBankIncrease = 2;
  UpdateDamage_1 = 3;
  UpdateDamage_2 = 4;
  UpdateDamage_3 = 5;
  UpdateDamage_4 = 6;
  UpdateIDBonusOpFinished = 7;

  (*
   * Zum Rendern des "Lebensbalkens" über einer Einheit, oder wähend des Gebäude bauens
   *)
  LifebarOffset = 5;
  LifebarHeight = 3;

  (*
   * Error Codes
   *)
  EC_No_Error = 0;
  // Fehlercodes bezogen auf Join-Game
  EC_Invalid_Password = 1;
  EC_User_already_exists = 2;
  EC_Game_Full = 3;
  EC_Invalid_Versions = 4; // Bezogen auf Vergleich Client Version vs. Server Version

  (*
   * Damit der Tiefentest funktioniert müssen diverse Layer definiert werden
   *
   * Verfügbarer Z-Bereich ]-1..1[
   *
   *)
  ctd_EPSILON = 0.05;

  (*
   * Die Karte Rendert sich wie Folgt :
   * -0.9  = ctd_Map_Layer                  = Map-Kacheln
   * -0.85 = ctd_Map_Layer + 1 * ctd_EPSILON = Map-Kacheln Grid
   * -0.8  = ctd_Map_Layer + 2 * ctd_EPSILON = Map-Placements
   * -0.75 = ctd_Map_Layer + 3 * ctd_EPSILON = Map-Boden Gegner
   * -0.7  = ctd_Map_Layer + 4 * ctd_EPSILON = Map-Gebäude
   * -0.65 = ctd_Map_Layer + 5 * ctd_EPSILON = Map-Luft Gegner
   * -0.6  = ctd_Map_Layer + 6 * ctd_EPSILON = Bullets / Select Rahmen der Einheiten, Gebäude
   * -0.55 = ctd_Map_Layer + 7 * ctd_EPSILON = Waypoints
   *)
  ctd_Map_Layer = -0.9; // Die Kacheln

  (*
   * -0.4  = ctd_Buy_Preview_Layer                   = Das Layer in dem zu Kaufende Objekte Liegen
   * -0.45 = ctd_Buy_Preview_Layer + 1 * ctd_EPSILON = Rote Einfärbung, wenn das Gebäude hier nicht Plaziert werden darf
   *)
  ctd_Buy_Preview_Layer = -0.4; // Buy Preview

  {---- Alles Ab hier sind "Menü Anzeigen" ----}

  (*
   * -0.05 = ctd_MapBlackOutLayer - 1 * ctd_EPSILON = Range eines Selektierten Gebäudes beim Hinting / im Selekted mode
   *  0.0  = ctd_MapBlackOutLayer                   = Schwarzer Untergrund unter dem Menü, welcher evtl zu viel gerenderte Teile der Karte Ausgraut
   *)
  ctd_MapBlackOutLayer = 0.0; // Alles was mit der Karte zu tun hat, muss < diesem Wert sein

  (*
   * 0.3  = ctd_BuyEditorLayer               = Kacheln
   * 0.35 = ctd_BuyEditorLayer + ctd_EPSILON = Buy Buttons (Die Gebäude-Buttons)
   *)
  ctd_BuyEditorLayer = 0.3; // Der BuyEditor

  (*
   * 0.6  = ctd_Menu_Layer                   = Listbox, Edit ...
   * 0.65 = ctd_Menu_Layer + 1 * ctd_EPSILON = Preview Map-Kacheln
   * 0.7  = ctd_Menu_Layer + 2 * ctd_EPSILON = Preview Map-Placements
   * 0.75 = ctd_Menu_Layer + 3 * ctd_EPSILON = Preview Map-Boden Gegner
   * 0.80 = ctd_Menu_Layer + 4 * ctd_EPSILON = Preview Map-Gebäude
   * 0.85 = ctd_Menu_Layer + 5 * ctd_EPSILON = Preview Map-Luft Gegner
   *)
  ctd_Menu_Layer = 0.6; // Map Preview, Chat, Auswahl Buttons , OnScreenTexte

  (*
   * 0.9  = ctd_Tipp_Layer                     = Schwarzer Hintergrund hinter dem Tipp
   * 0.95 = ctd_Tipp_Layer + 1 * ctd_EPSILON    = Der Tooltipp Text
   * 0.975 = ctd_Tipp_Layer + 1.5 * ctd_EPSILON = Die Bilder des Tooltipp
   *)
  ctd_Tipp_Layer = 0.9; // Die ToolTipps und teile des Sidemenüs

Type

  TLogLevel = (llTrace, lldebug, llInfo, llWarning, llError, llCritical, llFatal);

  TLogShowHandler = Procedure(Msg: String; WarnLevel: TLogLevel);

  TSlowDown = Record
    slowdownstatic: single;
    slowdowndynamic: single;
    slowdowntime: integer;
  End;

  TUpdatingState = (usIdleInactive, usInProgress);

  TUpdating = Record
    StartTime: int64;
    FinState: integer;
    State: TUpdatingState;
  End;

  TGameState = (
    gs_WaitForJoin, // Der Benutzer wird zum "Server erstellen oder Joinen" aufgefordert.
    gs_EditMode, // Wir sind als Client verbunden und Editieren fleißig die Karte
    gs_WaitToStart, // Der Server hat eine neue Runde gestartet, wir warten bis es los geht
    gs_Loading, // Der Client hat eine neue Karte beim Server Angefragt und Lädt diese nun, Während des laden wird dann ein Fortschrittsbalken angezeigt
    gs_Gaming // Das Spiel läuft
    );

  TUpdateEvent = Procedure(Player: integer; Reason, ID2: integer) Of Object; // Wenn die Karte Events auslösen muss (z.B. Einheit gekillt, Einheit verringert Spieler leben ..)

  // Sonst gibts ne Circuläre Abhängigkeit ..
  tctd_BulletObject = Class
  private
  public
  End;

  TPower = Array[0..3] Of integer; // !! ACHTUNG !! An ettlichen Stellen ist Hardcodiert, das dieses Array 0..3 ist, Wenn das geändert wird knallts an allen Ecken und Enden

  THintKind = (hkUnknown, hkOpponent, hkbuilding, hkHero);

  THint = Record
    Kind: THintKind;
    Name: String;
    Power: TPower;
    PowerSum: integer;
    StageCount: Integer; // nur bei hkbuilding gültig
    Stage: integer; // nur bei hkbuilding gültig
    Description: String;
    TotalLivePoints: integer; // nur bei hkOpponent
  End;

  TRenderOpponent = Record // Datensatz, welcher für die Gegnerübertragen wird
    Index: uint16; // Index für FIndexMapper
    Identifier: uint16;
    AnimationOffset: uint16;
    Position: TVector2;
    Angle: int16;
    LifePoints: TPower;
  End;

  TRenderHero = Record // Datensatz, welcher für die Heros übertragen wird
    Position: TVector2;
    AnimationOffset: uint16;
    Angle: int16;
    CollectedDamages: uint32; // Den Schaden, den der Hero in diesem Level bereits "gesammelt" hat
    Level: uint16; // Das Aktuelle Level
    Moving: Boolean; // Wenn True, dann bewegt sich der Held gerade
  End;

  TRenderBullet = Record // Datensatz, welcher für die Bullets übertragen wird
    Index: uint16; // Index für FIndexMapper
    Position: TVector2;
    AnimationOffset: uint16;
    Angle: int16;
  End;

  TBuildingStrategy = (bsFirst, bsLast, bsNearest, bsFarest, bsStrongest, bsWeakest, bsRandom);

{$IFDEF Server}
Const
  MapBlockSize = 1; // Der Server Rechnet alles 1:1, damit kann der Client direkt mit Blockwidth hochscallieren
  SpawnGrid = 0.6; // Grid nach welchem TOpponents beim Spawning angeordnet werden
  MinDistanceBetweenOpponents = 0.6; // Mindestabstand zwischen 2 Oppoents
  MinDistanceBetweenHeros = 2; // Mindestabstand zwischen 2 Helden
{$ENDIF}
{$IFDEF Client}
Const
  TextrureImportMapBlockSize = 10; // Beim Importieren werden alle Graphiken entsprechend dieses Wertes Autoskalliert

{$ENDIF}
Var
{$IFDEF Client}
  MapBlockSize: integer = 10; // Blockgröße eines Blocks
  SpawnGrid: integer = 6; // Grid nach welchem TOpponents beim Spawning angeordnet werden
  BuildBuildingTex: TGraphikItem = (image: 0; Name: ''; IsAlphaImage: false; Stretched: smNone; OrigWidth: 0; OrigHeight: 0; StretchedWidth: 0; StretchedHeight: 0);
{$ENDIF}
  MapFolder: String = ''; // Egal ob Server oder Client, das ist der Pfad in dem alle Karten abgelegt sind (beim Client Shared beim Server Maps)
  MapName: String = ''; // Die Aktuelle Karte

  LogShowHandler: TLogShowHandler = Nil; // Debendency Injection auf eine LogShowMsg, wenn es die nicht gibt wird die von ulogger.pas genommen

  (*
   * Listet alle Unterverzeichnisse in RootDir (ohne RootDir, nicht Rekursiv)
   *)
Function ListAllSubdirs(Const RootDir: String): TStringList;

Function StringToPos(s: String): TPoint;

Function Serialize(Const Value: String): String;
Function DeSerialize(Const Value: String): String;

Function CRC_of_File(Const Value: TFilename): uint32;

Function WriteAccessToDirectory(Const Dir: String): Boolean;

{$IFDEF Client}
Procedure RenderLifeBar(sizex, sizey, lifepointspercent: Single);

Procedure RenderMoveable(
  Image, Direction: integer;
  Sizex, sizey, lifepointspercent: Single;
  ShowLifePoints: Boolean
  );
Procedure RenderMoveableAnim(
  AnimationOffset: integer;
  Const Animation: TOpenGL_Animation; Direction: integer;
  Sizex, sizey, lifepointspercent: Single;
  ShowLifePoints: Boolean
  );
Procedure RenderObj(Middle: Tpoint; Width, Height: Integer; Texture: integer; Rotation: integer = 0); Deprecated 'Sollten alle durch RenderObjItem ersetzt werden.';
Procedure RenderObjItem(Middle: Tpoint; Width, Height: Integer; Texture: TGraphikItem; Rotation: integer = 0);
Procedure RenderAnim(Middle: Tpoint; Width, Height: Integer; Const Animation: TOpenGL_Animation; Rotation: integer = 0);

Function LoadFileToMyPath(FileName: String; Foldername: String = ''): Boolean;
Procedure FixFormPosition(Const Form: TForm); // Rückt ein Formular wieder in den Screen, sollte es außerhalb des Sichtbaren sein
{$ENDIF}

Function PointInRect(P: classes.TPoint; r: classes.TRect): Boolean;

Procedure Nop;

Procedure InitOpenDialog(Const Dialog: TOpenDialog; Path: String);

Function Ceilp2(Value: uInt32): uInt32;

Procedure LogShow(LogText: String; LogLevel: TLogLevel = llInfo);
Procedure Log(LogText: String; LogLevel: TLogLevel = llInfo);
Procedure LogLeave;
Function GetLoggerLoglevel(): integer;
Procedure InitLogger();
{$IFDEF Windows}
Procedure EnableLogToConsole();
{$ENDIF}
Procedure SetLoggerLogFile(Filename: String);
Procedure SetLogLevel(Level: integer);
Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel = llInfo); // Logt nur wenn "Criteria" = true
Function LogLevelToString(LogLevel: TLogLevel): String;

Function GetTick: int64 Inline; // Wrapper für Gettickcount

// Macht das Gegenteil von TServer.HandleRequestSavegames -> Entfernt also was der anfügt..
Function RemoveTimestampInfoFromFilename(Info: String): String;

Function prettyTime(TimeInMs: int64): String; // Code entliehen aus CCM

Procedure RestartApplication();

Implementation

Uses FileUtil, LazUTF8, LazFileUtils, math
{$IFDEF Client}
  , dglOpenGL
{$ENDIF}
  , process, UTF8Process
  , ucrc;

// Taken from: https://forum.lazarus.freepascal.org/index.php?topic=17747.0
Procedure RestartApplication();
Var
  aProcess: TProcessUTF8;
Begin
  aProcess := TProcessUTF8.Create(Nil);
  aProcess.Executable := Application.ExeName;
  aProcess.Execute;
  aProcess.Free;
  Application.Terminate;
End;

Function prettyTime(TimeInMs: int64): String; // Code entliehen aus CCM
Var
  suffix: String;
  rest: int64;
  Time_In_Seconds: int64;
Begin
  Time_In_Seconds := TimeInMs Div 1000;
  suffix := 's';
  rest := 0;
  If Time_In_Seconds > 60 Then Begin
    suffix := 'min';
    rest := Time_In_Seconds Mod 60;
    Time_In_Seconds := Time_In_Seconds Div 60;
  End;
  If Time_In_Seconds > 60 Then Begin
    suffix := 'h';
    rest := Time_In_Seconds Mod 60;
    Time_In_Seconds := Time_In_Seconds Div 60;
  End;
  If (Time_In_Seconds > 24) And (suffix = 'h') Then Begin
    suffix := 'd';
    rest := Time_In_Seconds Mod 24;
    Time_In_Seconds := Time_In_Seconds Div 24;
  End;
  If suffix <> 's' Then Begin
    If rest < 10 Then Begin
      result := inttostr(Time_In_Seconds) + ':0' + inttostr(rest) + suffix;
    End
    Else Begin
      result := inttostr(Time_In_Seconds) + ':' + inttostr(rest) + suffix;
    End;
  End
  Else Begin
    result := inttostr(Time_In_Seconds) + suffix;
  End;
End;

Function ConvertLogLevel(ll: TLogLevel): ulogger.TLogLevel;
Begin
  Case ll Of
    llTrace: result := ulogger.llTrace;
    lldebug: result := ulogger.lldebug;
    llInfo: result := ulogger.llInfo;
    llWarning: result := ulogger.llWarning;
    llError: result := ulogger.llError;
    llCritical: result := ulogger.llCritical;
    llFatal: result := ulogger.llFatal;
  Else
    Raise Exception.Create('ConvertLogLevel: Hier ist was kaputt.');
  End;
End;

Procedure Log(LogText: String; LogLevel: TLogLevel);
Begin
  ulogger.Log(LogText, ConvertLogLevel(LogLevel));
End;

Procedure LogLeave;
Begin
  ulogger.LogLeave;
End;

Function GetLoggerLoglevel: integer;
Begin
  result := Logger.loglevel;
End;

{$IFDEF Server}

Procedure InitLogger();
Begin
  logger.LogToConsole := true;
  logger.LogToFile := false;
  logger.AutoLogStackOnFatal := true;
  logger.LogStackTrace := true;
  logger.SetLogLevel(2);
End;
{$ELSE}

Procedure InitLogger;
Begin
  logger.LogToFile := false;
  logger.SetLogLevel(2);
  logger.AutoLogStackOnFatal := true;
  logger.LogStackTrace := true;
{$IFDEF Linux}
  logger.LogToConsole := true;
{$ENDIF}
End;
{$ENDIF}

{$IFDEF Windows}

Procedure EnableLogToConsole();
Begin
  logger.LogToConsole := true;
End;
{$ENDIF}

Procedure SetLoggerLogFile(Filename: String);
Begin
  // Todo : Theoretisch müsste man hier prüfen ob auch Schreibrechte existieren..
  logger.SetLogFilename(Filename);
  logger.LogToFile := true;
End;

Procedure SetLogLevel(Level: integer);
Begin
  logger.SetLogLevel(level);
End;

Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel);
Begin
  // Alles muss durch Logshow geschleift werden
  If Criteria Then Begin
    LogShow(LogText, LogLevel);
  End;
End;

Function LogLevelToString(LogLevel: TLogLevel): String;
Begin
  result := ulogger.LogLevelToString(ConvertLogLevel(LogLevel));
End;

Procedure LogShow(LogText: String; LogLevel: TLogLevel);
Begin
  If assigned(LogShowHandler) Then Begin
    LogShowHandler(LogText, LogLevel);
  End
  Else Begin
{$IFDEF Server}
    log(LogText, LogLevel);
{$ELSE}
    ulogger.LogShow(LogText, ConvertLogLevel(LogLevel));
{$ENDIF}
  End;
End;

Procedure Nop;
Begin

End;

Function ListAllSubdirs(Const RootDir: String): TStringList;
Var
  sr: TSearchRec;
Begin
  result := TStringList.Create;
  If FindFirstutf8(IncludeTrailingPathDelimiter(RootDir) + '*', faDirectory, sr) = 0 Then Begin
    Repeat
      If ((sr.Attr And faDirectory) = faDirectory) And
        (sr.Name <> '.') And
        (sr.Name <> '..') Then Begin
        result.Add(sr.name);
      End;
    Until FindNextUTF8(sr) <> 0;
  End;
  FindCloseUTF8(sr);
End;

Function StringToPos(s: String): TPoint;
Begin
  s := trim(s);
  result.x := strtointdef(copy(s, 1, pos('x', s) - 1), 0);
  result.y := strtointdef(copy(s, pos('x', s) + 1, length(s)), 0);
End;

Function DeSerialize(Const Value: String): String;
Var
  i, j: integer;
Begin
  result := '';
  setlength(result, length(value) * 2);
  j := 1;
  i := 1;
  While i <= length(value) Do Begin
    If value[i] = '#' Then Begin
      inc(i);
      If i > length(value) Then Begin
        Raise exception.create('Error invalid deserialize String "' + value + '"');
      End;
      Case Value[i] Of
        '#': Begin
            result[j] := '#';
            inc(j);
          End;
        '+': Begin
{$IFDEF Windows}
            result[j] := LineEnding[1];
            result[j + 1] := LineEnding[2];
            inc(j, 2);
{$ELSE}
            result[j] := LineEnding;
            inc(j);
{$ENDIF}
          End
      Else Begin
          Raise exception.Create('Error "' + value[i] + '" Not known as deserialize param.');
        End;
      End;
      inc(i);
    End
    Else Begin
      result[j] := value[i];
      inc(i);
      inc(j);
    End;
  End;
  setlength(result, j - 1);
End;

Function Serialize(Const Value: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 1 To length(value) Do Begin
    Case value[i] Of
{$IFDEF Windows}
      #10: Begin // Das Muss Geschluckt werden
        End;
      #13: Begin
{$ELSE}
      LineEnding: Begin
{$ENDIF}
          result := result + '#+';
        End;
      '#': Begin
          result := result + '##';
        End
    Else Begin
        result := result + value[i];
      End;
    End;
  End;
End;

Function CRC_of_File(Const Value: TFilename): uint32;
Var
  f: TFileStream;
  m: TMemoryStream;
Begin
  f := TFileStream.Create(value, fmOpenRead);
  m := TMemoryStream.Create;
  m.CopyFrom(f, f.Size);
  f.free;
  m.Position := 0;
  result := $FFFFFFFF;
  CalcCRC32(m, Result);
  m.free;
End;

Function WriteAccessToDirectory(Const Dir: String): Boolean;
Var
  f: TextFile;
  s: String;
  tries: integer;
Begin
  result := false;
  s := IncludeTrailingPathDelimiter(dir) + inttostr(random(1000000)) + '.txt';
  tries := 0;
  While FileExistsUTF8(s) Do Begin
    s := IncludeTrailingPathDelimiter(dir) + inttostr(random(1000000)) + '.txt';
    inc(tries);
    If tries > 1000 Then Begin
      Raise exception.create('Error unable to create a unused filename in :' + dir + ' could not check, if writeaccess or not.');
    End;
  End;
  // Versuch etwas zu schreiben
  Try
    AssignFile(f, utf8tosys(s));
    rewrite(f);
    writeln(f, 'Hallo');
  Finally
    CloseFile(f);
  End;
  // Wir konnten die Datei erstellen, dann haben wir schreibrechte
  If FileExistsUTF8(s) Then Begin
    result := DeleteFileUTF8(s);
  End;
End;

{$IFDEF Client}

Procedure RenderLifeBar(sizex, sizey, lifepointspercent: Single);
Var
  d, t, l, w, h: Single;
Begin
  d := max(sizex * MapBlockSize, SizeY * MapBlockSize);
  // Mehr oder weniger das selbe steht auch bei TBuilding
  glPushMatrix;
  // TODO: Ohne tiefentest sieht es zwar gut aus, aber leider im Menü dann doof
  //glDisable(GL_DEPTH_TEST);
  t := -d * 0.5 - LifebarOffset; //-SizeY * 0.5 * MapBlockSize - LifebarOffset;
  l := -sizex * MapBlockSize / 2;
  h := LifebarHeight;
  w := sizex * MapBlockSize;
  glBindTexture(GL_TEXTURE_2D, 0);
  glbegin(GL_QUADS);
  glColor3f(0, 0, 0);
  glVertex2f(l - 1, t - 1);
  glVertex2f(l + 1 + w, t - 1);
  glVertex2f(l + 1 + w, t + h + 1);
  glVertex2f(l - 1, t + h + 1);
  glend;
  Case round(lifepointspercent * 100) Of
    71..100: glColor(Good_Col);
    31..70: glColor(Middle_Col);
    0..30: glColor(Bad_col);
  End;
  w := sizex * MapBlockSize * lifepointspercent;
  glTranslatef(0, 0, ctd_EPSILON);
  glbegin(GL_QUADS);
  glVertex2f(l, t);
  glVertex2f(l + w, t);
  glVertex2f(l + w, t + h);
  glVertex2f(l, t + h);
  glend;
  //glenable(GL_DEPTH_TEST);
  glPopMatrix;
End;

Procedure RenderMoveable(Image, Direction: integer; Sizex, sizey,
  lifepointspercent: Single; ShowLifePoints: Boolean);
Begin
  glPushMatrix;
  glTranslatef((SizeX * MapBlockSize) / 2, -(Sizey * MapBlockSize) / 2 + MapBlockSize, 0);
  If ShowLifePoints Then Begin
    glPushMatrix;
    glTranslatef(0, 0, ctd_EPSILON);
    RenderLifeBar(SizeX, SizeY, lifepointspercent);
    glPopMatrix;
  End;
  glColor4f(1, 1, 1, 1);
  RenderObj(point(0, 0), round(SizeX * MapBlockSize), round(SizeY * MapBlockSize), image, Direction);
  glPopMatrix;
End;

Procedure RenderMoveableAnim(AnimationOffset: integer;
  Const Animation: TOpenGL_Animation; Direction: integer; Sizex, sizey,
  lifepointspercent: Single; ShowLifePoints: Boolean);
Var
  ao: integer;
Begin
  glPushMatrix;
  glTranslatef((SizeX * MapBlockSize) / 2, -(Sizey * MapBlockSize) / 2 + MapBlockSize, 0);
  If ShowLifePoints Then Begin
    glPushMatrix;
    glTranslatef(0, 0, ctd_EPSILON);
    RenderLifeBar(SizeX, SizeY, lifepointspercent);
    glPopMatrix;
  End;
  glColor4f(1, 1, 1, 1);
  ao := Animation.AnimationOffset;
  Animation.AnimationOffset := AnimationOffset;
  RenderAnim(point(0, 0), round(SizeX * MapBlockSize), round(SizeY * MapBlockSize), Animation, Direction);
  Animation.AnimationOffset := ao;
  glPopMatrix;
End;

Procedure RenderObj(Middle: Tpoint; Width, Height: Integer; Texture: integer;
  Rotation: integer);
Begin
  // Alles was einen Alpha Kanal von > 0.5 hat, wird weggeschnitten
  // --> Harte Transparenz möglich, ohne das der Tiefenpuffer von Transparenten Teilen überschrieben wird
  glAlphaFunc(GL_LESS, 0.5);
  glEnable(GL_ALPHA_TEST);
  uopengl_graphikengine.RenderQuad(Middle, Width, -height, Rotation, texture);
  gldisable(GL_ALPHA_TEST);
End;

Procedure RenderObjItem(Middle: Tpoint; Width, Height: Integer;
  Texture: TGraphikItem; Rotation: integer);
Begin
  glAlphaFunc(GL_LESS, 0.5);
  glEnable(GL_ALPHA_TEST);
  glPushMatrix;
  glTranslatef(Middle.X, Middle.Y, 0);
  glScalef(Width / Texture.OrigWidth, height / Texture.OrigHeight, 1);
  uopengl_graphikengine.RenderQuad(v2(0, 0), Rotation, texture);
  glPopMatrix;
  gldisable(GL_ALPHA_TEST);
End;

Procedure RenderAnim(Middle: Tpoint; Width, Height: Integer;
  Const Animation: TOpenGL_Animation; Rotation: integer);
Begin
  // Alles was einen Alpha Kanal von > 0.5 hat, wird weggeschnitten
  // --> Harte Transparenz möglich, ohne das der Tiefenpuffer von Transparenten Teilen überschrieben wird
  If Animation.Sprite[0].AlphaImage Then Begin // -> Ganz Sauber ist das ja eigentlich nicht, da die Animation ja Theoretisch aus mehreren Graphiken bestehen könnte..
    glAlphaFunc(GL_LESS, 0.5);
    glEnable(GL_ALPHA_TEST);
  End;
  glPushMatrix();
  glTranslatef(round(-Width / 2 + middle.X), round(-Height / 2 + middle.y), 0); // Durch das Runden treten "weniger" Schwarze Streifen unterhalb des Bildes auf.
  glScalef(Width / Animation.Width(Rotation), Height / Animation.Height(Rotation), 1);
  Animation.Render(Rotation);
  glPopMatrix();
  If Animation.Sprite[0].AlphaImage Then Begin
    gldisable(GL_ALPHA_TEST);
  End;
End;

Function LoadFileToMyPath(FileName: String; Foldername: String): Boolean;
Var
  s: String;
  crc_d, crc_s: UInt32;
Begin
  result := false;
  If Foldername = '' Then Begin
    Foldername := MapFolder + MapName;
  End;
  Foldername := IncludeTrailingPathDelimiter(Foldername);
  s := Foldername + ExtractFileName(FileName);
  If FileName <> s Then Begin
    If FileExistsUTF8(s) Then Begin
      // Die Zieldatei existiert auch schon, sind beide Gleich ?
      crc_s := CRC_of_File(Filename);
      crc_d := CRC_of_File(s);
      result := crc_s = crc_d;
      If Not result Then Begin
        // Die Textur existiert mit dem Selben Dateinamen, ist aber nicht die Gleiche
        logshow(format('File %s already exists, but is different to %s', [s, FileName]), llWarning);
      End;
    End
    Else Begin
      result := CopyFile(utf8tosys(Filename), utf8tosys(s));
    End;
  End
  Else Begin
    // Die Datei ist schon da wo sie hin gehört.
    result := true;
  End;
End;

Procedure FixFormPosition(Const Form: TForm);
Var
  i, j: integer;
  p: Array[0..3] Of Tpoint;
Begin
  // Mit einem 30 Pixel Rahmen suchen wir ob das Fenster irgendwo zu sehen ist..
  p[0] := point(form.Left + 30, form.Top + 30);
  p[1] := point(form.Left + form.Width - 30, form.Top + 30);
  p[2] := point(form.Left + form.Width - 30, form.Top + form.Height - 30);
  p[3] := point(form.Left + 30, form.Top + form.Height - 30);
  For i := 0 To screen.MonitorCount - 1 Do Begin
    For j := 0 To 3 Do Begin
      If PointInRect(p[j], Screen.Monitors[i].BoundsRect) Then Begin
        exit;
      End;
    End;
  End;
  // Wenn wir es bis Hier her geschafft haben, dann ist das Formular nirgends zu sehen
  // 1. Suchen des Monitors auf dem Die Maus ist
  For i := 0 To screen.MonitorCount - 1 Do Begin
    // 2. Fenster auf den gefundenen Monitor schieben
    If PointInRect(Mouse.CursorPos, screen.Monitors[i].BoundsRect) Then Begin
      form.left := (screen.Monitors[i].width - form.width) Div 2 + screen.Monitors[i].BoundsRect.left;
      form.top := (screen.Monitors[i].height - form.height) Div 2 + screen.Monitors[i].BoundsRect.top;
      exit;
    End;
  End;
  // Der Mauscurser ist auf Keinem Monitor -> Zentrieren auf dem Primärmonitor
  form.left := (screen.Monitors[0].width - form.width) Div 2 + screen.Monitors[0].BoundsRect.left;
  form.top := (screen.Monitors[0].height - form.height) Div 2 + screen.Monitors[0].BoundsRect.top;
End;

{$ENDIF}

Function PointInRect(P: classes.TPoint; r: classes.TRect): Boolean;
Begin
  result := (p.x >= min(r.Left, r.Right)) And (p.x <= max(r.Left, r.Right)) And
    (p.y >= min(r.Bottom, r.Top)) And (p.y <= max(r.Bottom, r.Top));
End;

Procedure InitOpenDialog(Const Dialog: TOpenDialog; Path: String);
Begin
  path := IncludeTrailingPathDelimiter(path);
{$IFDEF Windows}
  // Geprüft funktioniert unter Windows 7
  Dialog.FileName := path;
{$ENDIF}
{$IFDEF Linux}
  // So stimmts ab dem 2. Dialog.execute, aber warum nicht ab dem 1. ?
  Dialog.InitialDir := ExcludeTrailingPathDelimiter(path);
{$ENDIF}
End;

Function Ceilp2(Value: uInt32): uInt32;
Begin
  // Eine Zweierpotenz sieht Binär wie folgt aus 10...0
  // Durch subtraktion von 1 ergibt sich         01...1
  // Diese einsen können für eine 32Bit Zahl durch geschicktes "or" bilden in
  // Log(32) Schritten erzeugt werden
  // Am Schluss muss lediglich wieder 1 Addiert werden ;)
  value := value - 1;
  value := value Or (value Shr 1);
  value := value Or (value Shr 2);
  value := value Or (value Shr 4);
  value := value Or (value Shr 8);
  value := value Or (value Shr 16);
  result := value + 1;
End;

Function GetTick: int64;
Begin
{$IFDEF WinXPMode}
  result := GetTickCount;
{$ELSE}
  result := GetTickCount64;
{$ENDIF}
End;

Function RemoveTimestampInfoFromFilename(Info: String): String;
Var
  i: integer;
Begin
  result := Info; // Wenn es gar keine Info gibt
  i := pos('|', Info);
  If i <> 0 Then Begin
    result := copy(info, i + 2, length(info));
  End;
End;

End.


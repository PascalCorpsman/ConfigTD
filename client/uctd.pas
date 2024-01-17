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
Unit uctd;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils, OpenGLContext, Graphics, Controls,

  lNetComponents, lnet, uopengl_graphikengine,

  uchunkManager, uctd_common, uOpenGL_WidgetSet, uctd_map, uvectormath,
  uctd_splashhintmanager, uctd_mapobject, uctd_building, uctd_opp, uctd_hero,
  uSplashMarkManager;

Type

  TUpdateMapPropertyEvent = Procedure(Sender: TObject; MapProperty: Integer; Const Data: TStream) Of Object;
  TEndRoundEvent = Procedure(Sender: TObject; Succeed: Boolean; Round: Integer) Of Object;
  TOnGetStringListEvent = Procedure(Sender: TObject; Const Data: TStringlist) Of Object;
  TOnGetBooleanEvent = Procedure(Sender: TObject; Suceed: Boolean) Of Object;
  TOnGetStreamEvent = Procedure(Sender: TObject; Const Data: TStream) Of Object;
  TOnWaveCloneEvent = Procedure(Sender: TObject; SourceWaveNum, DestWaveNum: Integer) Of Object;

  TOnShowGameStatistics = Procedure(Msg: String; Const Data: TStream) Of Object; // Zum Anzeigen der Spielstatistik am Ende des Spieles

  TMapPrieviewInfo = Record
    Image: TBitmap;
    Description: String;
  End;

  TOnGetMapPrieviewInfoEvent = Procedure(Sender: TObject; Const Data: TMapPrieviewInfo) Of Object;

  TFileData = Record
    MapName: String;
    Filename: String;
  End;

  TNeedLoadMapOnReceivingFinish = Record
    Need: Boolean;
    MapName: String;
    NeedGamingData: Boolean;
    Data: TMemoryStream;
    NeedRefreshBackTex: Boolean;
    NeedRefreshDC1: Boolean;
    NeedRefreshDC2: Boolean;
    NeedRefreshDC3: Boolean;
    NeedRefreshDC4: Boolean;
  End;

  TSubBuyMenu = Record
    Kind: TBuyAbleKind;
    obj: tctd_mapopbject; // Ein Vorgeladenes Map Object für den Privaten Hausgebrauch ;)
    Item: String; // Dateiname des Gebäudes
    Count: Integer; // Wieviele dürfen davon auf der Karte gebaut werden
    Category: String; // Die Kategorie des Gebäudes -- Future Use
  End;

  // Liste aller Verfügbaren / Kaufbaren Gebäude für die Aktuelle Wave
  TBuyMenu = Record
    Items: Array Of TSubBuyMenu;
    MaxItemSize: TPoint; // Die Maximalen Ausdehnungen in x und y Richtung
  End;

  // Zur Anzeige von "Hint" informationen an der Mausposition wird hier ein Object abgelegt
  THintObject = Record
    Obj: tctd_mapopbject; // Das Objekt
    Time: int64; // Der zeitpunkt an welchem das Objekt zugeordnet wurde
  End;

  TPlayerInfos = Record
    uid: integer;
    Cash: integer;
    Lives: integer;
    Kills: integer;
    BonusFinisher: integer; // Anzahl der Bonus Gegner welche das Ziel Erreicht haben und gezählt werden müssen wie ein Kill
    KillsOnWaveStart: integer;
    LivesOnWaveStart: integer;
    Name: String;
  End;

  TOpenGame = Record
    IP: String;
    Name: String;
    Port: String;
    LastValidTimeStamp: Int64;
  End;

  TMenuPosition = (mpBottom, mpRight);

  { TBuildingStrategyButton }

  TBuildingStrategyButton = Class(TOpenGL_BaseClass)
  private
    Fimage: integer;
    FSelectedImage: integer;
  protected
    Procedure OnRender(); override;
  public
    Strategy: TBuildingStrategy;
    Selected: Boolean;
    Property OnClick;
    Procedure Click; override;
    Constructor Create(Owner: TOpenGLControl); override;
    Procedure SetImage(Filename: String);
  End;

  { TCTDinfofield }

  TCTDinfofield = Class(TOpenGl_BaseClass)
  private
    ftexture: integer;
  protected
    Procedure OnRender(); override;
  public
    Text: String;
    Constructor Create(Owner: TOpenGLControl; Texture: String); reintroduce;
  End;

  { TCTDDualinfoField }

  TCTDDualinfoField = Class(TCTDinfofield)
  protected
    Procedure OnRender(); override;
  public
    Text2: String;
  End;

  TOnGetBooleanEventRecord = Record
    Callback: TOnGetBooleanEvent;
    s, d: String;
  End;

  THolder = Record
    Identifier: Integer;
    Used: Boolean; // Nur wenn das True ist, ist der Eintrag Gültig, das Spart die New und Dispose
    (*
     * Es ist immer nur einer der 3 Aktiv, das machen aber die Aufrufer unter sich auf
     *)
    StringListEventCallback: TOnGetStringListEvent;
    MapPrieviewInfoEventCallback: TOnGetMapPrieviewInfoEvent;
    BooleanEventRecord: TOnGetBooleanEventRecord;
    // Todo: alles so umschreiben, dass die 3 Events oben wieder Raus fliegen und alles über StreamEvents abgearbeitet wird, die Record variante wäre dann eine die nen Userpointer mit führen darf..
    StreamEvent: TOnGetStreamEvent;
  End;

  { TCallbackHolder }

  TCallbackHolder = Class
  private
    fHolder: Array Of THolder;
  public
    Constructor Create;
    Destructor Destroy; override;

    Function AddWaitIdentifier(Identifier: integer; Callback: TOnGetStringListEvent): Boolean; overload;
    Function AddWaitIdentifier(Identifier: integer; Callback: TOnGetMapPrieviewInfoEvent): Boolean; overload;
    Function AddWaitIdentifier(Identifier: integer; Callback: TOnGetBooleanEventRecord): Boolean; overload;
    Function AddWaitIdentifier(Identifier: integer; Callback: TOnGetStreamEvent): Boolean; overload;

    Function GetIdentifierStringListEventCallback(Identifier: integer): TOnGetStringListEvent;
    Function GetIdentifierMapPrieviewInfoEventCallback(Identifier: integer): TOnGetMapPrieviewInfoEvent;
    Function GetIdentifierBooleanEventCallback(Identifier: integer): TOnGetBooleanEventRecord;
    Function GetIdentifierStreamEventCallback(Identifier: integer): TOnGetStreamEvent;
  End;

  (*
   * Das Ding ist so ein Pfush
   *)
  TOpenGl_HintImage = Class(TOpenGl_Image)
  public
    Hint: String;
  End;

  { Tctd }

  Tctd = Class
  private
    fDashSeparator: String; // die "---" im Build menü auf die Richtige Länge skalliert
    // Die Game Info Elemente
    fTargetsInfo: TCTDinfofield;
    fCoinsInfo: TCTDinfofield;
    fLivesInfo: TCTDinfofield;
    fWaveinfo: TCTDDualinfoField;

    fHostButton: TOpenGl_Button;
    fJoinButton: TOpenGl_Button;

    fNewMapButton: TOpenGl_Button;
    fLoadMapButton: TOpenGl_Button;
    fLoadGameButton: TOpenGl_Button;

    fSideMenuVisible: Boolean; // True, dann ist das Seitenmenü (unten oder Rechts) sichtbar
    fMenuPosition: TMenuPosition;
    fSH: TCallbackHolder; // Der Handler, der sich die Notwendigen Callbacks merkt, die Zeitverzögert Daten nach Reichen
    fOpenGames: Array Of TOpenGame; // Liste aller via UDP- Ermittelten offenen Spiele
    fUDPConnection: TLUDPComponent; // Die UDP Componente, mit welcher wir via Boradcast nach offenen Servern fragen

    fSplashMarks: TSplashMarkManager; // Zum Anzeigen von Ein und Ausblendenden Ausrufezeichen auf der Karte
    fSplashhints: TSplashHintManager; // Zum Anzeigen von Textnachrichten auf dem Screen
    fPlayerInfos: Array Of TPlayerInfos;
    fPlayerIndex: integer; // Der Index in FPlayer auf dem Server
    fAktualWave: Integer;
    fpausing: Boolean;
    fShowBuildableTiles: Boolean;
    fPassword: String;
    FuserName: String;

    Level_Up_Image: TOpenGl_HintImage;
    Sell_Image: TOpenGl_HintImage;
    Tab_Image: TOpenGl_Image;
    BuildingStrategyButtons: Array[0..7] Of TBuildingStrategyButton;

    fServerUid: Integer; // Die vom Server Zugeteilte UID
    fstatebeforeloading, fgameState: TGameState;
    fMap: TMap; // Die Aktuelle Karte
    fChunkManager: TChunkManager;
    FReceivingQueue: Array Of TFileData;
    fFileReceivingCount, fFileReceivingGoalCount: integer; // Anzahl der Dateien die der Client gleich alle Anfragen muss..
    FOnReceivingFilesFinish: TNeedLoadMapOnReceivingFinish;
    FBuyMenu: TBuyMenu; // Im Spiel das Building menü
    FHintObject: THintObject; // Zur Anzeige von "Hint" informationen an der Mausposition wird hier ein Object abgelegt
    fBuyingObject: tctd_mapopbject; // Das Objekt, welches der Benutzer gerade zum "Kaufen" Plazieren ausgewählt hat, ! Achtung ! Die Instanz muss via Free freigegeben werden.
    fSideMenuObject: tctd_mapopbject;
    fSidemenuOpponent: TOpponent; // Zum Umgang mit dem Nebeneffekt von TMap.GetObjUnderCursor brauchen wir einen Dummy, welcher uns den Identifier speichert
    fSelectedBuildings: TBuildingArray; // Quasi Redundant zum fSideMenuObject aber nur für Gebäude, so das hier viele Angewählt werden können
    fSelectedHeros: THeroArray; // Quasi Redundant zum fSideMenuObject aber nur für Helden, so das hier viele Angewählt werden können

    fsx, fsy: integer; // Fürs Karten Scrolling
    fmDownPos: TPoint; // Die Mausposition beim MouseDown Event
    fmScrollPos: TPoint; // Die Mausposition Fürs Karten Scrolling
    fBuyKachel: TGraphikItem; // OpenGL Bild für die HintergrundKachel im Buy Menü
    fStartRoundTick: int64;

    fCaptured_OpenGLControl: TOpenGLControl; // Das OpenGLControl, welches die Events weiter leitet
    FOnMouseDownCapture: TMouseEvent; // Originaler OnMouseDown Event
    FOnMouseUpCapture: TMouseEvent; // Originaler OnMouseUp Event
    FOnMouseMoveCapture: TMouseMoveEvent; // Originaler OnMouseMove Event
    FOnDblClickCapture: TNotifyEvent;
    FOnKeyDownCapture: TKeyEvent;
    FOnKeyUpCapture: TKeyEvent;
    fCTRLPressed: Boolean; // True, wenn die CTRL taste gedrückt wurde

    (*
     * Größen Informationen in Pixel für den Renderbereich der Karte
     *)
    fMapT: integer; // Top / Left Koordinate
    fMapL: integer;
    fMapH: integer; // Breite / Höhe in Pixel
    fMapW: integer;
    (*
     * Größen Informationen in Pixel für den Renderbereich des Preview Bereiches
     *)
    fPreviewT: integer;
    fPreviewL: integer;
    fPreviewH: integer;
    fPreviewW: integer;
    (*
     * Größen Informationen in Pixel für den Renderbereich des Bau Bereiches
     *)
    fBuildT: integer;
    fBuildL: integer;
    fBuildH: integer;
    fBuildW: integer;
    (*
     * Größen Informationen in Picel für den Renderbereich der "Beschreibungen"
     *)
    fDestT: integer;
    fDestL: integer;
    fDestH: integer;
    fDestW: integer;

    fwold, fhold: integer; // Breite und Höhe des Anzeigefensters (wird in jedem OnRender Aktualisiert)
    fCursorPos: Tpoint; // Immer die Aktuelle Position des Mauscursors
    fLeftMousePressed: Boolean; // True, wenn die Maus mit Links gedrückt wurde, automatischer reset bei MouseUp
    fSelectTex: TGraphikItem; // Die Selektiert Textur die um Einheiten und Gebäude dargestellt wird wenn sie Angewählt wurde.
    fOpponentsAtEndOfWave: integer; // Schmiermerker zur Berechnung der Anzahl, der Gegner, welche noch in der Aktuellen Runde zu erwarten sind.
    fBuyMenuToolTipp: String; // Kleiner Hint wenn die Maus sich über dem Buymenu befindet
    fStrategyToolTipp: String; // Tooltip beim Hovern über den Strategie Buttons
    fStrategyToolTippPos: TPoint;

    Procedure DeselectCursor; // Setzt alles was "unterm" Cursor ist zurück und wählt es ab
    Procedure FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure FOnDblClick(Sender: TObject);

    Procedure FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure HandleLevelUpBuilding(Const Stream: TStream);
    Procedure HandleRemoveBuilding(Const Stream: TStream);

    Procedure SetOnHostButtonClick(AValue: TNotifyEvent);
    Procedure SetOnJoinButtonClick(AValue: TNotifyEvent);

    Procedure SetOnNewMapButtonClick(AValue: TNotifyEvent);
    Procedure SetOnLoadMapButtonClick(AValue: TNotifyEvent);
    Procedure SetOnLoadGameButtonClick(AValue: TNotifyEvent);

    Procedure SetScreenTo(x, y: integer);
    Procedure SetMenuPosition(AValue: TMenuPosition);
    Function GetIsInEditMode: boolean;
    Function GetPlayer(index: integer): TPlayerInfos;
    Procedure OnEndFiletransfer(); // Arbeiten die es zu tun gibt, wenn alle Filetransfer erfolgreich abgeschlossen sind

    Procedure OnLevelUpButtonClick(Sender: TObject); // Der Level Up Button wurde gedrückt
    Procedure OnSellButtonClick(Sender: TObject); // Der Verkaufen Button wurde gedrückt
    Procedure OnTabButtonClick(Sender: TObject); // Der Sidemenü aus / einblenden Knopf wurde gedrückt
    Procedure OnStrategyButtonClick(Sender: TObject);
    Procedure OnAirStrategyButtonClick(Sender: TObject);
    Procedure OnStrategyButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnUpgradeSellMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    Procedure Connection_Connect(aSocket: TLSocket);
    Procedure Connection_Disconnect(aSocket: TLSocket);
    Procedure Connection_Error(Const msg: String; aSocket: TLSocket);

    Procedure OnUDPConnection_Receive(aSocket: TLSocket); // Empfängt die UDP Broadcast Nachrichten der Server

    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
    Procedure SendChunk(UserDefinedID: Integer; Const Data: TStream);

    Procedure HandleRequestFileTransfer(MapName, Filename: String; CRC: UInt32);
    Procedure HandleFileReceifed(MapName, Filename: String; Const Data: TStream);
    Procedure HandleOnConnectToServer(ServerUid: integer);
    Procedure HandleOnDisconnectFromServer;
    Procedure HandleUpdateMapProperty(MapProperty, SenderUID: Integer; Data: TStream);
    Procedure HandleLoadGamingData(Const Stream: TStream);

    Procedure LoadMapFromShare(MapName_: String); // Lädt die Karte von Share als die Gerade Aktuelle Karte

    Procedure HandleStartRound(Round: integer; Difficulty: integer);
    Procedure HandleOnEndRound(Succeed: Boolean; Round: integer; Const Data: TStream);

    Procedure HandleRefresPlayerStats(Const Data: TStream);
    Procedure HandleOnBuildingsToStage(Const List: TStream);
    Procedure HandleOnHerosToLevel(Const List: TStream);

    Procedure RenderBlackOutMapBorder;
    Procedure RenderBuyMenu;
    Procedure RenderMenuTabButton;
    Procedure RenderSideMenu;
    Procedure RenderSelected(sx, sy, x, y: integer);
    Procedure RenderGameInfos; // Zeigt die infos zur Aktuellen Runde an.

    Procedure UpdatePausing(Value: Boolean);

    Procedure CreateBuildMenu(Wave: integer);
    Function GetMapObjUnderCursor(x, y: integer): tctd_mapopbject;
    Function GetBuildObjUnderCursor(x, y: integer): tctd_mapopbject;
    Procedure SetBuyingObject(obj: tctd_mapopbject); // Wählt ein Gebäude zum "Kaufen" aus, muss aus "FBuildingMenu.Items" kommen !
    Procedure SetheroTargets(x, y: Single); // Setzt ein neues "Ziel" für alle Selektierten Helden
    Procedure BuyBuilding(x, y: integer; Obj: TBuilding); // Client Kauft ein Gebäude und könnte es sich leisten
    Procedure BuyHero(x, y: integer; Obj: THero); // Client Kauft einen Helden und könnte es sich leisten
    Procedure AddBuilding(x, y: integer; Name: String; Owner: integer);
    Procedure AddHero(x, y: integer; Name: String; Owner: integer);
    Procedure SwitchToEditMode();
    Function fGetPlayerCount: integer;
    Procedure ModifyTerrain(kx, ky: integer);
    Procedure ModifyWaypointArea(kx, ky: integer);
    Procedure CreateSplashMark(x, y: integer);

    Procedure CalcSumKillsLives(Out SumKills: integer; Out SumLives: integer);

    Procedure DoWaveClone(SourceWaveNum, DestWaveNum: integer);
    Procedure RenderHint(x, y: integer; Const Hint: THint; AllowAdjusting: Boolean = true);
  public
    VersionInfoString: String;
    ShowBuildableTilesDuringBuild: Boolean; // Wenn True, dann wird er "B" Tastendruck simuliert während ein Gebäude Plaziert wird
    BlockMapUpdateSending: Boolean;
    ShowGrid: Boolean;
    ShowPlayerStartNames: Boolean; // Zeigt die Spielernamen an den Startpositionen an
    InvertMouseScrolling: Boolean;
    DarkOtherBuildings: Boolean; // Wenn true, dann werden andere Gebäude Grau gemalt
    UseMiddleMouseButtonToScroll: Boolean;
    OnConnectToServer: TNotifyEvent;
    OnDisconnectFromServer: TNotifyEvent;
    OnLoadMap: TNotifyEvent;
    OnUpdateMapProperty: TUpdateMapPropertyEvent;
    OnStartRound: TNotifyEvent;
    OnForceEditMode: TNotifyEvent;
    OnEndRound: TEndRoundEvent;
    OnHandleLoadGameingData: TNotifyEvent;
    OnShowGameStatistics: TOnShowGameStatistics;
    OnWaveCloneEvent: TOnWaveCloneEvent;
    OnGetHighscores: TOnGetStreamEvent;
    ShowLifepoints: Boolean; // Sollen beim Rendern die Lebenspunkte der Gegnerischen Einheiten angezeigt werden ?
    HintShowBuildingRange: Boolean; // Anzeige des Feuerradiusses eines Gebäudes beim Hinting
    HintShowHeroRange: Boolean; // Anzeige des Feuerradiusses eines Helden beim Hinting
    OnRefreshPlayerStats: TNotifyEvent;
    Property OnHostButtonClick: TNotifyEvent write SetOnHostButtonClick;
    Property OnJoinButtonClick: TNotifyEvent write SetOnJoinButtonClick;
    Property OnNewMapButtonClick: TNotifyEvent write SetOnNewMapButtonClick;
    Property OnLoadMapButtonClick: TNotifyEvent write SetOnLoadMapButtonClick;
    Property OnLoadGameButtonClick: TNotifyEvent write SetOnLoadGameButtonClick;

    Property GameState: TGameState read fgameState;
    Property SideMenuVisible: Boolean read fSidemenuVisible;
    Property MenuPosition: TMenuPosition read fMenuPosition write SetMenuPosition;
    Property Player[index: integer]: TPlayerInfos read GetPlayer;
    Property AktualWave: integer read fAktualWave;
    Property Map: TMap read fMap;
    Property PlayerCount: integer read fGetPlayerCount;
    Property PlayerIndex: integer read fPlayerIndex;

    Constructor create;
    Destructor Destroy; override;

    Property IsInEditMode: boolean read GetIsInEditMode;
    Procedure Initialize(Const Owner: TOpenGLControl); // Lädt alles was es so zu laden gibt (OpenGL-Technisch), wird einmalig in OnMakeCurrent Aufgerufen
    Procedure RegisterTCPConnection(Const Connection: TLTCPComponent);
    Procedure RegisterUDPConnection(Const Connection: TLUDPComponent);

    Function Join(ip: String; Port: integer; Password, Username: String): Boolean;
    Procedure DisConnect();

    Procedure Render(w, h: integer);
    Procedure Resize;

    Procedure NewMap(w, h: Integer; n: String);
    Procedure LoadMap(MapName: String);
    Procedure getMapList(Callback: TOnGetStringListEvent);
    Procedure getMapPrieviewInfo(MapName: String; Callback: TOnGetMapPrieviewInfoEvent);
    Procedure CloneMapWave(SourceWaveNum, DestWaveNum: integer);

    Procedure UpdateMapProperty(MapProperty: integer; Const Stream: TStream);
    Procedure InitiateNewGame(Difficulty: integer);
    Procedure RestartRound;
    Procedure Continue;
    Procedure AbortWave;

    Procedure Splashhint(Const Text: String; Color: TVector3);
    Procedure splashAirLevels;
    Procedure splashBossLevels;
    Procedure splashBonusLevels;
    Procedure RequestPingTimes;
    Procedure SendKillCommand(PlayerName: String);

    Procedure TogglePause;
    Procedure SendChat(Message: String);
    Procedure TransferFile(SourceFile: String; Dest: String;
      Callback: TOnGetBooleanEvent);
    Procedure GetFileList(Mask: String; Callback: TOnGetStringListEvent); // Sucht beim Server nach Dateien mit der Dateiendung Mask und gibt das Ergebniss zurück
    Procedure GetSavegames(Callback: TOnGetStreamEvent);
    Procedure SaveGame(Filename: String);
    Procedure LoadGame(Filename: String);
    Procedure DelSaveGame(Filename: String);
    Procedure DelAllSaveGames();
    Procedure DelOpponent(Opp: String);
    Procedure DelBuilding(Geb: String);
    Procedure DelHero(Hero: String);
    Procedure TransferCash(DestPlayer, Count: integer);
    Procedure TransferCompleteMapTerrain(Const Stream: TStream);
    Procedure DoSell;
    Procedure DoHeroStop;
    Procedure DoSelectAllOwnHeros;
    Procedure DoUpdate;
    Procedure IncSpeed;
    Procedure DecSpeed;
    Procedure RequestPlayerInfos;
    Procedure ChangePlayerPos(aPlayerindex: integer; Up: Boolean);
    Procedure CleanupUnusedOpponets();

    Procedure Check_Scrollborders;
    Procedure Zoom(ZoomIn: Boolean);

    Procedure PingForOpenGames; // Sendet einen UDP-Ping ins Netz und erwartet von allen Servern, die Bereit sind neue Verbindugnen zu empfangen eine entsprechende Antwort
    Function AktualGameList: TStringlist; // Liefert eine Liste aller Aktuell bekannter Verbindbarer Spiele
    Procedure SelectAllBuildingsLikeTheSelected();

    Procedure SendMapRating(Rating: integer);
    Procedure RequestHighscores;
    Procedure RequestClearHighscore;
    Procedure AddNewRandomWave(Difficulty: integer);

    Procedure NilSideObject(Obj: tctd_mapopbject);
  End;

Var
  ctd: Tctd; // Die SpielEngine

  (*
   * Zugriff auf Konfigurationsparameter
   *)
Function GetValue(Section, Ident, Default: String): String;
Procedure SetValue(Section, Ident, Value: String);

Procedure CenterTextOut(w, h: integer; text: String); // Gibt über dem Fenster Zentriert einen Text aus z.B. "Pause"

Implementation

Uses IniFiles, LazUTF8, LCLIntf, forms, LazFileUtils, math, dglOpenGL, LCLType,

  uOpenGL_ASCII_Font, uopengl_spriteengine,
  unit1, unit13, unit4,
  uctd_messages, uip;

Var
  ini: TInifile = Nil;

  (*
   * Jedes mal wenn ein der Client eigentlich Blocking Daten Anfrägt wird
   * diese ID zur Identifizierung Übertragen, Der Wert ist eigentlich fast Egal
   * Hauptsache es steht was drin *g*;
   *
   * Zugrgriffen werden darf nur via GetNextGlobalStreamQueueID
   *
   *)
  GlobalStreamQueueID: UInt16 = 0;

Function GetNextGlobalStreamQueueID: Uint16;
Begin
  If GlobalStreamQueueID < High(UInt16) Then Begin
    GlobalStreamQueueID := GlobalStreamQueueID + 1;
  End
  Else Begin
    GlobalStreamQueueID := 0;
  End;
  result := GlobalStreamQueueID;
End;

Function RenderToolTipp(x, y: integer; Tipp: String; AllowAdjusting: Boolean = true): TVector2;
Var
  fs, w, h: Single;
  dim: Array[0..3] Of Integer;
Begin
  x := x + 15; // Rechts neben den Cursor schieben.
  fs := OpenGL_ASCII_Font.Size;
  OpenGL_ASCII_Font.Size := OpenGL_ASCII_Font.Size * fontscale;
  w := OpenGL_ASCII_Font.TextWidth(Tipp);
  h := OpenGL_ASCII_Font.TextHeight(Tipp);
  glGetIntegerv(GL_VIEWPORT, @dim[0]);
  If (x + w > dim[2]) And (AllowAdjusting) Then Begin // Wenn der Tooltip Rechts aus dem Bild geht
    x := x - 2 * 15 - integer(round(w));
  End;
  If (y + h > dim[3]) And (AllowAdjusting) Then Begin // Wenn der Tooltip unten aus dem Bild geht
    y := y - round(h);
  End;
  // Einen Schwarzen hintergrund unter den Text
  glPushMatrix;
  glBindTexture(GL_TEXTURE_2D, 0);
  glTranslatef(x, y, ctd_Tipp_Layer);
  result := v2(x, y); // Merken ab Welcher Koordinate der Text gerendert wurde ;)
  glColor4f(0, 0, 0, 1);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(w, 0);
  glVertex2f(w, h);
  glVertex2f(0, h);
  glend;
  // der Eigentliche Text oben drüber
  glTranslatef(0, 0, ctd_EPSILON);
  OpenGL_ASCII_Font.Color := clGray;
  OpenGL_ASCII_Font.Textout(0, 0, tipp);
  glPopMatrix;
  OpenGL_ASCII_Font.Size := fs;
End;

Procedure Tctd.RenderHint(x, y: integer; Const Hint: THint;
  AllowAdjusting: Boolean);
Var
  t, s: String;
  p: TVector2;
  i, j: Integer;
  yo, lw, lh: Single;
  gi: TGraphikItem;
Begin
  lw := OpenGL_ASCII_Font.TextWidth('8') * fontscale;
  lh := OpenGL_ASCII_Font.TextHeight('8') * fontscale;
  Case Hint.Kind Of
    hkOpponent: Begin
        // Berechnen wie Groß der Hint wird
        t := format('(  %d   %d   %d   %d) = %d', [
          hint.Power[0],
            hint.Power[1],
            hint.Power[2],
            hint.Power[3],
            hint.TotalLivepoints]);
        s := hint.Name + LineEnding +
          t + LineEnding +
          hint.Description;
        p := RenderToolTipp(x, y, s, AllowAdjusting);
        yo := lh;
      End;
    hkbuilding: Begin
        (*
         * Stage wird hier etwas "Kreativ" ausgewertet
         * -2 = Kauf Menü kein Leerzeichen zwischen Name und Description
         * -1 = Kauf Menü
         *  0 = Im Bau
         * >0 = Gebaut -> Stage Info anzeigen
         *)
        If hint.Stage = 0 Then Begin // Das Gebäude wird noch gebaut -> kein Sinnvoller Hint anzeigbar.
          RenderToolTipp(x, y, hint.Name, true);
          exit;
        End
        Else Begin
          s := trim(hint.Description);
          If s <> '' Then s := s + LineEnding;
          t := hint.Name + LineEnding;
          If hint.stage > 0 Then Begin
            t := t + format('Stage: %d / %d', [hint.Stage, hint.StageCount]) + LineEnding;
          End
          Else Begin
            If (s = '') And (hint.stage <> -2) Then s := s + LineEnding;
          End;
          t := t + s +
            format('(  %d   %d   %d   %d)', [
            hint.Power[0],
              hint.Power[1],
              hint.Power[2],
              hint.Power[3]
              ]);
          p := RenderToolTipp(x, y, t, AllowAdjusting);
          yo := (OpenGL_ASCII_Font.TextHeight(t) - OpenGL_ASCII_Font.TextHeight('8')) * fontscale;
        End;
      End;
    hkHero: Begin
        (*
         * Stage wird hier etwas "Kreativ" ausgewertet
         * -2 = Kauf Menü kein Leerzeichen zwischen Name und Description
         *  0 = Im Bau
         * >0 = Gebaut -> Stage Info anzeigen
         *)
        If hint.Stage = 0 Then Begin // Der Held wird noch gebaut -> kein Sinnvoller Hint anzeigbar.
          RenderToolTipp(x, y, hint.Name, true);
          exit;
        End
        Else Begin
          s := trim(hint.Description);
          If s <> '' Then s := s + LineEnding;
          t := hint.Name + LineEnding;
          If hint.stage > 0 Then Begin
            t := t + format('Level: %d / %d', [hint.Stage, hint.StageCount]) + LineEnding;
          End
          Else Begin
            If (s = '') And (hint.stage <> -2) Then s := s + LineEnding;
          End;
          t := t + s +
            format('(  %d   %d   %d   %d)', [
            hint.Power[0],
              hint.Power[1],
              hint.Power[2],
              hint.Power[3]
              ]);
          p := RenderToolTipp(x, y, t, AllowAdjusting);
          yo := (OpenGL_ASCII_Font.TextHeight(t) - OpenGL_ASCII_Font.TextHeight('8')) * fontscale;
        End;
      End;
  Else Begin
      Raise exception.create('Tctd.RenderHint: missing implementation.');
    End;
  End;
  // Das "passende" Einzeichnen der Damageclassen in den Text
  glPushMatrix;
  glTranslatef(p.x, p.y, ctd_Tipp_Layer + 1.5 * ctd_EPSILON);
  t := format(',  %d,  %d,  %d,', [
    hint.Power[0],
      hint.Power[1],
      hint.Power[2]]);
  j := 0;
  glColor3f(1, 1, 1);
  For i := 0 To 3 Do Begin
    j := j + pos(',', t);
    delete(t, 1, pos(',', t));
    gi := fMap.OpenGLDamageClassTex[i];
    glPushMatrix;
    glTranslatef(lw * j, yo, 0);
    glScalef(lh / gi.OrigWidth, lh / gi.OrigWidth, 1);
    RenderQuad(0, 0, gi);
    glPopMatrix;
  End;
  glPopMatrix;
End;

Function GetValue(Section, Ident, Default: String): String;
Begin
  If Not Assigned(ini) Then Begin
    result := Default;
  End
  Else Begin
    result := ini.ReadString(Section, Ident, Default);
  End;
End;

Procedure SetValue(Section, Ident, Value: String);
Begin
  If assigned(ini) Then Begin
    ini.WriteString(Section, Ident, Value);
  End;
End;

Procedure TextOut(x, y: integer; text: String);
Var
  fs: Single;
Begin
  glBindTexture(GL_TEXTURE_2D, 0);
  fs := OpenGL_ASCII_Font.Size;
  OpenGL_ASCII_Font.Size := OpenGL_ASCII_Font.Size * fontscale;
  OpenGL_ASCII_Font.Textout(x, y, text);
  OpenGL_ASCII_Font.Size := fs;
End;

Procedure CenterTextOut(w, h: integer; text: String);
Var
  tw, th: integer;
Begin
  tw := round(OpenGL_ASCII_Font.TextWidth(text) * fontscale);
  th := round(OpenGL_ASCII_Font.TextHeight(text) * fontscale);
  TextOut((w - tw) Div 2, (h - th) Div 2, text);
End;

{ TCTDDualinfoField }

Procedure TCTDDualinfoField.OnRender();
Const
  Border = 2;
Begin
  glColor4f(1, 1, 1, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(left, top, ctd_Tipp_Layer + 2 * Epsilon);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(Width, 0);
  glVertex2f(Width, Height);
  glVertex2f(0, Height);
  glend();
  glTranslatef(0, 0, Epsilon);
  RenderAlphaQuad(point(Width Div 2, Height Div 2), Height, -Height, 0, ftexture);
  OpenGL_ASCII_Font.Color := clBlack;
  OpenGL_ASCII_Font.RenderTextToRect(rect((Width + Height) Div 2 + Border, Border, width - Border, height - Border), Text);
  OpenGL_ASCII_Font.RenderTextToRect(rect(Border, Border, (width - Height) Div 2 - Border, height - Border), Text2);
  glPopMatrix;
End;

{ TCTDinfofield }

Procedure TCTDinfofield.OnRender();
Const
  Border = 2;
Begin
  glColor4f(1, 1, 1, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(left, top, ctd_Tipp_Layer + 2 * Epsilon);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(Width, 0);
  glVertex2f(Width, Height);
  glVertex2f(0, Height);
  glend();
  glTranslatef(0, 0, Epsilon);
  RenderAlphaQuad(point(Height Div 2, Height Div 2), Height, -Height, 0, ftexture);
  OpenGL_ASCII_Font.Color := clBlack;
  OpenGL_ASCII_Font.RenderTextToRect(rect(Height + Border, Border, width - Border, height - Border), Text);
  glPopMatrix;
End;

Constructor TCTDinfofield.Create(Owner: TOpenGLControl; Texture: String);
Begin
  Inherited Create(Owner);
  ftexture := OpenGL_GraphikEngine.LoadAlphaGraphik(Texture, smStretchHard);
End;

{ TStreamHolder }

Constructor TCallbackHolder.Create;
Begin
  Inherited create;
  fHolder := Nil;
End;

Destructor TCallbackHolder.Destroy;
Begin
  setlength(fHolder, 0);
End;

Function TCallbackHolder.AddWaitIdentifier(Identifier: integer;
  Callback: TOnGetStringListEvent): Boolean;
Var
  index, i: integer;
Begin
  result := true;
  index := -1;
  // Die Identifier darf es nicht doppelt geben
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := false;
      exit;
    End;
    // Wir haben einen Freien Slot gefunden (und zwar den ersten :)), den Merken wir uns gleich mal
    If (index = -1) And (Not fHolder[i].Used) Then Begin
      index := i;
    End;
  End;
  // Wenn wir hier her kommen, dann gab es den Identifier nicht
  If Index = -1 Then Begin
    // Es gibt keinen Freien Platz mehr also machen wir uns welchen
    setlength(fHolder, high(fHolder) + 2);
    index := high(fHolder);
  End;
  // Den Datensatz initialisieren
  fHolder[index].Used := true;
  fHolder[index].Identifier := Identifier;
  fHolder[index].StringListEventCallback := Callback;
  fHolder[index].MapPrieviewInfoEventCallback := Nil;
  fHolder[index].BooleanEventRecord.Callback := Nil;
  fHolder[index].BooleanEventRecord.s := '';
  fHolder[index].BooleanEventRecord.d := '';
  fHolder[index].StreamEvent := Nil;
End;

Function TCallbackHolder.AddWaitIdentifier(Identifier: integer;
  Callback: TOnGetMapPrieviewInfoEvent): Boolean;
Var
  index, i: integer;
Begin
  result := true;
  index := -1;
  // Die Identifier darf es nicht doppelt geben
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := false;
      exit;
    End;
    // Wir haben einen Freien Slot gefunden (und zwar den ersten :)), den Merken wir uns gleich mal
    If (index = -1) And (Not fHolder[i].Used) Then Begin
      index := i;
    End;
  End;
  // Wenn wir hier her kommen, dann gab es den Identifier nicht
  If Index = -1 Then Begin
    // Es gibt keinen Freien Platz mehr also machen wir uns welchen
    setlength(fHolder, high(fHolder) + 2);
    index := high(fHolder);
  End;
  // Den Datensatz initialisieren
  fHolder[index].Used := true;
  fHolder[index].Identifier := Identifier;
  fHolder[index].StringListEventCallback := Nil;
  fHolder[index].MapPrieviewInfoEventCallback := Callback;
  fHolder[index].BooleanEventRecord.Callback := Nil;
  fHolder[index].BooleanEventRecord.s := '';
  fHolder[index].BooleanEventRecord.d := '';
  fHolder[index].StreamEvent := Nil;
End;

Function TCallbackHolder.AddWaitIdentifier(Identifier: integer;
  Callback: TOnGetBooleanEventRecord): Boolean;
Var
  index, i: integer;
Begin
  result := true;
  index := -1;
  // Die Identifier darf es nicht doppelt geben
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := false;
      exit;
    End;
    // Wir haben einen Freien Slot gefunden (und zwar den ersten :)), den Merken wir uns gleich mal
    If (index = -1) And (Not fHolder[i].Used) Then Begin
      index := i;
    End;
  End;
  // Wenn wir hier her kommen, dann gab es den Identifier nicht
  If Index = -1 Then Begin
    // Es gibt keinen Freien Platz mehr also machen wir uns welchen
    setlength(fHolder, high(fHolder) + 2);
    index := high(fHolder);
  End;
  // Den Datensatz initialisieren
  fHolder[index].Used := true;
  fHolder[index].Identifier := Identifier;
  fHolder[index].StringListEventCallback := Nil;
  fHolder[index].MapPrieviewInfoEventCallback := Nil;
  fHolder[index].BooleanEventRecord := Callback;
  fHolder[index].StreamEvent := Nil;
End;

Function TCallbackHolder.AddWaitIdentifier(Identifier: integer;
  Callback: TOnGetStreamEvent): Boolean;
Var
  index, i: integer;
Begin
  result := true;
  index := -1;
  // Die Identifier darf es nicht doppelt geben
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := false;
      exit;
    End;
    // Wir haben einen Freien Slot gefunden (und zwar den ersten :)), den Merken wir uns gleich mal
    If (index = -1) And (Not fHolder[i].Used) Then Begin
      index := i;
    End;
  End;
  // Wenn wir hier her kommen, dann gab es den Identifier nicht
  If Index = -1 Then Begin
    // Es gibt keinen Freien Platz mehr also machen wir uns welchen
    setlength(fHolder, high(fHolder) + 2);
    index := high(fHolder);
  End;
  // Den Datensatz initialisieren
  fHolder[index].Used := true;
  fHolder[index].Identifier := Identifier;
  fHolder[index].StringListEventCallback := Nil;
  fHolder[index].MapPrieviewInfoEventCallback := Nil;
  fHolder[index].BooleanEventRecord.Callback := Nil;
  fHolder[index].BooleanEventRecord.d := '';
  fHolder[index].BooleanEventRecord.s := '';
  fHolder[index].StreamEvent := Callback;
End;

Function TCallbackHolder.GetIdentifierStringListEventCallback(
  Identifier: integer): TOnGetStringListEvent;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := fHolder[i].StringListEventCallback;
      fHolder[i].Used := false;
      exit;
    End;
  End;
  LogShow('TStreamHolder.GetIdentifierStringListEventCallback requesting not existing Identifier', llCritical);
End;

Function TCallbackHolder.GetIdentifierMapPrieviewInfoEventCallback(
  Identifier: integer): TOnGetMapPrieviewInfoEvent;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := fHolder[i].MapPrieviewInfoEventCallback;
      fHolder[i].Used := false;
      exit;
    End;
  End;
  LogShow('TStreamHolder.GetIdentifierMapPrieviewInfoEventCallback requesting not existing Identifier', llCritical);
End;

Function TCallbackHolder.GetIdentifierBooleanEventCallback(Identifier: integer
  ): TOnGetBooleanEventRecord;
Var
  i: Integer;
Begin
  result.Callback := Nil;
  result.s := '';
  result.d := '';
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := fHolder[i].BooleanEventRecord;
      fHolder[i].Used := false;
      exit;
    End;
  End;
  LogShow('TStreamHolder.GetIdentifierBooleanEventCallback requesting not existing Identifier', llCritical);
End;

Function TCallbackHolder.GetIdentifierStreamEventCallback(Identifier: integer
  ): TOnGetStreamEvent;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHolder) Do Begin
    If fHolder[i].Used And (fHolder[i].Identifier = Identifier) Then Begin
      result := fHolder[i].StreamEvent;
      fHolder[i].Used := false;
      exit;
    End;
  End;
  LogShow('TStreamHolder.GetIdentifierStreamEventCallback requesting not existing Identifier', llCritical);
End;

{ TBuildingStrategyButton }

Constructor TBuildingStrategyButton.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  Selected := false;
  Strategy := bsFirst;
  Fimage := 0;
  FSelectedImage := 0;
End;

Procedure TBuildingStrategyButton.OnRender;
Begin
  If Selected Then Begin
    RenderQuad(v2(left, top), v2(left + width, top + height), 0, false, FSelectedImage);
  End
  Else Begin
    RenderQuad(v2(left, top), v2(left + width, top + height), 0, false, Fimage);
  End;
End;

Procedure TBuildingStrategyButton.Click;
Begin
  Inherited Click;
End;

Procedure TBuildingStrategyButton.SetImage(Filename: String);
Var
  f, p, e: String;
Begin
  log('TBuildingStrategyButton.SetImage', llTrace);
  Fimage := OpenGL_GraphikEngine.LoadGraphik(Filename, smStretchHard);
  f := ExtractFileNameOnly(Filename);
  p := IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
  e := ExtractFileExt(Filename);
  FSelectedImage := OpenGL_GraphikEngine.LoadGraphik(p + f + 's' + e, smStretchHard);
  If Fimage = 0 Then Begin
    log('Could not load : ' + Filename, llError);
  End;
  If FSelectedImage = 0 Then Begin
    log('Could not load : ' + p + f + 's' + e, llError);
  End;
  LogLeave;
End;

{ Tctd }

Procedure Tctd.FOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  obj: tctd_mapopbject;
  i, j, kx, ky: integer;
  m: TMemoryStream;
  xs, ys: Single;
Begin
  fmDownPos := point(x, y);
  fmScrollPos := point(x, y);
  fCursorPos := point(x, y);
  fLeftMousePressed := ssLeft In Shift;
  // das "!" via STRG + Linksklick
  If (ssCtrl In Shift) And (ssleft In shift) And assigned(fMap) Then Begin
    // das "!" Symbol via Map Preview
    If uctd_common.PointInRect(point(x, y), rect(fPreviewL, fPreviewT, fPreviewL + fPreviewW, fPreviewT + fPreviewH)) Then Begin
      i := round(ConvertDimension(0, fPreviewW, x - fPreviewL, 0, fMap.Width));
      j := round(ConvertDimension(0, fPreviewH, y - fPreviewT, 0, fMap.Height));
      CreateSplashMark(i, j);
    End;
    // das "!" Symbol via Click auf die Karte
    If uctd_common.PointInRect(point(x, y), rect(fMapL, fMapT, fMapL + fMapW, fMapT + fMapH)) Then Begin
      i := (fsx + x - fMapL) Div MapBlockSize;
      j := (fsy + y - fMapT) Div MapBlockSize;
      CreateSplashMark(i, j);
    End;
    exit;
  End;
  // Scrollen via Click auf Preview Karte, nur bei Links Klick, sonst passiert evtl ein komisches Scrollen
  If ssleft In shift Then Begin
    If uctd_common.PointInRect(point(x, y), rect(fPreviewL, fPreviewT, fPreviewL + fPreviewW, fPreviewT + fPreviewH)) Then Begin
      SetScreenTo(x, y);
      exit;
    End;
  End;
  If (fGameState = gs_Gaming) Then Begin
    // Der Spieler Versucht ein Gebäude zu plazieren
    If (ssleft In shift) And assigned(FBuyingObject) And (Not fpausing) Then Begin
      If uctd_common.PointInRect(point(x, y), rect(fMapL, fMapT, fMapL + fMapW, fMapT + fMapH)) Then Begin
        If fPlayerInfos[fPlayerIndex].Cash >= FBuyingObject.GetBuyCost() Then Begin
          i := (fsx + x - fMapL) Div MapBlockSize;
          j := (fsy + y - fMapT) Div MapBlockSize;
          If fBuyingObject Is TBuilding Then Begin
            If Not fMap.CoordIsBuildable(i, j, FBuyingObject As TBuilding) Then exit;
            BuyBuilding(i, j, FBuyingObject As TBuilding);
          End
          Else Begin
            If fBuyingObject Is THero Then Begin
              // If Not fMap.CoordIsBuildable(i, j, FBuyingObject) Then exit; -- Helden können überall gebaut werden !
              BuyHero(i, j, fBuyingObject As Thero);
            End
            Else Begin
              Raise exception.Create('Missing implementation.');
            End;
          End;
          If Not (ssShift In shift) Then Begin
            DeselectCursor;
          End;
        End
        Else Begin
          SplashHint('Error, not enough money to buy ' + FBuyingObject.name, v3(1, 0, 0));
          exit;
        End;
      End
    End;
    // Der Spieler wählt ein Gebäude auf der Karte an
    If (ssleft In shift) And (Not (ssShift In Shift)) Then Begin
      obj := GetMapObjUnderCursor(x, y);
      If assigned(obj) Then Begin
        FSideMenuObject := obj;
        If fSideMenuObject Is TOpponent Then Begin
          fSidemenuOpponent.AssignDataFromObj(TOpponent(fSideMenuObject));
        End;
        If fSideMenuObject Is TBuilding Then Begin
          setlength(fSelectedBuildings, 1);
          fSelectedBuildings[0] := TBuilding(fSideMenuObject);
        End
        Else Begin
          fSelectedBuildings := Nil;
        End;
        If FSideMenuObject Is THero Then Begin
          setlength(fSelectedHeros, 1);
          fSelectedHeros[0] := THero(FSideMenuObject);
        End
        Else Begin
          fSelectedHeros := Nil;
        End;
        If assigned(FBuyingObject) Then FBuyingObject.free;
        FBuyingObject := Nil;
        exit;
      End
      Else Begin
        // Der Spieler hat ins "Leere" geklickt und wählt damit das Gebäude wieder ab
        // Das ist genau gleich wie bei Tctd.FOnMouseUp
        FSideMenuObject := Nil;
        If assigned(fBuyingObject) Then fBuyingObject.free;
        FBuyingObject := Nil;
        fSelectedBuildings := Nil;
        fStrategyToolTipp := '';
        // Der Spieler will Heros an neue Koords schicken !
        If assigned(fSelectedHeros) Then Begin
          If uctd_common.PointInRect(point(x, y), rect(fMapL, fMapT, fMapL + fMapW, fMapT + fMapH)) Then Begin
            xs := (fsx + x - fMapL) / MapBlockSize;
            ys := (fsy + y - fMapT) / MapBlockSize;
            SetheroTargets(xs, ys);
            exit; // Sonst würden die Heros abgewählt werden, dass soll der User via Rechte Maus machen !
          End;
        End;
      End;
    End;
    // Evtl Toggle eines Angewählten Gebäudes
    If (ssleft In shift) And (ssShift In Shift) Then Begin
      obj := GetMapObjUnderCursor(x, y);
      // Nur wenn auch ein Gebäude angeklickt wurde wird Reagiert
      If assigned(obj) And (obj Is TBuilding) Then Begin // Das angeklickte Objekt ist ein Gebäude und Updatet sich gerade nicht
        If (TBuilding(obj).fUpdating.State = usIdleInactive) Then Begin
          // 2 Möglichkeiten
          For i := 0 To high(fSelectedBuildings) Do Begin
            If fSelectedBuildings[i] = obj Then Begin
              // 2.1 das Geb ist drin, dann Abwahl
              For j := i To high(fSelectedBuildings) - 1 Do
                fSelectedBuildings[j] := fSelectedBuildings[j + 1];
              setlength(fSelectedBuildings, high(fSelectedBuildings));
              If high(fSelectedBuildings) = -1 Then Begin
                fSideMenuObject := Nil;
              End
              Else Begin
                fSideMenuObject := fSelectedBuildings[0]; // Egal welches hauptsache ein Gültiges
              End;
              exit;
            End;
          End;
          // 2.2 das Geb ist nicht Drin, dann Anwahl
          If (high(fSelectedBuildings) = -1) Or (TBuilding(obj).Stage = fSelectedBuildings[0].Stage) Then Begin
            // Nur Gebäude Gleichen Levels werden Aktzeptiert
            setlength(fSelectedBuildings, high(fSelectedBuildings) + 2);
            fSelectedBuildings[high(fSelectedBuildings)] := TBuilding(obj);
            fSideMenuObject := fSelectedBuildings[0]; // Egal welches hauptsache ein Gültiges
          End;
          exit;
        End;
      End
      Else Begin
        If assigned(obj) And (obj Is THero) Then Begin // Das angeklickte Objekt ist ein Gebäude und Updatet sich gerade nicht
          // 2 Möglichkeiten
          For i := 0 To high(fSelectedHeros) Do Begin
            If fSelectedHeros[i] = obj Then Begin
              // 2.1 das Geb ist drin, dann Abwahl
              For j := i To high(fSelectedHeros) - 1 Do
                fSelectedHeros[j] := fSelectedHeros[j + 1];
              setlength(fSelectedHeros, high(fSelectedHeros));
              If high(fSelectedHeros) = -1 Then Begin
                fSideMenuObject := Nil;
              End
              Else Begin
                fSideMenuObject := fSelectedHeros[0]; // Egal welches hauptsache ein Gültiges
              End;
              exit;
            End;
          End;
          // 2.2 das Geb ist nicht Drin, dann Anwahl
          setlength(fSelectedHeros, high(fSelectedHeros) + 2);
          fSelectedHeros[high(fSelectedHeros)] := THero(obj);
          fSideMenuObject := fSelectedHeros[0]; // Egal welches hauptsache ein Gültiges
          exit;
        End;
      End;
    End;
    // Der Spieler wählt ein Gebäude im Buy Menü aus
    If ssleft In shift Then Begin
      obj := GetBuildObjUnderCursor(x, y);
      If assigned(obj) Then Begin
        SetBuyingObject(obj);
      End;
    End;
  End; // -- Endif "(fGameState = gs_Gaming)"

  // Karte Verändern im Edit Modus
  If fGameState = gs_EditMode Then Begin
    If (ssleft In shift) And assigned(Map) And (y >= fMapT) And (y < fMapH) Then Begin // Wenn der Cursor überhaupt im Sichtbaren Bereich auf der Karte ist
      // Berechnen der Koordinaten unter der Maus
      kx := (x + fsx) Div MapBlockSize;
      ky := (y + fsy) Div MapBlockSize;
      If form4.CheckBox2.Checked Then Begin // Boden bearbeiten
        ModifyTerrain(kx, ky);
      End;
      If form4.CheckBox7.Checked Then Begin // Flächenwegpunkte bearbeiten
        ModifyWaypointArea(kx, ky);
      End;
      If form4.CheckBox5.Checked Then Begin // Wegpunkte Bearbeiten
        If ((form4.SpinEdit1.Value - 1) < 0) Or ((form4.SpinEdit1.Value - 1) > high(map.Waypoints)) Then Begin
          LogShow('Invalid param for waypoint player allowed "1..' + inttostr(High(map.Waypoints) + 1) + '" go to General and increase Max Player count.', llWarning);
          exit;
        End;
        // Suchen des Wegpunktes
        j := -1;
        For i := 0 To high(map.Waypoints[form4.SpinEdit1.Value - 1]) Do Begin
          If (map.Waypoints[form4.SpinEdit1.Value - 1, i].Point.x = kx) And (map.Waypoints[form4.SpinEdit1.Value - 1, i].Point.y = ky) Then Begin
            j := i;
            break;
          End;
        End;
        If form4.RadioButton1.Checked And (j = -1) Then Begin // Punkt Einfügen
          m := TMemoryStream.Create;
          i := form4.SpinEdit1.Value - 1;
          m.Write(i, sizeof(i));
          m.Write(kx, sizeof(kx));
          m.Write(ky, sizeof(ky));
          UpdateMapProperty(mpAddWayPoint, m);
        End;
        If j <> -1 Then Begin // Nur wenn ein Punkt gefunden wurde kann dieser "geändert" werden
          If Form4.RadioButton2.Checked Then Begin // Punkt Löschen
            m := TMemoryStream.Create;
            i := form4.SpinEdit1.Value - 1;
            m.Write(i, sizeof(i));
            m.Write(j, sizeof(j));
            UpdateMapProperty(mpDelWayPoint, m);
          End;
          If Form4.RadioButton3.Checked And (j > 0) Then Begin // Punkt in Reihenfolge "Erniedrigen"
            m := TMemoryStream.Create;
            i := form4.SpinEdit1.Value - 1;
            m.Write(i, sizeof(i));
            m.Write(j, sizeof(j));
            UpdateMapProperty(mpDecPointOrder, m);
          End;
        End;
      End;
      If form4.CheckBox6.Checked Then Begin // Placement Setzen oder Löschen
        If Form4.RadioButton4.checked Then Begin // Add Placement
          If assigned(PlacementeObject) Then Begin
            m := TMemoryStream.Create;
            m.Write(kx, sizeof(kx));
            m.Write(ky, sizeof(ky));
            i := strtointdef(form4.edit5.text, 1) - 1;
            m.Write(i, sizeof(i));
            m.WriteAnsiString(ExtractFileName(PlacementeObject.Filename));
            UpdateMapProperty(mpAddPlacement, m);
          End;
        End;
        If Form4.RadioButton5.checked Then Begin // Del Placement
          obj := Map.PlacementAtCoord(kx, ky);
          If assigned(obj) Then Begin
            m := TMemoryStream.Create;
            i := round(obj.Position.x);
            m.Write(i, sizeof(i));
            i := round(obj.Position.y);
            m.Write(i, sizeof(i));
            UpdateMapProperty(mpDelPlacement, m);
          End;
        End;
      End;
    End;
  End;
  If Assigned(FOnMouseDownCapture) Then
    FOnMouseDownCapture(Sender, Button, Shift, x, y);
End;

Procedure Tctd.DeselectCursor;
Begin
  fSideMenuObject := Nil;
  If assigned(FBuyingObject) Then FBuyingObject.free;
  fBuyingObject := Nil;
  fSelectedBuildings := Nil;
  fSelectedHeros := Nil;
End;

Procedure Tctd.FOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  fCursorPos := point(x, y);
  // Abwahl eines gewählten Objektes, aber nur, wenn der Spieler nicht gerade die Karte verschoben hat.
  If (abs(fmDownPos.x - x) < 15) And (abs(fmDownPos.y - y) < 15) And (mbRight = Button) Then Begin
    If (fGameState = gs_Gaming) Or (fGameState = gs_WaitToStart) Then Begin
      // Achtung, der Selbe Code steht auch in Tctd.FOnMouseDown, wenn "obj := GetMapObjUnderCursor(x, y)" -> Nil
      DeselectCursor;
      fStrategyToolTipp := '';
    End;
  End;
  If fLeftMousePressed And (fgameState = gs_Gaming) And (fMap.HeroCount > 0) And (Not assigned(fBuyingObject))
    And (fSelectedHeros = Nil)
    Then Begin
    // TODO: mittels shift in TShiftstate erlauben das die selektierungen "Erweitert" / Entfernt werden.
    fSelectedHeros := fMap.GetAllHerosOfOwnerRect(fPlayerIndex,
      rect(
      (fsx + min(fmDownPos.x, fCursorPos.X) - fMapL) Div MapBlockSize, (fsy + min(fmDownPos.y, fCursorPos.y) - fMapT) Div MapBlockSize,
      (fsx + max(fmDownPos.x, fCursorPos.X) - fMapL) Div MapBlockSize, (fsy + max(fmDownPos.y, fCursorPos.y) - fMapT) Div MapBlockSize
      )
      );
    If assigned(fSelectedHeros) Then Begin
      fSelectedBuildings := Nil;
      fSideMenuObject := fSelectedHeros[0]; // den 1. Anwählen sonst sieht man das nicht
      If Assigned(fBuyingObject) Then Begin
        fBuyingObject.free;
        fBuyingObject := Nil;
      End;
    End;
  End;
  fLeftMousePressed := false;
  If Assigned(FOnMouseUpCapture) Then
    FOnMouseUpCapture(sender, Button, shift, x, y);
End;

Procedure Tctd.FOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  num, i, dx, dy, ky, kx: Integer;
  obj: tctd_mapopbject;
Begin
  //   If FFreed Then exit;
  fCursorPos := point(x, y);
  // Verschieben des Kartenausschnittes bei gedrückter Linker Maus
  If (ssleft In shift) And uctd_common.PointInRect(point(x, y), rect(fPreviewL, fPreviewT, fPreviewL + fPreviewW, fPreviewT + fPreviewH)) Then Begin
    SetScreenTo(x, y);
    exit;
  End;

  // Scrollen der Karte mit Rechter Maustaste oder Mittlerer Maustaste
  If assigned(fMap) And (((ssRight In shift) And (Not UseMiddleMouseButtonToScroll))
    Or ((ssMiddle In shift) And (UseMiddleMouseButtonToScroll))) Then Begin
    dx := (fmScrollPos.x - x);
    If dx <> 0 Then Begin
      If InvertMouseScrolling Then Begin
        fsx := fsx - dx;
      End
      Else Begin
        fsx := fsx + dx;
      End;
    End;
    fmScrollPos.x := x;
    dy := (fmScrollPos.y - y);
    If dy <> 0 Then Begin
      If InvertMouseScrolling Then Begin
        fsy := fsy - dy;
      End
      Else Begin
        fsy := fsy + dy;
      End;
    End;
    fmScrollPos.y := y;
    Check_Scrollborders;
  End;
  fmScrollPos := point(x, y);
  fBuyMenuToolTipp := '';
  fStrategyToolTipp := '';
  // Hints Anzeigen, wenn die Maus "Hovert"
  If (fGameState = gs_Gaming) Or (fGameState = gs_WaitToStart) Then Begin
    obj := GetMapObjUnderCursor(x, y);
    // Done : Bug, wenn Maus auf Build Menü Hovert, dann sieht man einen "Phantom" Range links oben im Eck
    //If Not assigned(obj) Then
    //  obj := GetBuildObjUnderCursor(x, y);
    If FHintObject.Obj <> obj Then Begin
      FHintObject.Obj := obj;
      FHintObject.Time := GetTick();
    End;
    obj := GetBuildObjUnderCursor(x, y);
    If assigned(obj) Then Begin
      num := -1;
      // Einfügen der Keyboard Shortcut information
      For i := 0 To High(FBuyMenu.Items) Do Begin
        If FBuyMenu.Items[i].obj = obj Then Begin
          num := i + 1;
          break;
        End;
      End;
      If (num <= 10) And (num >= 1) Then Begin
        fBuyMenuToolTipp := obj.name + ' [' + inttostr(num Mod 10) + ']';
      End
      Else Begin
        fBuyMenuToolTipp := obj.name;
      End;
    End;
  End;
  If fGameState = gs_EditMode Then Begin
    // Anzeige Kartenkoordinaten in der Caption
    If assigned(map) And (map.ShowWaypoints) Then Begin // Die Waypoints werden nur im Editiermodus angezeigt ;)
      kx := (x + fsx) Div MapBlockSize;
      ky := (y + fsy) Div MapBlockSize;
      form1.Caption := defCaption + format(', X:%d, Y:%d', [min(map.width, kx + 1), min(map.height, ky + 1)]);
    End;

    // Verändern der Karte im Editiermodus
    If (ssleft In shift) And assigned(Map) And (y >= fMapT) And (y < fMapH) Then Begin // Wenn der Cursor überhaupt im Sichtbaren Bereich auf der Karte ist
      // Berechnen der Koordinaten unter der Maus
      kx := (x + fsx) Div MapBlockSize;
      ky := (y + fsy) Div MapBlockSize;
      If form4.CheckBox2.Checked Then Begin // Modify terrain
        ModifyTerrain(kx, ky);
      End;
      If form4.CheckBox7.Checked Then Begin // Flächenwegpunkte bearbeiten
        ModifyWaypointArea(kx, ky);
      End;
    End;
  End;
  If Assigned(FOnMousemoveCapture) {And Not (FFreed)} Then
    FOnMousemoveCapture(Sender, Shift, x, y);
End;

Procedure Tctd.FOnDblClick(Sender: TObject);
Begin
  // Ein Gebäude auf der Karte wird doppelt geklickt
  If uctd_common.PointInRect(fCursorPos, rect(fMapL, fMapT, fMapL + fMapW, fMapT + fMapH)) Then Begin
    If assigned(fSideMenuObject) And (fSideMenuObject Is TBuilding) And Not assigned(fBuyingObject) And (Not fCTRLPressed) Then Begin // Bugfix wenn STRG+ Leftklick (= anzeigen !) dann sollen die evtl angewählten Gebäude nicht mit angewählt werden.
      // Alle Anwählen die gerade Sichtbar und vom selben Typ sind.
      fSelectedBuildings := fMap.GetAllBuildingsSameOfSameTypeInRect(TBuilding(fSideMenuObject),
        rect(fsx Div MapBlockSize, fsy Div MapBlockSize, (fMapW - fMapL + fsx) Div MapBlockSize, (fMapH - fMapT + fsy) Div MapBlockSize)
        );
    End;
    If assigned(fSideMenuObject) And (fSideMenuObject Is THero) And Not assigned(fBuyingObject) And (Not fCTRLPressed) Then Begin // Bugfix wenn STRG+ Leftklick (= anzeigen !) dann sollen die evtl angewählten Gebäude nicht mit angewählt werden.
      // Alle Anwählen die gerade Sichtbar und vom selben Typ sind.
      fSelectedHeros := fMap.GetAllHerosOfSameTypeInRect(THero(fSideMenuObject),
        rect(fsx Div MapBlockSize, fsy Div MapBlockSize, (fMapW - fMapL + fsx) Div MapBlockSize, (fMapH - fMapT + fsy) Div MapBlockSize)
        );
    End;
  End;
  If assigned(FOnDblClickCapture) Then Begin
    FOnDblClickCapture(sender);
  End;
End;

Procedure Tctd.FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  v: integer;
Begin
  // Auswahl eines Gebäudes aus dem Baumenü heraus
  If key In [ord('0')..ord('9')] Then Begin
    If fgameState = gs_Gaming Then Begin
      v := (key - ord('0') + 9) Mod 10;
      If (v >= 0) And (v <= high(FBuyMenu.Items)) Then Begin
        SetBuyingObject(FBuyMenu.Items[v].obj);
      End;
    End;
  End;
  If key = ord('Q') Then Begin
    SelectAllBuildingsLikeTheSelected();
  End;
  If key = ord('W') Then Begin
    ShowPlayerStartNames := true;
  End;
  If key = ord('P') Then Begin
    TogglePause;
  End;
  If key = ord('H') Then Begin
    DoSelectAllOwnHeros;
  End;
  If key = ord('S') Then Begin
    If Assigned(fSelectedBuildings) Then Begin
      DoSell;
    End;
    If assigned(fSelectedHeros) Then Begin
      DoHeroStop;
    End;
  End;
  If key = ord('U') Then Begin
    DoUpdate;
  End;
  If (key = VK_ADD) Or (key = 187) Then Begin
    IncSpeed;
  End;
  If (key = VK_SUBTRACT) Or (key = 189) Then Begin
    DecSpeed;
  End;
  If (Key = VK_ESCAPE) Then Begin
    DeselectCursor();
  End;
  If (Key = ord('B')) Then Begin
    fShowBuildableTiles := true;
  End;
  // Ein und Ausblenden des menüs
{$IFDEF LINUX}
  If key = 150 Then Begin // ord('^') geht leider nicht
{$ELSE}
  If key = 220 Then Begin // ord('^') geht leider nicht
{$ENDIF}
    OnTabButtonClick(Nil);
  End;
  If ssCtrl In shift Then fCTRLPressed := true;
  If assigned(FOnKeyDownCapture) Then Begin
    FOnKeyDownCapture(sender, key, shift);
  End;
  // Im Spiel
  If (fgameState = gs_Gaming) Then Begin
    If key In [VK_F1..VK_F8] Then Begin
      BuildingStrategyButtons[Key - VK_F1].click;
    End;
  End;
End;

Procedure Tctd.FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  fCTRLPressed := false; // TODO: klären ob das so schon reicht oder ggf noch zu tun ist ..
  If (Key = ord('B')) Then Begin
    fShowBuildableTiles := false;
  End;
  If assigned(FOnKeyUpCapture) Then Begin
    FOnKeyUpCapture(sender, key, shift);
  End;
End;

Procedure Tctd.HandleLevelUpBuilding(Const Stream: TStream);
Var
  bd: TBuilding;
  i, j: integer;
  index, StreamSize: Int64;
Begin
  index := Stream.Position;
  StreamSize := stream.Size;
  While index < StreamSize Do Begin
    i := -1;
    Stream.Read(i, sizeof(i));
    j := -1;
    Stream.Read(j, sizeof(j));
    bd := TBuilding(fmap.GetObjUnderCursor(i * MapBlockSize, j * MapBlockSize));
    If assigned(bd) Then Begin
      bd.IncStage;
    End;
    inc(index, 2 * sizeof(Integer));
  End;
End;

Procedure Tctd.HandleRemoveBuilding(Const Stream: TStream);
Var
  l: integer;
  i, j, k: integer;
  index, StreamSize: Int64;
Begin
  index := Stream.Position;
  StreamSize := stream.Size;
  While index < StreamSize Do Begin
    i := -1;
    Stream.Read(i, sizeof(i));
    j := -1;
    Stream.Read(j, sizeof(j));
    // Der Lokale Spieler hat das Gebäude schon abgewählt, ein Client könnte es aber auch Selektiert
    // haben, also muss es aus der Liste
    // Berücksichtigen dass SidemenüBuilding evtl im Selected Array ist, dann nur das Eine Abwählen
    For k := 0 To high(fSelectedBuildings) Do Begin
      If (fSelectedBuildings[k].Position.x = i) And
        (fSelectedBuildings[k].Position.y = j) Then Begin
        For l := k To high(fSelectedBuildings) - 1 Do Begin
          fSelectedBuildings[l] := fSelectedBuildings[l + 1];
        End;
        setlength(fSelectedBuildings, high(fSelectedBuildings));
        break;
      End;
    End;
    If Assigned(fSideMenuObject) And
      (fSideMenuObject Is TBuilding) And
      (round(TBuilding(fSideMenuObject).Position.x) = i) And
      (round(TBuilding(fSideMenuObject).Position.y) = j) Then Begin
      fSideMenuObject := Nil;
      If high(fSelectedBuildings) > -1 Then Begin
        fSideMenuObject := fSelectedBuildings[0];
      End;
    End;
    If Assigned(FHintObject.Obj) And
      (FHintObject.Obj Is TBuilding) And
      (round(TBuilding(FHintObject.Obj).Position.x) = i) And
      (round(TBuilding(FHintObject.Obj).Position.y) = j) Then Begin
      FHintObject.Obj := Nil;
    End;
    fmap.RemoveBuilding(i, j, -1);
    inc(index, 2 * sizeof(Integer));
  End;
End;

Procedure Tctd.SetOnHostButtonClick(AValue: TNotifyEvent);
Begin
  fHostButton.OnClick := AValue;
End;

Procedure Tctd.SetOnJoinButtonClick(AValue: TNotifyEvent);
Begin
  fJoinButton.OnClick := AValue;
End;

Procedure Tctd.SetOnNewMapButtonClick(AValue: TNotifyEvent);
Begin
  fNewMapButton.OnClick := AValue;
End;

Procedure Tctd.SetOnLoadMapButtonClick(AValue: TNotifyEvent);
Begin
  fLoadMapButton.OnClick := AValue;
End;

Procedure Tctd.SetOnLoadGameButtonClick(AValue: TNotifyEvent);
Begin
  fLoadGameButton.OnClick := AValue;
End;

Procedure Tctd.SetMenuPosition(AValue: TMenuPosition);
Begin
  If fMenuPosition = AValue Then Exit;
  fMenuPosition := AValue;
  If assigned(fCaptured_OpenGLControl) Then Begin
    Resize;
  End;
End;

Procedure Tctd.SetScreenTo(x, y: integer);
Var
  i, j, kx, ky: Integer;
Begin
  If Not assigned(fMap) Then exit;
  // i und j Zeigen die Position auf welche geklickt wurde
  i := round(ConvertDimension(fPreviewL, fPreviewL + fPreviewW, x, 0, fMap.Width));
  j := round(ConvertDimension(fPreviewT, fPreviewT + fPreviewH, y, 0, fMap.Height));
  // kx, ky = Anzahl Sichtbarer Kacheln auf dem Schirm
  kx := fMapW Div MapBlockSize;
  ky := fMapH Div MapBlockSize;
  fsx := (i - (kx Div 2)) * MapBlockSize;
  fsy := (j - (ky Div 2)) * MapBlockSize;
  Check_Scrollborders;
End;

Function Tctd.GetIsInEditMode: boolean;
Begin
  result := fgameState = gs_EditMode;
End;

Function Tctd.GetPlayer(index: integer): TPlayerInfos;
Begin
  result := fPlayerInfos[index];
End;

Procedure Tctd.OnEndFiletransfer;
Var
  s: String;
  b: Boolean;
  m: TMemoryStream;
Begin
  fFileReceivingCount := fFileReceivingCount + 1;
  If high(FReceivingQueue) = -1 Then Begin // Wenn alle zu Empfangenen Dateien empfangen sind
    If FOnReceivingFilesFinish.Need Then Begin
      FOnReceivingFilesFinish.Need := false;
      LoadMapFromShare(FOnReceivingFilesFinish.MapName);
    End;
    If FOnReceivingFilesFinish.NeedGamingData Then Begin
      FOnReceivingFilesFinish.NeedGamingData := false;
      HandleLoadGamingData(FOnReceivingFilesFinish.Data);
      FOnReceivingFilesFinish.Data.free;
    End;
    If FOnReceivingFilesFinish.NeedRefreshBackTex Then Begin
      b := BlockMapUpdateSending;
      BlockMapUpdateSending := true;
      // Sieht unsinnig aus, funktioniert aber, weil Backtex nen Setter hat ;)
      s := fMap.BackTex;
      fMap.BackTex := '';
      fMap.BackTex := s;
      BlockMapUpdateSending := b;
    End;
    If FOnReceivingFilesFinish.NeedRefreshDC1 Then Begin
      b := BlockMapUpdateSending;
      BlockMapUpdateSending := true;
      // Sieht unsinnig aus, funktioniert aber, weil DamageClass*Tex nen Setter hat ;)
      s := fMap.DamageClass1Tex;
      fMap.DamageClass1Tex := '';
      fMap.DamageClass1Tex := s;
      If assigned(OnUpdateMapProperty) Then Begin
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        m.Position := 0;
        OnUpdateMapProperty(self, mpDC1Tex, m);
        m.free;
      End;
      BlockMapUpdateSending := b;
    End;
    If FOnReceivingFilesFinish.NeedRefreshDC2 Then Begin
      b := BlockMapUpdateSending;
      BlockMapUpdateSending := true;
      // Sieht unsinnig aus, funktioniert aber, weil DamageClass*Tex nen Setter hat ;)
      s := fMap.DamageClass2Tex;
      fMap.DamageClass2Tex := '';
      fMap.DamageClass2Tex := s;
      If assigned(OnUpdateMapProperty) Then Begin
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        m.Position := 0;
        OnUpdateMapProperty(self, mpDC2Tex, m);
        m.free;
      End;
      BlockMapUpdateSending := b;
    End;
    If FOnReceivingFilesFinish.NeedRefreshDC3 Then Begin
      b := BlockMapUpdateSending;
      BlockMapUpdateSending := true;
      // Sieht unsinnig aus, funktioniert aber, weil DamageClass*Tex nen Setter hat ;)
      s := fMap.DamageClass3Tex;
      fMap.DamageClass3Tex := '';
      fMap.DamageClass3Tex := s;
      If assigned(OnUpdateMapProperty) Then Begin
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        m.Position := 0;
        OnUpdateMapProperty(self, mpDC3Tex, m);
        m.free;
      End;
      BlockMapUpdateSending := b;
    End;
    If FOnReceivingFilesFinish.NeedRefreshDC4 Then Begin
      b := BlockMapUpdateSending;
      BlockMapUpdateSending := true;
      // Sieht unsinnig aus, funktioniert aber, weil DamageClass*Tex nen Setter hat ;)
      s := fMap.DamageClass4Tex;
      fMap.DamageClass4Tex := '';
      fMap.DamageClass4Tex := s;
      If assigned(OnUpdateMapProperty) Then Begin
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        m.Position := 0;
        OnUpdateMapProperty(self, mpDC4Tex, m);
        m.free;
      End;
      BlockMapUpdateSending := b;
    End;
  End;
End;

Procedure Tctd.OnLevelUpButtonClick(Sender: TObject);
Var
  m: TMemoryStream;
  i, j, k: Integer;
Begin
  If fSideMenuObject Is THero Then exit; // Eigentlich egal weil TBuilding unten geprüft wird ..
  If (fgameState = gs_Gaming) And
    (Not fpausing) And
    (Assigned(fSideMenuObject)) And
    (assigned(fSelectedBuildings)) And
    (fSideMenuObject Is TBuilding) And
    (TBuilding(fSideMenuObject).Owner = fPlayerIndex) And
    (TBuilding(fSideMenuObject).fUpdating.State = usIdleInactive) And
    (TBuilding(FSideMenuObject).Stage < high(TBuilding(FSideMenuObject).Stages)) Then Begin
    log('Tctd.OnLevelUpButtonClick', llTrace);
    // 2 Möglichkeiten der Player kann sich Alle Updates Leisten, oder eben nur Einige
    If fPlayerInfos[fPlayerIndex].Cash >= length(fSelectedBuildings) * fSelectedBuildings[0].Stages[fSelectedBuildings[0].Stage + 1].Cost Then Begin
      // Der Spieler Kann sich alles Leisten
      m := TMemoryStream.Create;
      i := fPlayerIndex;
      m.Write(i, SizeOf(i));
      For j := 0 To high(fSelectedBuildings) Do Begin
        i := round(fSelectedBuildings[j].Position.x);
        m.Write(i, SizeOf(i));
        i := round(fSelectedBuildings[j].Position.y);
        m.Write(i, SizeOf(i));
      End;
      SendChunk(miWantLevelUpBuilding, m);
    End
    Else Begin
      // Nicht Alle können Geuppt werden
      k := trunc(fPlayerInfos[fPlayerIndex].Cash / fSelectedBuildings[0].Stages[fSelectedBuildings[0].Stage + 1].Cost);
      If k > 0 Then Begin
        m := TMemoryStream.Create;
        i := fPlayerIndex;
        m.Write(i, SizeOf(i));
        For j := high(fSelectedBuildings) Downto high(fSelectedBuildings) - k + 1 Do Begin
          i := round(fSelectedBuildings[j].Position.x);
          m.Write(i, SizeOf(i));
          i := round(fSelectedBuildings[j].Position.y);
          m.Write(i, SizeOf(i));
        End;
        SendChunk(miWantLevelUpBuilding, m);
        setlength(fSelectedBuildings, length(fSelectedBuildings) - k);
        fSideMenuObject := fSelectedBuildings[0];
      End;
    End;
    LogLeave;
  End;
End;

Procedure Tctd.OnSellButtonClick(Sender: TObject);
Var
  m: TMemoryStream;
  i, j: integer;
Begin
  If fSideMenuObject Is THero Then exit; // Eigentlich egal weil TBuilding unten geprüft wird ..
  log('Tctd.OnSellButtonClick', llTrace);
  If (fgameState = gs_Gaming) And
    (Not fpausing) And
    (Assigned(fSideMenuObject)) And
    (fSideMenuObject Is TBuilding) And
    (TBuilding(fSideMenuObject).Owner = fPlayerIndex) And
    (TBuilding(fSideMenuObject).fUpdating.State = usIdleInactive) Then Begin
    m := TMemoryStream.Create;
    i := fPlayerIndex;
    m.Write(i, SizeOf(i));
    For j := 0 To high(fSelectedBuildings) Do Begin
      i := round(fSelectedBuildings[j].Position.x);
      m.Write(i, SizeOf(i));
      i := round(fSelectedBuildings[j].Position.y);
      m.Write(i, SizeOf(i));
    End;
    If m.size <> SizeOf(i) Then Begin
      SendChunk(miSellBuilding, m);
    End
    Else Begin
      m.free;
    End;
    fSelectedBuildings := Nil;
    fSideMenuObject := Nil;
  End;
  LogLeave;
End;

Procedure Tctd.OnTabButtonClick(Sender: TObject);
Begin
  fSidemenuVisible := Not fSidemenuVisible;
  Resize;
End;

Procedure Tctd.OnStrategyButtonClick(Sender: TObject);
Var
  m: TMemoryStream;
  i, j: Integer;
  b: Boolean;
Begin
  If (fgameState = gs_Gaming) And
    (Assigned(fSideMenuObject)) And
    (fSideMenuObject Is TBuilding) And
    (TBuilding(fSideMenuObject).Owner = fPlayerIndex) { And (TBuilding(fSideMenuObject).fUpdating.State = 0)} Then Begin
    log('Tctd.OnBuildingStrategyButtonClick', llTrace);
    m := TMemoryStream.Create;
    i := fPlayerIndex;
    m.Write(i, SizeOf(i));
    For j := 0 To high(fSelectedBuildings) Do Begin
      If (fSelectedBuildings[j].Owner = fPlayerIndex) Then Begin
        fSelectedBuildings[j].strategy := TBuildingStrategyButton(Sender).Strategy;
        i := round(fSelectedBuildings[j].Position.x);
        m.Write(i, SizeOf(i));
        i := round(fSelectedBuildings[j].Position.y);
        m.Write(i, SizeOf(i));
        b := fSelectedBuildings[j].PreverAir;
        m.Write(b, SizeOf(b));
        m.Write(TBuildingStrategyButton(Sender).Strategy, SizeOf(TBuildingStrategyButton(Sender).Strategy));
      End;
    End;
    If m.size <> SizeOf(i) Then Begin
      SendChunk(miChangeBuildingStrategy, m);
    End
    Else Begin
      m.free;
    End;
    LogLeave;
  End;
  If (fgameState = gs_Gaming) And
    (Assigned(fSideMenuObject)) And
    (fSideMenuObject Is THero) And
    (THero(fSideMenuObject).Owner = fPlayerIndex) { And (THero(fSideMenuObject).fUpdating.State = 0)} Then Begin
    log('Tctd.OnBuildingStrategyButtonClick', llTrace);
    m := TMemoryStream.Create;
    i := fPlayerIndex;
    m.Write(i, SizeOf(i));
    For j := 0 To high(fSelectedHeros) Do Begin
      If fSelectedHeros[j].Owner = fPlayerIndex Then Begin
        fSelectedHeros[j].strategy := TBuildingStrategyButton(Sender).Strategy;
        i := fSelectedHeros[j].MapHeroIndex;
        m.Write(i, SizeOf(i));
        b := fSelectedHeros[j].PreverAir;
        m.Write(b, SizeOf(b));
        m.Write(TBuildingStrategyButton(Sender).Strategy, SizeOf(TBuildingStrategyButton(Sender).Strategy));
      End;
    End;
    If m.size <> SizeOf(i) Then Begin
      SendChunk(miChangeHeroStrategy, m);
    End
    Else Begin
      m.free;
    End;
    LogLeave;
  End;
End;

Procedure Tctd.OnAirStrategyButtonClick(Sender: TObject);
Var
  m: TMemoryStream;
  i, j: Integer;
  b: Boolean;
Begin
  If (fgameState = gs_Gaming) And
    (Assigned(fSideMenuObject)) And
    (fSideMenuObject Is TBuilding) And
    (TBuilding(fSideMenuObject).Owner = fPlayerIndex) { And (TBuilding(fSideMenuObject).fUpdating.State = 0)} Then Begin
    log('Tctd.OnBuildingStrategyButtonClick', llTrace);
    m := TMemoryStream.Create;
    i := fPlayerIndex;
    m.Write(i, SizeOf(i));
    For j := 0 To high(fSelectedBuildings) Do Begin
      If (fSelectedBuildings[j].Owner = fPlayerIndex) Then Begin
        fSelectedBuildings[j].PreverAir := Not fSelectedBuildings[0].PreverAir;
        i := round(fSelectedBuildings[j].Position.x);
        m.Write(i, SizeOf(i));
        i := round(fSelectedBuildings[j].Position.y);
        m.Write(i, SizeOf(i));
        b := fSelectedBuildings[j].PreverAir;
        m.Write(b, SizeOf(b));
        m.Write(TBuildingStrategyButton(Sender).Strategy, SizeOf(TBuildingStrategyButton(Sender).Strategy));
      End;
    End;
    If m.size <> SizeOf(i) Then Begin
      SendChunk(miChangeBuildingStrategy, m);
    End
    Else Begin
      m.free;
    End;
    LogLeave;
  End;
  If (fgameState = gs_Gaming) And
    (Assigned(fSideMenuObject)) And
    (fSideMenuObject Is THero) And
    (THero(fSideMenuObject).Owner = fPlayerIndex) { And (THero(fSideMenuObject).fUpdating.State = 0)} Then Begin
    log('Tctd.OnBuildingStrategyButtonClick', llTrace);
    m := TMemoryStream.Create;
    i := fPlayerIndex;
    m.Write(i, SizeOf(i));
    For j := 0 To high(fSelectedHeros) Do Begin
      If fSelectedHeros[j].Owner = fPlayerIndex Then Begin
        fSelectedHeros[j].PreverAir := Not fSelectedHeros[0].PreverAir;
        i := fSelectedHeros[j].MapHeroIndex;
        m.Write(i, SizeOf(i));
        b := fSelectedHeros[j].PreverAir;
        m.Write(b, SizeOf(b));
        m.Write(TBuildingStrategyButton(Sender).Strategy, SizeOf(TBuildingStrategyButton(Sender).Strategy));
      End;
    End;
    If m.size <> SizeOf(i) Then Begin
      SendChunk(miChangeHeroStrategy, m);
    End
    Else Begin
      m.free;
    End;
    LogLeave;
  End;
End;

Procedure Tctd.OnStrategyButtonMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  If Not assigned(fBuyingObject) And // Kein Gebäude Zum Bauen
  assigned(FSideMenuObject) And (FSideMenuObject Is TBuilding) And // Überhaupt ein Gebäude angewählt
  (TBuilding(FSideMenuObject).Owner = fPlayerIndex) And // Das Gebäude gehört uns
  (TBuilding(FSideMenuObject).Stage >= 0) And (TBuilding(FSideMenuObject).Stages[TBuilding(FSideMenuObject).Stage].range <> 0) Then Begin // Das Gebäude kann überhaupt Strategieen fahren ..
    fStrategyToolTipp := TBuildingStrategyButton(sender).Name;
    fStrategyToolTippPos := point(TBuildingStrategyButton(sender).left + x, TBuildingStrategyButton(sender).Top + y);
  End;
End;

Procedure Tctd.OnUpgradeSellMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  If sender = Level_Up_Image Then Begin
    If Level_Up_Image.Hint = '' Then exit;
    fStrategyToolTipp := Level_Up_Image.Hint;
  End
  Else Begin
    If Sell_Image.Hint = '' Then exit;
    fStrategyToolTipp := Sell_Image.Hint;
  End;
  fStrategyToolTippPos := point(TOpenGl_HintImage(sender).left + x, TOpenGl_HintImage(sender).Top + y);
End;

Procedure Tctd.Connection_Connect(aSocket: TLSocket);
Var
  m: TMemoryStream;
Begin
  // Der Client ist beim Server Registriert, nun gilt es um die Mitspielerlaubniss zu fragen.
  log('Tctd.Connection_Connect', llTrace);
  fChunkManager.SetNoDelay(true);
  m := TMemoryStream.Create;
  m.WriteAnsiString(FuserName);
  m.WriteAnsiString(fPassword);
  m.Write(version, sizeof(version));
  SendChunk(miRequestLogin, m);
  LogLeave;
End;

Procedure Tctd.Connection_Disconnect(aSocket: TLSocket);
Begin
  log('Tctd.Connection_Disconnect', llTrace);
  // Verbindung zum Server verloren / getrennt
  If assigned(OnDisconnectFromServer) Then Begin
    OnDisconnectFromServer(self);
  End;
  fgameState := gs_WaitForJoin;
  LogLeave;
End;

Procedure Tctd.Connection_Error(Const msg: String; aSocket: TLSocket);
Begin
  log('Tctd.Connection_Error', llTrace);
  LogShow(msg, llError);
  If assigned(OnDisconnectFromServer) Then Begin
    OnDisconnectFromServer(self);
  End;
  fgameState := gs_WaitForJoin;
  LogLeave;
End;

Procedure Tctd.OnUDPConnection_Receive(aSocket: TLSocket);
Var
  Buffer: Array[0..1023] Of byte;
  cnt: integer;
  B: Byte;
  i: Integer;
  serverIP, ServerUser: String;
  bool: Boolean;
Begin
  log('Tctd.OnUDPConnection_Receive', llTrace);
  Repeat
    cnt := aSocket.Get(buffer, 1024);
    If cnt <> 0 Then Begin
      b := 42;
      ServerUser := '';
      setlength(ServerUser, cnt - 1);
      For i := 0 To cnt - 1 Do Begin
        b := b Xor buffer[i];
        If i <> cnt - 1 Then Begin
          ServerUser[i + 1] := chr(buffer[i]);
        End;
      End;
      If b = 0 Then Begin //
        log('Tctd.OnUDPConnection_Receive, CRC OK = ' + ServerUser, llInfo);
        serverIP := aSocket.PeerAddress;
        bool := true;
        For i := 0 To high(fOpenGames) Do Begin
          If (fOpenGames[i].IP = serverIP) Then Begin
            fOpenGames[i].Name := copy(ServerUser, 1, pos(':', ServerUser) - 1);
            fOpenGames[i].Port := copy(ServerUser, pos(':', ServerUser) + 1, length(ServerUser));
            fOpenGames[i].LastValidTimeStamp := GetTick();
            bool := false;
            break;
          End;
        End;
        If bool Then Begin
          If serverIP <> '127.0.0.1' Then Begin // Den Loopback adapter klammern wir aus, da der auch über die IP der Netzwerkkarte rein kommt und sonst doppelt wäre
            setlength(fOpenGames, high(fOpenGames) + 2);
            fOpenGames[high(fOpenGames)].IP := serverIP;
            fOpenGames[high(fOpenGames)].Name := copy(ServerUser, 1, pos(':', ServerUser) - 1);
            fOpenGames[high(fOpenGames)].Port := copy(ServerUser, pos(':', ServerUser) + 1, length(ServerUser));
            fOpenGames[high(fOpenGames)].LastValidTimeStamp := GetTick();
          End;
        End;
      End;
    End;
  Until cnt = 0;
  LogLeave;
End;

Procedure Tctd.ModifyTerrain(kx, ky: integer);
Var
  m: TMemoryStream;
  i, j, c, d: integer;
Begin
  c := 0;
  If form4.CheckBox3.Checked Then
    c := c Or Begehbar;
  If form4.CheckBox4.Checked Then
    c := c Or Bebaubar;
  For i := -form4.ScrollBar1.Position To +form4.ScrollBar1.Position Do
    For j := -form4.ScrollBar1.Position To +form4.ScrollBar1.Position Do Begin
      // If sqr(form4.ScrollBar1.Position) >= sqr(i) + sqr(j) Then -- Macht nen Runden Cursor
      If (kx + i >= 0) And (kx + i < map.Width) And
        (ky + j >= 0) And (ky + j < map.Height) And
        (Map.fTerrain[kx + i, ky + j].data <> c) Then Begin
        Map.fTerrain[kx + i, ky + j].data := c;
        map.UpdateBackTexCoord(kx + i, ky + j);
        m := TMemoryStream.Create;
        d := kx + i;
        m.write(d, sizeof(d));
        d := ky + j;
        m.write(d, sizeof(d));
        m.write(c, sizeof(c));
        UpdateMapProperty(mpCoord, m);
      End;
    End;
End;

Procedure Tctd.ModifyWaypointArea(kx, ky: integer);
Var
  b: Boolean;
  i, j, d, c: Integer;
  m: TMemoryStream;
Begin
  b := Form4.RadioButton6.Checked; // Add / Remove
  For i := -form4.ScrollBar2.Position To +form4.ScrollBar2.Position Do
    For j := -form4.ScrollBar2.Position To +form4.ScrollBar2.Position Do Begin
      // If sqr(form4.ScrollBar2.Position) >= sqr(i) + sqr(j) Then -- Macht nen Runden Cursor
      If (kx + i >= 0) And (kx + i < map.Width) And
        (ky + j >= 0) And (ky + j < map.Height) And
        (Map.fTerrain[kx + i, ky + j].WArea <> b) Then Begin
        Map.fTerrain[kx + i, ky + j].WArea := b;
        m := TMemoryStream.Create;
        d := kx + i;
        m.write(d, sizeof(d));
        d := ky + j;
        m.write(d, sizeof(d));
        c := ord(b);
        m.write(c, sizeof(c));
        UpdateMapProperty(mpWCoord, m);
      End;
    End;
End;

Procedure Tctd.CreateSplashMark(x, y: integer);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.Write(x, SizeOf(x));
  m.Write(y, SizeOf(y));
  SendChunk(miCreateSplashMark, m);
End;

Procedure Tctd.CalcSumKillsLives(Out SumKills: integer; Out SumLives: integer);
Var
  i: integer;
Begin
  SumKills := 0;
  SumLives := 0;
  If fmap.MapType In [mtCoop, mtCoopMaze] Then Begin
    For i := 0 To high(fPlayerInfos) Do Begin
      SumKills := SumKills + fPlayerInfos[i].Kills + fPlayerInfos[i].BonusFinisher - fPlayerInfos[i].KillsOnWaveStart;
      SumLives := SumLives + fPlayerInfos[i].Lives - fPlayerInfos[i].LivesOnWaveStart;
    End;
    SumLives := SumLives Div length(fPlayerInfos);
  End
  Else Begin
    SumKills := fPlayerInfos[fPlayerIndex].Kills + fPlayerInfos[fPlayerIndex].BonusFinisher - fPlayerInfos[fPlayerIndex].KillsOnWaveStart;
    SumLives := fPlayerInfos[fPlayerIndex].Lives - fPlayerInfos[fPlayerIndex].LivesOnWaveStart;
  End;
End;

Procedure Tctd.DoWaveClone(SourceWaveNum, DestWaveNum: integer);
Begin
  If assigned(fMap) Then Begin
    fMap.CloneWave(SourceWaveNum, DestWaveNum);
    If assigned(OnWaveCloneEvent) Then Begin // Die LCL- Updaten
      OnWaveCloneEvent(self, SourceWaveNum, DestWaveNum);
    End;
  End;
End;

Procedure Tctd.HandleOnDisconnectFromServer;
Begin
  log('Tctd.HandleOnDisconnectFromServer', llTrace);
  fgameState := gs_WaitForJoin;
  If assigned(OnDisconnectFromServer) Then Begin
    OnDisconnectFromServer(self);
  End;
  LogLeave;
End;

Procedure Tctd.HandleUpdateMapProperty(MapProperty, SenderUID: Integer;
  Data: TStream);
Var
  s: String;
  p: int64;
  i, j, c: Integer;
  tmp: Tpoint;
  b: Boolean;
  bk: TBuyAbleKind;
Begin
  log('Tctd.HandleUpdateMapProperty : ' + MessageMapPropertyToString(MapProperty), llTrace);
  b := BlockMapUpdateSending;
  BlockMapUpdateSending := true; // Unterdrücken dass die LCL Komponenten irgendwelche Refreshes zum Server Senden
  p := data.Position;
  (*
   * Der Code in HandleUpdateMapProperty.inc übernimmt die Informationen aus Data und schreibt sie in FMap
   * dies ist bei Server und Client Identisch, zur Reduktion von Redundantem
   * Code ist aus diesem Grund der Folgende Code in das .inc ausgelagert.
   *)
{$I HandleUpdateMapProperty.inc}
  data.Position := p;
  If (MapProperty = mpBackTex) And (high(FReceivingQueue) <> -1) Then Begin // Wenn die Textur erst übertragen wird, dann wird sie hier nicht Richtig geladen, so wird sie nach Abschluss der Dateiübertragung noch mal gesetzt
    FOnReceivingFilesFinish.NeedRefreshBackTex := true;
  End;
  If (MapProperty = mpDC1Tex) And (high(FReceivingQueue) <> -1) Then Begin // Wenn die Textur erst übertragen wird, dann wird sie hier nicht Richtig geladen, so wird sie nach Abschluss der Dateiübertragung noch mal gesetzt
    FOnReceivingFilesFinish.NeedRefreshDC1 := true;
  End;
  If (MapProperty = mpDC2Tex) And (high(FReceivingQueue) <> -1) Then Begin // Wenn die Textur erst übertragen wird, dann wird sie hier nicht Richtig geladen, so wird sie nach Abschluss der Dateiübertragung noch mal gesetzt
    FOnReceivingFilesFinish.NeedRefreshDC2 := true;
  End;
  If (MapProperty = mpDC3Tex) And (high(FReceivingQueue) <> -1) Then Begin // Wenn die Textur erst übertragen wird, dann wird sie hier nicht Richtig geladen, so wird sie nach Abschluss der Dateiübertragung noch mal gesetzt
    FOnReceivingFilesFinish.NeedRefreshDC3 := true;
  End;
  If (MapProperty = mpDC4Tex) And (high(FReceivingQueue) <> -1) Then Begin // Wenn die Textur erst übertragen wird, dann wird sie hier nicht Richtig geladen, so wird sie nach Abschluss der Dateiübertragung noch mal gesetzt
    FOnReceivingFilesFinish.NeedRefreshDC4 := true;
  End;
  // Wenn wir selbst der Absender sind, brauchen wirs nicht an die LCL mitteilen, die stimmt ja dann schon
  If assigned(OnUpdateMapProperty) And (fServerUid <> SenderUID) Then { #todo -cFIXME : Die Bedingung fServerUID <> SenderUID raus nehmen und alles nur noch im Event abhandeln, das müsste den Code deutlich entschlacken .. }
    OnUpdateMapProperty(self, MapProperty, data);
  BlockMapUpdateSending := b; // Unterdrücken dass die LCL Komponenten irgendwelche Refreshes zum Server Senden
  LogLeave;
End;

Procedure Tctd.HandleLoadGamingData(Const Stream: TStream);
Begin
  Map.LoadGameingData(stream);
  fSelectedBuildings := Nil;
  fSelectedHeros := Nil;
  If assigned(OnHandleLoadGameingData) Then
    OnHandleLoadGameingData(self);
End;

Procedure Tctd.HandleOnEndRound(Succeed: Boolean; Round: integer;
  Const Data: TStream);
Var
  i: integer;
  msg, w, v: String;
Begin
  msg := '';
  log('Tctd.HandleOnEndRound : ' + inttostr(ord(Succeed)), llTrace);
  If fSideMenuObject Is TOpponent Then Begin
    fSideMenuObject := Nil; // Wenn ein Gegner angewählt war, dann diesen Abwählen, der ist eh hinüber ..
  End;
  If FHintObject.Obj Is TOpponent Then Begin
    FHintObject.Obj := Nil;
  End;
  fAktualWave := Round;
  // 1. Anzeigen wer gewonnen hat
  If Succeed Then Begin
    // Alle Spieler haben "Gewonnen", es kann die Nächste Runde gespielt werden
    msg := 'You have successfully survived this wave. Be aware, the next wave will be harder!';
    If (Round + 1 = length(fMap.Waves)) Then Begin
      msg := 'You have successfully survived. Now it''s your turn, ...';
    End;
    Splashhint(msg, v3(1, 1, 0));
  End
  Else Begin
    SwitchToEditMode();
    // Mindestens ein Spieler hat verloren, Das Spiel ist vorbei
    w := '';
    v := '';
    For i := 0 To high(fPlayerInfos) Do Begin
      If fPlayerInfos[i].Lives <= 0 Then Begin
        If v <> '' Then v := v + ', ';
        v := v + fPlayerInfos[i].Name;
      End
      Else Begin
        If w <> '' Then w := w + ', ';
        w := w + fPlayerInfos[i].Name;
      End;
    End;
    If w = '' Then Begin
      // Alle Haben Verloren
      msg := 'All players have lost the game! Try harder!';
      Splashhint(msg, v3(1, 0, 0));
    End
    Else Begin
      msg := 'Winner : ' + w + LineEnding +
        'Loser : ' + v;
      Splashhint(msg, DefaultSplashHintColor);
    End;
  End;
  If Assigned(OnEndRound) Then Begin
    OnEndRound(self, succeed, fAktualWave);
  End;
  If (Not Succeed) Or (Round + 1 = length(fMap.Waves)) Then Begin
    If assigned(OnShowGameStatistics) Then Begin
      OnShowGameStatistics(msg, data);
    End;
  End;
  LogLeave;
End;

Procedure Tctd.LoadMapFromShare(MapName_: String);
Begin
  log('Tctd.LoadMapFromShare : ' + MapName_, llTrace);
  SwitchToEditMode();
  If Assigned(fMap) Then fmap.free;
  fmap := TMap.Create;
  fmap.Load(MapName_);
  If assigned(OnLoadMap) Then
    OnLoadMap(self);
  LogLeave;
End;

Procedure Tctd.Check_Scrollborders;
Begin
  If assigned(fmap) Then Begin
    fsx := max(fsx, 0);
    fsy := max(fsy, 0);
    If fmap.width * MapBlockSize - fMapW + fMapL > 0 Then Begin
      fsx := min(fsx, fmap.width * MapBlockSize - fMapW + fMapL);
    End
    Else Begin
      fsx := 0;
    End;
    If fmap.Height * MapBlockSize - fMapH + fMapT > 0 Then Begin
      fsy := min(fsy, fmap.Height * MapBlockSize - fMapH + fMapT);
    End
    Else Begin
      fsy := 0;
    End;
  End;
End;

Procedure Tctd.Zoom(ZoomIn: Boolean);
Var
  i, j: integer;
Begin
  // Das ist die Koordinate in der Mitte des Monitors
  i := (fsx + fMapW Div 2) Div MapBlockSize;
  j := (fsy + fMapH Div 2) Div MapBlockSize;
  If ZoomIn Then Begin
    MapBlockSize := max(10, round(MapBlockSize * 0.9)); // Beschrängek rauszoomen auf keine Kachek kleiner als 10 Pixel
  End
  Else Begin
    MapBlockSize := min(round(MapBlockSize * 1.1), 100); // Zoom Beschrängek auf max 10-Fach, dass sollte Reichen
  End;
  // Das Scrolling so verschieben, dass es die Alte Koordinate wieder Zentriert
  fsx := i * MapBlockSize - fMapW Div 2;
  fsy := j * MapBlockSize - fMapH Div 2;
  SetValue('Global', 'MapBlockSize', inttostr(MapBlockSize));
  Check_Scrollborders;
End;

Procedure Tctd.PingForOpenGames;
Var
  N: TNetworkAdapterList;
  i: Integer;
  s: String;
Begin
  log('Tctd.PingForOpenGames', llTrace);
  If assigned(fUDPConnection) Then Begin
    Try
      n := GetLocalIPs();
      For i := 0 To high(n) Do Begin
        s := CalculateBroadCastAddressFromAddress(n[i].IpAddress, n[i].SubnetMask);
        fUDPConnection.SendMessage('Ping', s);
      End;
    Except
      log('Could not ping, maybe there is no valid network card present.', llCritical);
    End;
  End;
  LogLeave;
End;

Function Tctd.AktualGameList: TStringlist;
Var
  n: int64;
  i: Integer;
Begin
  result := TStringlist.create;
  n := GetTick();
  For i := 0 To High(fOpenGames) Do Begin
    If n - fOpenGames[i].LastValidTimeStamp <= OpenGamesTimeout Then Begin
      result.Add(format('%s:%s#%s', [fOpenGames[i].Name, fOpenGames[i].IP, fOpenGames[i].Port]));
    End;
  End;
End;

Procedure Tctd.SelectAllBuildingsLikeTheSelected; // TODO: Umbenennen in  SelectAllBuyablesLikeTheSelected
Begin
  If assigned(fSideMenuObject) And (fSideMenuObject Is TBuilding) And Not assigned(fBuyingObject) Then Begin
    fSelectedBuildings := fMap.GetAllBuildingsSameOfSameType(TBuilding(fSideMenuObject));
  End;
  If assigned(fSideMenuObject) And (fSideMenuObject Is THero) And Not assigned(fBuyingObject) Then Begin
    fSelectedHeros := fMap.GetAllHerosOfSameType(THero(fSideMenuObject));
  End;
End;

Procedure Tctd.SendMapRating(Rating: integer);
Var
  m: TMemoryStream;
Begin
  log('Tctd.SendMapRating', llTrace);
  If assigned(fMap) Then Begin
    m := TMemoryStream.Create;
    m.Write(Rating, SizeOf(Rating));
    SendChunk(miSendMapRating, m);
  End;
  LogLeave;
End;

Procedure Tctd.RequestHighscores;
Begin
  log('Tctd.RequestHighscores', llTrace);
  SendChunk(miRequestMapHighscoresAndRating, Nil);
  LogLeave;
End;

Procedure Tctd.RequestClearHighscore;
Begin
  log('Tctd.RequestClearHighscore', llTrace);
  SendChunk(miRequestClearMapHighscores, Nil);
  LogLeave;
End;

Procedure Tctd.AddNewRandomWave(Difficulty: integer);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.Write(Difficulty, sizeof(Difficulty));
  SendChunk(miAddRandomWave, m);
End;

Procedure Tctd.NilSideObject(Obj: tctd_mapopbject);
Begin
  If fSideMenuObject = Obj Then Begin
    fSideMenuObject := Nil;
  End;
End;

Procedure Tctd.RenderBlackOutMapBorder;
Begin
  glPushMatrix;
  glTranslatef(0, 0, ctd_MapBlackOutLayer);
  glcolor4f(0, 0, 0, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBegin(GL_QUADS);
  Case fMenuPosition Of
    mpBottom: Begin
        glVertex2f(0, fMapH + fMapT);
        glVertex2f(fwold, fMapH + fMapT);
        glVertex2f(fwold, fhold);
        glVertex2f(0, fhold);
      End;
    mpRight: Begin
        glVertex2f(fMapW, 0);
        glVertex2f(fMapW, fhold);
        glVertex2f(fwold, fhold);
        glVertex2f(fwold, 0);
      End;
  End;
  glend;
  glPopMatrix;
End;

Procedure Tctd.RenderBuyMenu;
Var
  i, j, x, y, k: integer;
Begin
  // TODO: Fix Bug, dass die Gebs unten raus fallen
  glPushMatrix;
  glTranslatef(fBuildL, fBuildT, 0);
  glcolor4f(1, 1, 1, 1);
  k := fBuildW Div (MapBlockSize * FBuyMenu.MaxItemSize.x); // Anzahl der Zeichenbaren Kacheln in der Breite
  If k = 0 Then Begin
    log('could not render build menu k = 0', llCritical);
    glPopMatrix;
    exit;
  End;
  //l := fBuildh Div (MapBlockSize * FBuildingMenu.MaxItemSize.y); // Anzahl der Zeichenbaren Kacheln in der Höhe
  // Rendern des Gekachelten Hintergrundes
  glPushMatrix;
  glTranslatef(0, 0, ctd_BuyEditorLayer);
  For i := 0 To (fBuildW Div MapBlockSize) - 1 Do
    For j := 0 To (fBuildH Div MapBlockSize) - 1 Do Begin
      RenderObjItem(point(i * MapBlockSize + MapBlockSize Div 2, j * MapBlockSize + MapBlockSize Div 2), MapBlockSize, MapBlockSize, fBuyKachel);
    End;
  glPopMatrix;
  // Rendern der Kauf Items
  glPushMatrix;
  glTranslatef(0, 0, ctd_BuyEditorLayer + ctd_EPSILON);
  For i := 0 To high(FBuyMenu.Items) Do Begin
    x := (i Mod k) * MapBlockSize * FBuyMenu.MaxItemSize.x;
    y := (i Div k) * MapBlockSize * FBuyMenu.MaxItemSize.y;
    If y + MapBlockSize * FBuyMenu.MaxItemSize.y >= fBuildH Then break; // Wenn die Gebäude unten raus fallen würden...
    // Im Buy Menü gibt es keine Animationen -> Hier Reicht also RenderObj
    RenderObj(point(x + round(FBuyMenu.Items[i].obj.Width * MapBlockSize) Div 2, y + round(FBuyMenu.Items[i].obj.Height * MapBlockSize) Div 2),
      round(FBuyMenu.Items[i].obj.Width * MapBlockSize), round(FBuyMenu.Items[i].obj.Height * MapBlockSize),
      FBuyMenu.Items[i].obj.Fimage);
  End;
  glPopMatrix;
  glPopMatrix;
End;

Procedure Tctd.RenderMenuTabButton;
Begin
  glPushMatrix;
  glTranslatef(0, 0, ctd_Menu_Layer);
  Tab_Image.Render();
  glPopMatrix;
End;

Procedure Tctd.RenderSelected(sx, sy, x, y: integer);
  Procedure RenderBuildingSelector(Const b: TBuilding);
  Var
    mx, my, w, h: Single;
  Begin
    glPushMatrix;
    glTranslatef((b.Stages[max(0, b.Stage)].w * MapBlockSize) / 2, -(b.Stages[max(0, b.Stage)].h * MapBlockSize) / 2 + MapBlockSize, 0);
    mx := round(b.Position.x * MapBlockSize);
    my := round(b.Position.y * MapBlockSize);
    w := b.Stages[max(0, b.Stage)].w * MapBlockSize;
    h := b.Stages[max(0, b.Stage)].h * MapBlockSize;
    glTranslatef(mx - w / 2, my - h / 2, 0);
    glScalef(w / fSelectTex.OrigWidth, h / fSelectTex.OrigHeight, 1);
    RenderAlphaQuad(0, 0, fSelectTex);
    glPopMatrix;
  End;

  Procedure RenderHeroSelector(Const he: THero);
  Var
    mx, my, w, h: Single;
  Begin
    glPushMatrix;
    glTranslatef((he.Levels[max(0, he.Level)].w * MapBlockSize) / 2, -(he.Levels[max(0, he.Level)].h * MapBlockSize) / 2 + MapBlockSize, 0);
    mx := round(he.Position.x * MapBlockSize);
    my := round(he.Position.y * MapBlockSize);
    w := he.Levels[max(0, he.Level)].w * MapBlockSize;
    h := he.Levels[max(0, he.Level)].h * MapBlockSize;
    glTranslatef(mx - w / 2, my - h / 2, 0);
    glScalef(w / fSelectTex.OrigWidth, h / fSelectTex.OrigHeight, 1);
    RenderAlphaQuad(0, 0, fSelectTex);
    glPopMatrix;
  End;

  Procedure RenderOpponentSelector(Const O: TOpponent);
  Var
    lifepointspercent, k, l: Single;
    d: integer;
  Begin
    glPushMatrix;
    k := (MapBlockSize - MapBlockSize * o.SizeX) / 2;
    l := (MapBlockSize - MapBlockSize * o.Sizey) / 2;
    glTranslatef(o.Position.x * MapBlockSize + k, (o.Position.y) * MapBlockSize - l, 0);
    // Die Opponent Placements sind noch ein kleines Bischen Verschoben gerendert (da sie ja als Gebäude behandelt werden)
    If o.Identifier >= PLacementIdentiferOffset Then Begin
      glTranslatef((o.SizeX * MapBlockSize) / 2, -(o.Sizey * MapBlockSize) / 2 + MapBlockSize, 0);
      glTranslatef(-MapBlockSize / 2, -MapBlockSize / 2, 0);
    End;
    // Anfahren des Mittelpunkts Pos des Opps
    glTranslatef((o.SizeX * MapBlockSize) / 2, -(o.Sizey * MapBlockSize) / 2 + MapBlockSize, 0);

    glPushMatrix;
    d := max(round(o.SizeX * MapBlockSize), round(o.SizeY * MapBlockSize));
    glTranslatef(-d / 2, -d / 2, 0);
    glScalef(D / fSelectTex.OrigWidth, D / fSelectTex.OrigHeight, 1);
    RenderAlphaQuad(0, 0, fSelectTex);
    glPopMatrix;

    (* Anzeigen der Lebensenergie ..*)
    If o.Identifier < PLacementIdentiferOffset Then Begin // Placements haben keine Lebensanzeige !
      lifepointspercent := min(1, (o.LifePoints[0] + o.LifePoints[1] + o.LifePoints[2] + o.LifePoints[3]) / o.TotalLivepoints);
      RenderLifeBar(o.SizeX, O.SizeY, lifepointspercent);
    End;
    glPopMatrix;
  End;

Var
  i: Integer;
Begin
  If assigned(fSideMenuObject) Then Begin
    glColor4f(1, 1, 1, 1);
    glPushMatrix;
    glTranslatef(x - sx, y - sy, ctd_Map_Layer + 6 * ctd_EPSILON);
    If fSideMenuObject Is TOpponent Then Begin
      If fMap.UpdateOpponentData(fSidemenuOpponent) Then Begin
        RenderOpponentSelector(TOpponent(fSidemenuOpponent));
      End;
    End;
    If fSideMenuObject Is TBuilding Then Begin
      For i := 0 To high(fSelectedBuildings) Do Begin // Da bei Anwahl eines zu bauenden Gebäudes das Array auf Nil geht passt das so
        RenderBuildingSelector(fSelectedBuildings[i]);
      End;
    End;
    If fSideMenuObject Is THero Then Begin
      For i := 0 To high(fSelectedHeros) Do Begin
        RenderHeroSelector(fSelectedHeros[i]);
      End;
    End;
    glPopMatrix;
  End;
End;

Procedure Tctd.RenderGameInfos;
Var
  SumKills, SumLives: integer;
Begin
  CalcSumKillsLives(SumKills, SumLives);
  fWaveinfo.Text := inttostr(length(fMap.Waves));
  fWaveinfo.Text2 := inttostr(fAktualWave + 1);
  fWaveinfo.Render();
  fLivesInfo.Text := inttostr(fPlayerInfos[fPlayerIndex].Lives);
  fLivesInfo.Render();
  fCoinsInfo.text := format('%.0n', [double(fPlayerInfos[fPlayerIndex].Cash)]);
  fCoinsInfo.Render();
  fTargetsInfo.Text := format('%4d' + LineEnding + '%4d',
    [Map.OpponentCount,
    fOpponentsAtEndOfWave - SumKills + SumLives
      ]);
  fTargetsInfo.Render();
End;

Procedure Tctd.RenderSideMenu;
Var
  i: Integer;
  nshi, hi: THint;
  Sellrefund: integer;
Begin
  (*
   * Eigentlich hat Resize fDestL passend gesetzt dass all das hier "rausgeschoben" wird.
   * Das Problem ist aber das RenderToolTipp den Screen berücksichtigt und explizit den
   * Text wieder nach "innen" Rückt
   *
   * -> Wir Canceln einfach alles komplett
   *)
  If fDestL >= fwold - 1 Then exit;
  If fDestT >= fhold - 1 Then exit;
  If assigned(FBuyingObject) Then Begin
    // Wenn wir gerade dabei sind ein Gebäude zu kaufen
    hi := FBuyingObject.GetHint();
    hi.Name := 'Cost: ' + inttostr(FBuyingObject.GetBuyCost);
    hi.Stage := -1;
    RenderHint(fDestL - 10, fDestT + 30, hi, false);
  End
  Else Begin
    If (FSideMenuObject Is TOpponent) Then Begin
      If fMap.UpdateOpponentData(fSidemenuOpponent) Then Begin
        TOpponent(FSideMenuObject).LifePoints := fSidemenuOpponent.LifePoints;
        RenderHint(fDestL - 10, fDestT + 30, FSideMenuObject.GetHint(), false);
        // Todo : Rendern des Lebenspunkte Balkens über dem Gegner ?? --> Wird das nicht schon unter RenderSelected gemacht ??
      End
      Else Begin
        // Das Objekt gibt es wohl nicht mehr, wurde gekillt ;)
        FSideMenuObject := Nil;
      End;
    End;
    If (FSideMenuObject Is TBuilding) Then Begin // Wenn wir ein Gebäude Updaten oder Verkaufen können
      // Anzeigen der Ranges der Gebäude
      If HintShowBuildingRange Then Begin
        For i := 0 To high(fSelectedBuildings) Do Begin
          glPushMatrix;
          glTranslatef(fSelectedBuildings[i].Position.x * MapBlockSize - fsx + fMapL, fSelectedBuildings[i].Position.y * MapBlockSize - fsy + fMapT, ctd_MapBlackOutLayer - ctd_EPSILON);
          fSelectedBuildings[i].RenderRange();
          glPopMatrix;
        End;
      End;
      // Alle nachfolgenden Element sind Menü Elemente und müssen in das MenüLayer gerendert werden
      Sell_Image.Hint := '';
      Level_Up_Image.Hint := '';
      hi := FSideMenuObject.GetHint();
      // Das Gebäude gehört uns, also Zeigen wir an was es als nächstes Gibt
      If (TBuilding(FSideMenuObject).Owner = fPlayerIndex) Then Begin
        // Das Gebäude gehört uns
        If (TBuilding(FSideMenuObject).fUpdating.State = usIdleInactive) Then Begin // Nicht in Update Mode
          // Das Gebäude darf verkauft werden
          Sellrefund := TBuilding(FSideMenuObject).CalcSellRefund(fMap.Difficulty);
          glPushMatrix;
          glTranslatef(0, 0, ctd_Menu_Layer);
          Sell_Image.Render();
          Sell_Image.Hint := 'Refund: ' + inttostr(Sellrefund);
          glPopMatrix;
          RenderToolTipp(Sell_Image.Left - 15, fDestT + 30, 'Refund: ' + inttostr(Sellrefund), false);
          // Die Strategie Buttons Anzeigen aber nur, wenn wir auch eine Strategie fahren können ;)
          If (TBuilding(FSideMenuObject).Stages[TBuilding(FSideMenuObject).Stage].range <> 0) Then Begin
            // Die 7 Strategien
            For i := 0 To 6 Do Begin
              BuildingStrategyButtons[i].Selected := BuildingStrategyButtons[i].Strategy = TBuilding(FSideMenuObject).strategy;
              BuildingStrategyButtons[i].Render();
            End;
            // Differenzierung Präveriert Luft Präferiert Boden
            BuildingStrategyButtons[7].Selected := TBuilding(FSideMenuObject).PreverAir;
            BuildingStrategyButtons[7].Render();
          End;
          If (TBuilding(FSideMenuObject).Stage < high(TBuilding(FSideMenuObject).Stages)) Then Begin
            // Das Gebäude kann noch geupt werden
            glPushMatrix;
            glTranslatef(0, 0, ctd_Menu_Layer);
            Level_Up_Image.Render();
            glPopMatrix;
            Level_Up_Image.Hint := 'Cost: ' + inttostr(TBuilding(FSideMenuObject).Stages[TBuilding(FSideMenuObject).Stage + 1].Cost);
            nshi := TBuilding(FSideMenuObject).GetNextStageHint();
            nshi.Stage := -2;
            nshi.Name := Level_Up_Image.Hint;
            hi.Name := nshi.Description + LineEnding + fDashSeparator + LineEnding;
            nshi.Description := '';
            RenderHint(fDestL - 10, fDestT + 30, nshi, false);
            RenderHint(fDestL - 10, fDestT + 30 + round(2 * OpenGL_ASCII_Font.TextHeight('8') * fontscale), hi, false);
          End
          Else Begin
            // Das Gebäude ist maximal ausgebaut
            hi.Name := LineEnding + hi.Name;
            RenderHint(fDestL - 10, fDestT + 30, hi, false);
          End;
        End
        Else Begin
          // Das Gebäude wird gerade "geupdated" -> da geht nix nur ne kurze Info anzeigen
          hi.Name := 'Refund: ' + inttostr(TBuilding(FSideMenuObject).CalcSellRefund(fMap.Difficulty));
          hi.Stage := 0;
          RenderHint(fDestL - 10, fDestT + 30, hi, false);
        End;
      End
      Else Begin
        // Das Gebäude gehört uns nicht, also zeigen wir an was es gerade hat.
        If (TBuilding(FSideMenuObject).Owner >= 0) And (TBuilding(FSideMenuObject).Owner <= high(fPlayerInfos)) Then Begin
          // Wenn das Geb kein Placement ist, dann zeigen wir an wem es gehört.
          hi.Name := 'Owner: ' + fPlayerInfos[TBuilding(FSideMenuObject).Owner].Name + LineEnding + LineEnding +
            'Cost: ' + inttostr(TBuilding(FSideMenuObject).CalculateCost) + LineEnding + hi.Name;
        End;
        RenderHint(fDestL - 10, fDestT + 30, hi, false);
      End;
    End;
    If (fSideMenuObject Is THero) Then Begin
      // Anzeigen der Ranges der Helden
      If HintShowHeroRange Then Begin
        For i := 0 To high(fSelectedHeros) Do Begin
          glPushMatrix;
          glTranslatef(fSelectedHeros[i].Position.x * MapBlockSize - fsx + fMapL, fSelectedHeros[i].Position.y * MapBlockSize - fsy + fMapT, ctd_MapBlackOutLayer - ctd_EPSILON);
          fSelectedHeros[i].RenderRange();
          glPopMatrix;
        End;
      End;
      // Alle nachfolgenden Element sind Menü Elemente und müssen in das MenüLayer gerendert werden
      Sell_Image.Hint := '';
      Level_Up_Image.Hint := '';
      hi := FSideMenuObject.GetHint();
      // Das Gebäude gehört uns, also Zeigen wir an was es als nächstes Gibt
      If (THero(FSideMenuObject).Owner = fPlayerIndex) Then Begin
        // Das Gebäude gehört uns
        If (THero(FSideMenuObject).Level >= 0) Then Begin // Nicht in Update Mode
          // Die 7 Strategien
          For i := 0 To 6 Do Begin
            BuildingStrategyButtons[i].Selected := BuildingStrategyButtons[i].Strategy = THero(FSideMenuObject).strategy;
            BuildingStrategyButtons[i].Render();
          End;
          // Differenzierung Präveriert Luft Präferiert Boden
          BuildingStrategyButtons[7].Selected := THero(FSideMenuObject).PreverAir;
          BuildingStrategyButtons[7].Render();

          If (THero(FSideMenuObject).Level < high(THero(FSideMenuObject).Levels)) Then Begin
            // Das Gebäude kann noch geupt werden
            nshi := THero(FSideMenuObject).GetNextLevelHint();
            nshi.Stage := -2;
            nshi.Name := Level_Up_Image.Hint;
            hi.Name := nshi.Description + LineEnding + fDashSeparator + LineEnding;
            nshi.Description := '';
            RenderHint(fDestL - 10, fDestT + 30, nshi, false);
            RenderHint(fDestL - 10, fDestT + 30 + round(2 * OpenGL_ASCII_Font.TextHeight('8') * fontscale), hi, false);
          End
          Else Begin
            // Das Gebäude ist maximal ausgebaut
            hi.Name := LineEnding + hi.Name;
            RenderHint(fDestL - 10, fDestT + 30, hi, false);
          End;
        End
        Else Begin
          // Das Gebäude wird gerade "geupdated" -> da geht nix nur ne kurze Info anzeigen
          hi.Name := 'Building...';
          hi.Stage := 0;
          RenderHint(fDestL - 10, fDestT + 30, hi, false);
        End;
      End
      Else Begin
        // Das Gebäude gehört uns nicht, also zeigen wir an was es gerade hat.
        If (THero(FSideMenuObject).Owner >= 0) And (THero(FSideMenuObject).Owner <= high(fPlayerInfos)) Then Begin
          // Wenn das Geb kein Placement ist, dann zeigen wir an wem es gehört.
          hi.Name := 'Owner: ' + fPlayerInfos[THero(FSideMenuObject).Owner].Name + LineEnding + LineEnding +
            'Cost: ' + inttostr(THero(FSideMenuObject).Cost) + LineEnding + hi.Name;
        End;
        RenderHint(fDestL - 10, fDestT + 30, hi, false);
      End;
    End;
  End;
End;

Procedure Tctd.UpdatePausing(Value: Boolean);
Begin
  (*
   * Hier müssen alle Programmteile gelistet werden, welche zeitgesteuerte Prozesse haben, diese müssen entsprechend
   * Informiert werden.
   *)
  fpausing := Value;
  If assigned(fMap) Then
    fmap.pause(fpausing);
  fSplashhints.Pause(fpausing);
End;

Procedure Tctd.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Var
  i, j, k: integer;
  s, t: String;
  crc: UInt32;
  m: TMemoryStream;
  r, g, b: Single;
  bool: Boolean;
  ts: int64;
  cbSE: TOnGetStreamEvent;
  cbSLE: TOnGetStringListEvent;
  cbMPIE: TOnGetMapPrieviewInfoEvent;
  cbBE: TOnGetBooleanEvent;
  MPI: TMapPrieviewInfo;
  sl: TStringlist;
  br: TOnGetBooleanEventRecord;
  f: TFileStream;
Begin
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miUpdateMoveables) And
    ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    log('Tctd.HandleReceivedData : ' + MessageIdentifierToString(Chunk.UserDefinedID), llTrace);
  Case (Chunk.UserDefinedID And $FFFF) Of
    miCleanupUnusedOpponents: Begin
        If assigned(fMap) Then Begin
          fMap.DeleteUnusedOpponents;
        End;
      End;
    miYouWereKickedOut: Begin
        (*
         * Wenn der Spieler aus dem Spiel geworfen wurde, dann sollte er das auch mit bekommen ...
         *)
        Connection_Disconnect(Nil);
      End;
    miDelBuilding: Begin
        s := Chunk.Data.ReadAnsiString;
        If assigned(fMap) Then fMap.delBuilding(s);
      End;
    miDelOpponent: Begin
        s := Chunk.Data.ReadAnsiString;
        If assigned(fMap) Then fMap.delOpponent(s);
      End;
    miRequestMapHighscoresAndRatingResult: Begin
        If (fgameState = gs_EditMode) And assigned(OnGetHighscores) Then Begin
          OnGetHighscores(self, chunk.Data);
        End;
      End;
    miCloneMapWave: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := -1;
        chunk.Data.Read(j, sizeof(j));
        DoWaveClone(i, j);
      End;
    miCreateSplashMark: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := -1;
        chunk.Data.Read(j, sizeof(j));
        fSplashMarks.addMark(i, j);
      End;
    miSetSpeedUp: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        If i <> -1 Then Begin
          Speedup := i;
          s := format('Speed is now %dx', [Speedup]);
          Splashhint(s, DefaultSplashHintColor);
        End;
      End;
    miLoadGamingdata: Begin
        If high(FReceivingQueue) <> -1 Then Begin
          FOnReceivingFilesFinish.NeedGamingData := true;
          FOnReceivingFilesFinish.Data := TMemoryStream.Create;
          FOnReceivingFilesFinish.Data.CopyFrom(chunk.data, Chunk.data.Size - Chunk.data.Position);
          FOnReceivingFilesFinish.Data.Position := 0;
        End
        Else Begin
          HandleLoadGamingData(Chunk.Data);
        End;
      End;
    miForceEditMode: Begin
        SwitchToEditMode();
      End;
    miEndRound: Begin
        bool := false;
        Chunk.Data.Read(bool, sizeof(bool));
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        HandleOnEndRound(Bool, i, Chunk.data);
      End;
    miSetBuildingsToStage: Begin
        HandleOnBuildingsToStage(chunk.Data);
      End;
    miSetHerosToLevel: Begin
        HandleOnHerosToLevel(chunk.Data);
      End;
    miPing: Begin
        SendChunk(miPing, Nil);
      End;
    miHeartBeat: Begin
        ts := 0;
        Chunk.Data.Read(ts, sizeof(ts));
        m := TMemoryStream.Create;
        m.Write(fPlayerIndex, sizeof(fPlayerIndex));
        m.write(ts, sizeof(ts));
        SendChunk(miHeartBeat, m);
      End;
    miLevelUpBuilding: Begin
        // Wenn das Gebäude direkt zum Runden Neustart geuppt wird, dann kann es sein
        // das fGamestate gerade auf Start steht und nicht auf gaming, wenn dem so ist
        // Verschluckt der Client die Anzeige, der Server korrigiert dies zwar beim
        // näcshten Runden neu start, aber das würde der User nicht aktzeptieren.
        // Also wird hier ein anderes Entscheidungskriterium herangezogen...
        // If fgameState = gs_Gaming Then Begin
        If assigned(fmap) Then Begin
          HandleLevelUpBuilding(Chunk.data);
        End;
      End;
    miRemoveBuilding: Begin
        HandleRemoveBuilding(Chunk.data);
      End;
    miAddBuilding: Begin
        s := Chunk.Data.ReadAnsiString;
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := -1;
        chunk.Data.Read(j, sizeof(j));
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        AddBuilding(i, j, s, k);
      End;
    miAddHero: Begin
        s := Chunk.Data.ReadAnsiString;
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := -1;
        chunk.Data.Read(j, sizeof(j));
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        AddHero(i, j, s, k);
      End;
    miChatMessage: Begin
        s := Chunk.Data.ReadAnsiString;
        s := DeSerialize(s);
        form13.AddMessage(s);
        Splashhint(s, DefaultSplashHintColor);
      End;
    miTogglePause: Begin
        bool := false;
        chunk.data.Read(bool, sizeof(bool));
        UpdatePausing(bool);
      End;
    miUpdateMoveables: Begin
        If fgameState = gs_Gaming Then Begin
          fmap.UpdateMoveables(Chunk.data);
        End;
      End;
    miStart: Begin
        fSplashhints.AddText('Starting round.');
        fmap.CreateMovableObjectList(fAktualWave); // Eigentlich sollte dieser Schritt nicht mehr nötig sein, da das ja schon gemacht wurde.
        fgameState := gs_Gaming;
        fShowBuildableTiles := false;
        Resize; // Wenn das Seitenmenü ausgeblendet ist muss der "<" Knopf in der Höhe angepasst werden
        FHintObject.Obj := Nil;
      End;
    miRefreshPlayerStats: Begin
        HandleRefresPlayerStats(Chunk.data);
      End;
    miStartRound: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := -1;
        chunk.Data.Read(j, sizeof(j));
        HandleStartRound(i, j);
      End;
    miSplashHint: Begin
        s := Chunk.Data.ReadAnsiString;
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        r := 0;
        g := 0;
        B := 0;
        chunk.Data.Read(r, sizeof(r));
        chunk.Data.Read(g, sizeof(g));
        chunk.Data.Read(b, sizeof(b));
        s := DeSerialize(s);
        fSplashhints.AddText(s, i, v3(r, g, b));
        form13.AddMessage(s); // Spiegeln in den Chat, falls das jemand nachlesen will ?
      End;
    miUpdateMapProperty: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := -1;
        chunk.Data.Read(j, sizeof(j));
        HandleUpdateMapProperty(i, j, chunk.Data);
      End;
    miFileTransfer: Begin
        s := Chunk.Data.ReadAnsiString;
        t := Chunk.Data.ReadAnsiString;
        m := TMemoryStream.Create;
        m.CopyFrom(Chunk.Data, Chunk.Data.Size - Chunk.Data.Position);
        m.position := 0;
        HandleFileReceifed(s, t, m);
        m.free;
      End;
    miLoadMap: Begin
        s := Chunk.Data.ReadAnsiString;
        If high(FReceivingQueue) <> -1 Then Begin
          FOnReceivingFilesFinish.Need := true;
          FOnReceivingFilesFinish.MapName := s;
        End
        Else Begin
          LoadMapFromShare(s);
        End;
      End;
    miRequestFileTransfer: Begin
        s := Chunk.Data.ReadAnsiString;
        t := Chunk.Data.ReadAnsiString;
        crc := 0;
        Chunk.Data.Read(crc, sizeof(crc));
        HandleRequestFileTransfer(s, t, crc);
      End;
    miRequestSavegamesResult: Begin
        m := TMemorystream.create;
        m.CopyFrom(Chunk.Data, chunk.Data.Size);
        m.Position := 0;
        cbSE := fsh.GetIdentifierStreamEventCallback(Chunk.UserDefinedID And $FFFF0000);
        If assigned(cbSE) Then Begin // Der ist immer Assigned !
          cbSE(Self, m);
        End;
        m.free;
      End;
    miRequestMapListResult,
      miRequestFileListResult: Begin
        m := TMemorystream.create;
        m.CopyFrom(Chunk.Data, chunk.Data.Size);
        m.Position := 0;
        i := 0;
        sl := TStringlist.Create;
        m.Read(i, sizeof(i));
        For j := 0 To i - 1 Do Begin
          sl.Add(m.ReadAnsiString);
        End;
        m.free;
        cbSLE := fsh.GetIdentifierStringListEventCallback(Chunk.UserDefinedID And $FFFF0000);
        If assigned(cbSLE) Then Begin // Der ist immer Assigned !
          cbSLE(Self, sl);
        End;
        sl.free;
      End;
    miRequestMapPreviewInfoResult: Begin
        m := TMemorystream.create;
        m.CopyFrom(Chunk.Data, chunk.Data.Size);
        m.Position := 0;
        MPI.Description := m.ReadAnsiString;
        MPI.Image := TBitmap.Create;
        MPI.Image.LoadFromStream(m, m.Size - m.Position);
        m.free;
        cbMPIE := fsh.GetIdentifierMapPrieviewInfoEventCallback(Chunk.UserDefinedID And $FFFF0000);
        If assigned(cbMPIE) Then Begin // Der ist immer Assigned !
          cbMPIE(self, mpi);
        End;
        mpi.Image.Free;
      End;
    miStartFileTransfer: Begin
        m := TMemorystream.create;
        m.CopyFrom(Chunk.Data, chunk.Data.Size);
        m.Position := 0;
        br := fSH.GetIdentifierBooleanEventCallback(Chunk.UserDefinedID And $FFFF0000);
        cbBE := br.Callback;
        bool := true;
        m.Read(bool, sizeof(bool));
        m.free;
        If bool Then Begin
          // Der Server will Die Datei
          f := TFileStream.Create(br.s, fmOpenRead);
          m := TMemoryStream.Create;
          m.WriteAnsiString(MapName);
          m.WriteAnsiString(br.d);
          m.CopyFrom(f, f.Size);
          f.free;
          SendChunk(miFileTransfer, m);
        End
        Else Begin
          // Der Server hat die Datei schon, dann ist eh alles I.O. :-)
        End;
        cbBE(self, true);
      End;
    miFilesToTransmitCount: Begin
        m := TMemorystream.create;
        m.CopyFrom(Chunk.Data, chunk.Data.Size);
        m.Position := 0;
        i := 0;
        m.Read(i, sizeof(i));
        fFileReceivingGoalCount := i;
        fFileReceivingCount := 0;
        m.free;
      End;
    miRequestLoginResult: Begin
        i := -1;
        Chunk.Data.Read(i, sizeof(i));
        If i = EC_No_Error Then Begin
          // Wir dürfen mit machen, Also fragen wir erst einmal die Aktuelle Karte an
          i := -1;
          Chunk.Data.Read(i, sizeof(i));
          HandleOnConnectToServer(i);
          SendChunk(miRequestMap, Nil);
        End
        Else Begin
          // Wir dürfen nicht mit spielen, also alles wieder zurück auf Anfang
          s := 'Unknown error.';
          Case i Of
            EC_Invalid_Password: s := 'Invalid password.';
            EC_User_already_exists: s := 'A player with this username already joined the server.';
            EC_game_full: s := 'Game already in progress. No additional connections are allowed.';
            EC_Invalid_Versions: s := 'Server and client have different versions. Please use the same version as server!';
          End;
          fChunkManager.Disconnect(true);
          HandleOnDisconnectFromServer;
          LogShow('Could not join the server : ' + s, llInfo);
        End;
      End;
  Else Begin
      log('Unknown user defined id : ' + inttostr((Chunk.UserDefinedID And $FFFF)), llError);
    End;
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miUpdateMoveables) And
    ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    LogLeave;
End;

Procedure Tctd.HandleStartRound(Round: integer; Difficulty: integer);
Var
  i: integer;
Begin
  log('Tctd.HandleStartRound', llTrace);
  log(format('Start round : %d on map %s', [round + 1, MapName]), llInfo);
  fgameState := gs_WaitToStart;
  fAktualWave := Round;
  // Bei einem Komplett neuen Spiel resetten wir auch alles !
  If Round = 0 Then Begin
    FHintObject.Obj := Nil;
    fSideMenuObject := Nil;
    fBuyingObject := Nil;
    fmap.ResetAllBuyedObjects;
    fmap.CalcWaypointFields;
    fmap.Difficulty := Difficulty;
    fStartRoundTick := GetTick();
    setlength(fSelectedBuildings, 0);
  End;
  If FHintObject.Obj Is TOpponent Then Begin
    FHintObject.Obj := Nil;
  End;
  //fSidemenuOpponent := Nil; -- Der Darf nicht auf Nil gesetzt werden -> das Setzen von fSideMenuObject = nil genügt bereits.
  If fSideMenuObject Is TOpponent Then Begin // Gegner werden abgewählt, Gebäude nicht ;)
    fSideMenuObject := Nil;
  End;
  fmap.ResetAllMovingObjects;
  fmap.CreateMovableObjectList(fAktualWave);
  fmap.ResetAllUpdateBuildings;
  fmap.CreateDamageClassTextures;
  For i := 0 To high(fPlayerInfos) Do Begin
    fPlayerInfos[i].KillsOnWaveStart := fPlayerInfos[i].Kills + fPlayerInfos[i].BonusFinisher;
    fPlayerInfos[i].LivesOnWaveStart := fPlayerInfos[i].Lives;
  End;
  //  CalcSumKillsLives(SumKills, SumLives);
  If fmap.MapType In [mtCoop, mtCoopMaze] Then Begin
    fOpponentsAtEndOfWave := fmap.OpponentsEmittedInWave(fAktualWave) * length(fPlayerInfos);
  End
  Else Begin
    fOpponentsAtEndOfWave := fmap.OpponentsEmittedInWave(fAktualWave);
  End;
  CreateBuildMenu(fAktualWave);
  UpdatePausing(false);
  If assigned(OnStartRound) Then Begin
    OnStartRound(self);
  End;
  SendChunk(miStartRoundresult, Nil);
  LogLeave;
End;

Procedure Tctd.HandleRefresPlayerStats(Const Data: TStream);
Var
  i, j: integer;
  uid, Cash, Lives, Kills, BonusFinisher: integer;
  s: String;
Begin
  log('Tctd.HandleRefresPlayerStats', llTrace);
  fPlayerIndex := -1; // Diese Initialisierung müsste eigentlich nur beim 1. Mal gemacht werden, läuft halt immer mit..
  // Suchen unseres Datensatzes und Übernehmen der Statistic Werte
  j := 0;
  data.Read(j, SizeOf(j));
  setlength(fPlayerInfos, j);
  For i := 0 To j - 1 Do Begin
    uid := -1;
    cash := 0;
    lives := 0;
    Kills := 0;
    BonusFinisher := 0;
    data.Read(uid, sizeof(uid));
    data.Read(Cash, sizeof(uid));
    data.Read(Lives, sizeof(uid));
    data.Read(Kills, sizeof(uid));
    data.Read(BonusFinisher, SizeOf(BonusFinisher));
    s := data.ReadAnsiString;
    fPlayerInfos[i].uid := uid;
    fPlayerInfos[i].Cash := Cash;
    fPlayerInfos[i].Lives := Lives;
    fPlayerInfos[i].Kills := Kills;
    fPlayerInfos[i].BonusFinisher := BonusFinisher;
    fPlayerInfos[i].Name := s;
    If uid = fServerUid Then Begin
      fPlayerIndex := i;
    End;
  End;
  If assigned(OnRefreshPlayerStats) Then
    OnRefreshPlayerStats(self);
  LogLeave;
End;

Procedure Tctd.HandleOnBuildingsToStage(Const List: TStream);
Var
  x, y, s, i, cnt: integer;
Begin
  log('Tctd.HandleOnBuildingsToStage', llTrace);
  If Not assigned(fMap) Then Begin
    LogLeave;
    exit;
  End;
  // Berechnen der Anzahl (Müsste = Anzahl der Gebäude sein)
  cnt := (list.Size - list.Position) Div (3 * SizeOf(x));
  x := -1;
  y := -1;
  s := -1;
  For i := 0 To cnt - 1 Do Begin
    List.Read(x, sizeof(x));
    List.Read(y, sizeof(y));
    List.Read(s, sizeof(s));
    fmap.SetBuildingStage(x, y, s);
  End;
  LogLeave;
End;

Procedure Tctd.HandleOnHerosToLevel(Const List: TStream);
Var
  cnt: integer;
  x, i, j: integer;
Begin
  log('Tctd.HandleOnHerosToLevel', llTrace);
  If Not assigned(fMap) Then Begin
    LogLeave;
    exit;
  End;
  cnt := (list.Size - list.Position) Div (2 * SizeOf(x));
  For i := 0 To cnt - 1 Do Begin
    j := -1;
    x := -1;
    List.Read(j, sizeof(j));
    List.Read(x, sizeof(x));
    fmap.SetHeroToLevel(j, x);
  End;
  LogLeave;
End;

Procedure Tctd.SendChunk(UserDefinedID: Integer; Const Data: TStream);
Begin
{$IFDEF DoNotLog_CyclicMessages}
  If ((UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    log('Tctd.SendChunk : ' + MessageIdentifierToString(UserDefinedID), llTrace);
  If Not fChunkManager.SendChunk(UserDefinedID, data) Then Begin
    log('Could not send.', llCritical);
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If ((UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    LogLeave;
End;

Procedure Tctd.HandleRequestFileTransfer(MapName, Filename: String; CRC: UInt32
  );
Var
  s: String;
  b: Boolean;
  CRC2: Uint32;
  m: TMemoryStream;
Begin
  log(format('Tctd.HandleRequestFileTransfer : %s, %s', [MapName, Filename]), lltrace);
  If fgameState <> gs_Loading Then Begin
    fstatebeforeloading := fgameState;
    fgameState := gs_Loading;
  End;
  // Egal wie der Counter Steht, er muss mindestens 1 mehr haben wie fFileReceivingCount weil ja nun eine Anfrage rein kam
  fFileReceivingGoalCount := max(fFileReceivingGoalCount, fFileReceivingCount + 1);
  s := MapFolder + MapName + PathDelim + Filename;
  b := false;
  If FileExistsutf8(s) Then Begin
    crc2 := CRC_of_File(s);
    b := crc2 <> crc; // Der CRC stimmt nicht..
  End
  Else Begin
    b := true; // Die Datei existiert gar nicht..
  End;
  If b Then Begin
    m := TMemoryStream.Create;
    m.WriteAnsiString(MapName);
    m.WriteAnsiString(Filename);
    setlength(FReceivingQueue, high(FReceivingQueue) + 2);
    FReceivingQueue[high(FReceivingQueue)].MapName := MapName;
    FReceivingQueue[high(FReceivingQueue)].Filename := Filename;
    SendChunk(miStartFileTransfer, m);
  End
  Else Begin
    // Bin nicht sicher, ob das hier je Kommen kann, nen schaden richtet es
    // durch den Check auf .need aber auch nicht an, also bleibts mal drin.
    OnEndFiletransfer();
  End;
  logleave;
End;

Procedure Tctd.HandleFileReceifed(MapName, Filename: String; Const Data: TStream
  );
Var
  s: String;
  f: TFileStream;
  i, j: Integer;
Begin
  log(format('Tctd.HandleFileReceifed : %s, %s', [MapName, Filename]), lltrace);
  s := MapFolder + MapName;
  If Not DirectoryExistsUTF8(s) Then Begin
    If Not CreateDirUTF8(s) Then Begin
      log('Could not create directory : ' + s, llFatal);
      LogLeave;
      exit;
    End;
  End;
  f := TFileStream.Create(s + PathDelim + Filename, fmOpenWrite Or fmCreate);
  f.CopyFrom(data, data.size);
  f.free;
  // Datei wieder aus der Warteschlange Löschen
  For i := 0 To high(FReceivingQueue) Do Begin
    If (FReceivingQueue[i].MapName = MapName) And (FReceivingQueue[i].Filename = Filename) Then Begin
      For j := i To high(FReceivingQueue) - 1 Do
        FReceivingQueue[j] := FReceivingQueue[j + 1];
      setlength(FReceivingQueue, high(FReceivingQueue));
      break;
    End;
  End;
  // Warteschlange ist leer, soll die Karte geladen werden ?
  OnEndFiletransfer();
  logleave;
End;

Procedure Tctd.HandleOnConnectToServer(ServerUid: integer);
Begin
  log('Tctd.HandleOnConnectToServer', lltrace);
  (*
   * Hier können alle möglichen Variablen Initialisiert werden, welche auf jeden Fall bei
   * Einer Serververbindung initialisiert sein sollten
   *)
  SwitchToEditMode();
  ShowPlayerStartNames := false;
  fServerUid := ServerUid;
  fPausing := false;
  If assigned(Fmap) Then fmap.free;
  fmap := Nil;
  MapName := '';
  setlength(FReceivingQueue, 0);
  BlockMapUpdateSending := false;
  FOnReceivingFilesFinish.Need := false;
  FOnReceivingFilesFinish.NeedGamingData := false;
  FOnReceivingFilesFinish.NeedRefreshBackTex := false;
  FOnReceivingFilesFinish.NeedRefreshDC1 := false;
  FOnReceivingFilesFinish.NeedRefreshDC2 := false;
  FOnReceivingFilesFinish.NeedRefreshDC3 := false;
  FOnReceivingFilesFinish.NeedRefreshDC4 := false;
  If assigned(OnConnectToServer) Then Begin
    OnConnectToServer(self);
  End;
  logleave;
End;

Constructor Tctd.create;
Begin
  log('Tctd.create', llTrace);
  Inherited create();
  fLeftMousePressed := false;
  VersionInfoString := '';
  ShowBuildableTilesDuringBuild := false;
  OnGetHighscores := Nil;
  fFileReceivingCount := 0;
  fShowBuildableTiles := false;
  fSH := TCallbackHolder.create;
  fOpenGames := Nil;
  fwold := -1;
  fhold := -1;
  InvertMouseScrolling := false;
  fChunkManager := TChunkManager.create;
  fSplashMarks := TSplashMarkManager.Create;
  fgameState := gs_WaitForJoin;
  OnConnectToServer := Nil;
  OnDisconnectFromServer := Nil;
  OnWaveCloneEvent := Nil;
  ShowGrid := false;
  HintShowBuildingRange := true;
  HintShowHeroRange := true;
  FBuyMenu.Items := Nil;
  fSelectedBuildings := Nil;
  fSelectedHeros := Nil;
  BlockMapUpdateSending := false;
  fSidemenuOpponent := TOpponent.create();
  fMenuPosition := mpBottom;
  fSidemenuVisible := true;
  LogLeave;
End;

Destructor Tctd.Destroy;
Var
  i: Integer;
Begin
  log('Tctd.Destroy', llTrace);
  fSH.free;
  setlength(fOpenGames, 0);
  If assigned(fCaptured_OpenGLControl) Then Begin
    fCaptured_OpenGLControl.OnMousemove := Nil; //FOnMouseMoveCapture;
    fCaptured_OpenGLControl.OnMouseDown := Nil; //FOnMouseDownCapture;
    fCaptured_OpenGLControl.OnMouseup := Nil; //FOnMouseUpCapture;
    fCaptured_OpenGLControl.OnDblClick := Nil; //FOnDblClickCapture;
    fCaptured_OpenGLControl.OnKeyDown := Nil;
    fCaptured_OpenGLControl.OnKeyUp := Nil;
  End;
  fSidemenuOpponent.free;
  fChunkManager.free;
  fSplashMarks.free;
  setlength(FReceivingQueue, 0);
  For i := 0 To high(BuildingStrategyButtons) Do Begin
    BuildingStrategyButtons[i].free;
    BuildingStrategyButtons[i] := Nil;
  End;
  Level_Up_Image.free;
  Level_Up_Image := Nil;
  Sell_Image.free;
  Sell_Image := Nil;
  Tab_Image.free;
  Tab_Image := Nil;
  fCaptured_OpenGLControl := Nil;
  fSplashhints.free;
  DeselectCursor;
  If Assigned(fMap) Then fmap.free;
  For i := 0 To high(FBuyMenu.Items) Do Begin
    FBuyMenu.Items[i].obj.Free;
  End;
  setlength(FBuyMenu.Items, 0);
  fWaveinfo.Free;
  fTargetsInfo.free;
  fLivesInfo.Free;
  fCoinsInfo.Free;
  fHostButton.free;
  fJoinButton.free;
  fLoadGameButton.free;
  fLoadMapButton.free;
  fNewMapButton.free;
  LogLeave;
End;

Procedure Tctd.Initialize(Const Owner: TOpenGLControl);
Var
  p: String;
  i: integer;
  png: TPortableNetworkGraphic;
  b1, b2: TBitmap;
Begin
  log('Tctd.Initialize', llTrace);
  fSplashhints := TSplashHintManager.create;
  fCaptured_OpenGLControl := Owner;
  FOnMouseMoveCapture := Owner.OnMousemove;
  Owner.OnMousemove := @FOnMouseMove;
  FOnMouseDownCapture := Owner.OnMouseDown;
  Owner.OnMouseDown := @FOnMouseDown;
  FOnMouseupCapture := Owner.OnMouseup;
  Owner.OnMouseup := @FOnMouseup;
  FOnDblClickCapture := Owner.OnDblClick;
  Owner.OnDblClick := @FOnDblClick;
  FOnKeyDownCapture := Owner.OnKeyDown;
  Owner.OnKeyDown := @FOnKeyDown;
  FOnKeyUpCapture := Owner.OnKeyUp;
  Owner.OnKeyUp := @FOnKeyUp;
  fHostButton := TOpenGl_Button.Create(Owner);
  fHostButton.caption := 'H&ost game';
  fJoinButton := TOpenGl_Button.Create(Owner);
  fJoinButton.caption := '&Join game';
  fNewMapButton := TOpenGl_Button.Create(Owner);
  fNewMapButton.caption := '&New Map';
  fLoadMapButton := TOpenGl_Button.Create(Owner);
  fLoadMapButton.caption := '&Load Map';
  fLoadGameButton := TOpenGl_Button.Create(Owner);
  fLoadGameButton.caption := 'L&oad game';

  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'textures' + PathDelim;
  fHostButton.LoadTextures(p + 'BTN_host_game_n.png', p + 'BTN_host_game_s.png', p + 'BTN_host_game_s.png');
  fJoinButton.LoadTextures(p + 'BTN_join_game_n.png', p + 'BTN_join_game_s.png', p + 'BTN_join_game_s.png');
  fNewMapButton.LoadTextures(p + 'BTN_new_map_n.png', p + 'BTN_new_map_s.png', p + 'BTN_new_map_s.png');
  fLoadMapButton.LoadTextures(p + 'BTN_load_map_n.png', p + 'BTN_load_map_s.png', p + 'BTN_load_map_s.png');
  fLoadGameButton.LoadTextures(p + 'BTN_load_game_n.png', p + 'BTN_load_game_s.png', p + 'BTN_load_game_s.png');

  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + PlayerStartPointTex, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + PlayerStartPointTex, llError);
  fBuyKachel := OpenGL_GraphikEngine.LoadAlphaGraphikItem(P + BuyKachel, smClamp);
  AssertLog(fBuyKachel.Image = 0, 'Could not load : ' + p + BuyKachel, llError);
  BuildBuildingTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(p + BuildBuilding, smClamp);
  AssertLog(BuildBuildingTex.Image = 0, 'Could not load : ' + p + BuildBuilding, llError);
  fSelectTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(p + SelectTex, smClamp);
  AssertLog(fSelectTex.Image = 0, 'Could not load : ' + p + SelectTex, llError);

  If FileExistsUTF8(p + SplashMarkTex) Then Begin
    // das Ausrufezeichen ist besonders, weil es Semitransparenzen hat, deswegen muss es als Maske erzeugt werden ;)
    png := TPortableNetworkGraphic.Create;
    png.LoadFromFile(p + SplashMarkTex);
    b1 := TBitmap.Create;
    b1.Assign(png);
    png.free;
    b2 := TBitmap.Create;
    b2.Width := b1.Width;
    b2.Height := b1.Height;
    b2.Canvas.Brush.Color := clWhite;
    b2.Canvas.Rectangle(-1, -1, b2.Width + 1, b2.Height + 1);
    fSplashMarks.Texture := OpenGL_GraphikEngine.LoadAlphaGraphik(b2, b1, 'SplashMarkTex', smStretchHard);
    b2.free;
    b1.free;
  End
  Else Begin
    Log('Could not load : ' + p + SplashMarkTex, llError);
  End;

  fTargetsInfo := TCTDinfofield.Create(Owner, p + 'targets.png');
  fCoinsInfo := TCTDinfofield.Create(Owner, p + 'coins.png');
  fLivesInfo := TCTDinfofield.Create(Owner, p + 'hard.png');
  fWaveinfo := TCTDDualinfoField.Create(Owner, p + 'wave.png');

  Level_Up_Image := TOpenGl_HintImage.create(Owner);
  Level_Up_Image.Hint := '';
  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + Levelup, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + Levelup, llError);
  Level_Up_Image.SetImage(i);
  Level_Up_Image.OnClick := @OnLevelUpButtonClick;
  Level_Up_Image.OnMouseMove := @OnUpgradeSellMouseMove;
  Sell_Image := TOpenGl_HintImage.create(Owner);
  Sell_Image.Hint := '';
  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + Sell, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + Sell, llError);
  sell_Image.SetImage(i);
  sell_Image.OnClick := @OnSellButtonClick;
  sell_Image.OnMouseMove := @OnUpgradeSellMouseMove;

  Tab_Image := TOpenGl_Image.create(Owner);
  // Das ist absicht, dass das 4 mal gemacht wird -> dadurch wird die Textur schon mal gepuffert
  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabLeft, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + TabLeft, llError);
  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabRight, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + TabRight, llError);
  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabUp, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + TabUp, llError);
  i := OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabDown, smStretchHard);
  AssertLog(i = 0, 'Could not load : ' + p + TabDown, llError);
  Tab_Image.SetImage(i);
  Tab_Image.Transparent := true;
  Tab_Image.OnClick := @OnTabButtonClick;

  For i := 0 To 6 Do Begin
    BuildingStrategyButtons[i] := TBuildingStrategyButton.create(Owner);
    BuildingStrategyButtons[i].OnClick := @OnStrategyButtonClick;
    BuildingStrategyButtons[i].OnMouseMove := @OnStrategyButtonMouseMove;
  End;
  BuildingStrategyButtons[7] := TBuildingStrategyButton.create(Owner);
  BuildingStrategyButtons[7].OnClick := @OnAirStrategyButtonClick;
  BuildingStrategyButtons[7].OnMouseMove := @OnStrategyButtonMouseMove;

  BuildingStrategyButtons[0].SetImage(p + 'first.png');
  BuildingStrategyButtons[0].Strategy := bsFirst;
  BuildingStrategyButtons[0].Name := 'Attack first creep [F1]';

  BuildingStrategyButtons[1].SetImage(p + 'last.png');
  BuildingStrategyButtons[1].Strategy := bsLast;
  BuildingStrategyButtons[1].Name := 'Attack last creep [F2]';

  BuildingStrategyButtons[2].SetImage(p + 'mindistance.png');
  BuildingStrategyButtons[2].Strategy := bsNearest;
  BuildingStrategyButtons[2].Name := 'Attack nearest creep first [F3]';

  BuildingStrategyButtons[3].SetImage(p + 'maxdistance.png');
  BuildingStrategyButtons[3].Strategy := bsFarest;
  BuildingStrategyButtons[3].Name := 'Attack farthest creep first [F4]';

  BuildingStrategyButtons[4].SetImage(p + 'strongest.png');
  BuildingStrategyButtons[4].Strategy := bsStrongest;
  BuildingStrategyButtons[4].Name := 'Attack strongest creep first [F5]';

  BuildingStrategyButtons[5].SetImage(p + 'weakest.png');
  BuildingStrategyButtons[5].Strategy := bsWeakest;
  BuildingStrategyButtons[5].Name := 'Attack weakest creep first [F6]';

  BuildingStrategyButtons[6].SetImage(p + 'random.png');
  BuildingStrategyButtons[6].Strategy := bsRandom;
  BuildingStrategyButtons[6].Name := 'Attack a random creep [F7]';

  BuildingStrategyButtons[7].SetImage(p + 'prefer_air.png');
  BuildingStrategyButtons[7].Strategy := bsWeakest; // Das macht doch gar keinen Sinn ?
  BuildingStrategyButtons[7].Name := 'Prefer air/land creeps [F8]';

  LogLeave;
End;

Procedure Tctd.RegisterTCPConnection(Const Connection: TLTCPComponent);
Begin
  log('Tctd.RegisterTCPConnection', llTrace);
  Connection.OnConnect := @Connection_Connect;
  Connection.OnDisconnect := @Connection_Disconnect;
  Connection.OnError := @Connection_Error;
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;
  fChunkManager.RegisterConnection(Connection);
  LogLeave;
End;

Procedure Tctd.RegisterUDPConnection(Const Connection: TLUDPComponent);
Begin
  log('Tctd.RegisterUDPConnection', llTrace);
  fUDPConnection := Connection;
  If fUDPConnection.Connected Then fUDPConnection.Disconnect();
  fUDPConnection.OnReceive := @OnUDPConnection_Receive;
  If Not fUDPConnection.Connect('', UDPPingPort) Then Begin
    LogShow('Error, could not start UDP Server.', llError);
  End;
  LogLeave;
End;

Function Tctd.Join(ip: String; Port: integer; Password, Username: String
  ): Boolean;
Begin
  log('Tctd.Join', lltrace);
  log(format('Joining to %s on port %d as %s, password="%s"', [ip, port, username, Password]));
  result := false;
  // Trennen falls notwendig
  DisConnect;
  fPassword := Password;
  FuserName := username;
  result := fChunkManager.Connect(ip, port);
  logleave;
End;

Procedure Tctd.DisConnect;
Begin
  log('Tctd.DisConnect', lltrace);
  If fChunkManager.Connected Then Begin
    fChunkManager.Disconnect();
    While fChunkManager.Connected Do Begin
      fChunkManager.CallAction();
    End;
  End;
  logleave;
End;

Procedure Tctd.Render(w, h: integer);
Var
  x, y, i, j: integer;
  f: single;
  hi: THint;
  s: String;
Begin
  If (fwold <> w) Or (fhold <> h) Then Begin
    fwold := w;
    fhold := h;
    Resize;
  End;
  WidgetSetGo2d(w, h); // Ist Identisch zum Go2D von uOpenGL_Ascii_Font
  fCoinsInfo.Visible := GameState = gs_Gaming; // Das wäre evtl wo anders besser aufgehoben
  fTargetsInfo.Visible := GameState = gs_Gaming; // Das wäre evtl wo anders besser aufgehoben
  fLivesInfo.Visible := GameState = gs_Gaming; // Das wäre evtl wo anders besser aufgehoben
  fWaveinfo.Visible := GameState = gs_Gaming; // Das wäre evtl wo anders besser aufgehoben
  fHostButton.Visible := fgameState = gs_WaitForJoin; // Das wäre evtl wo anders besser aufgehoben
  fJoinButton.Visible := fgameState = gs_WaitForJoin; // Das wäre evtl wo anders besser aufgehoben
  fNewMapButton.Visible := (((fgameState = gs_WaitToStart) Or (fgameState = gs_EditMode)) And (Not assigned(fMap))); // Das wäre evtl wo anders besser aufgehoben
  fLoadMapButton.Visible := (((fgameState = gs_WaitToStart) Or (fgameState = gs_EditMode)) And (Not assigned(fMap))); // Das wäre evtl wo anders besser aufgehoben
  fLoadGameButton.Visible := (((fgameState = gs_WaitToStart) Or (fgameState = gs_EditMode)) And (Not assigned(fMap))); // Das wäre evtl wo anders besser aufgehoben

  OpenGL_SpriteEngine.Enabled := (fgameState = gs_Gaming) And (Not FPausing);
  Case fgameState Of
    gs_WaitForJoin: Begin
        OpenGL_ASCII_Font.Color := clgreen;
        fHostButton.Render();
        fJoinButton.Render();
        CenterTextOut(w, h, 'Please host or join a session.');
        If VersionInfoString <> '' Then Begin
          RenderToolTipp(w - 10, h - 10, VersionInfoString);
        End;
      End;
    gs_Gaming: Begin
        If form4.Visible Then form4.Close;
        fmap.ShowWaypoints := false;
        If DarkOtherBuildings Then Begin
          fmap.Render(fsx, fsy, fMapL, fMapT, ShowGrid, ShowLifepoints, fPlayerIndex); // -- Muss als erstes gemacht werden, da es "ungenaue" Ränder hat.
        End
        Else Begin
          fmap.Render(fsx, fsy, fMapL, fMapT, ShowGrid, ShowLifepoints, -1); // -- Muss als erstes gemacht werden, da es "ungenaue" Ränder hat.
        End;
        fSplashMarks.Render(fsx, fsy, fMapL, fMapT);
        RenderSelected(fsx, fsy, fMapL, fMapT);
        RenderBlackOutMapBorder;
        RenderBuyMenu();
        RenderMenuTabButton();

        If DarkOtherBuildings Then Begin
          fMap.RenderPreview(fPreviewL + 1, fPreviewT + 1, fPreviewW - 2, fPreviewH - 2, fsx, fsy, fMapW, fMapH - fMapT, fPlayerIndex);
        End
        Else Begin
          fMap.RenderPreview(fPreviewL + 1, fPreviewT + 1, fPreviewW - 2, fPreviewH - 2, fsx, fsy, fMapW, fMapH - fMapT, -1);
        End;
        fSplashMarks.RenderPreview(fPreviewL + 1, fPreviewT + 1, fPreviewW - 2, fPreviewH - 2, fmap.Width, fmap.Height);

        // Rendern des SideMenü
        If assigned(FSideMenuObject) Then Begin
          RenderSideMenu;
        End;
        If FPausing Then Begin
          OpenGL_ASCII_Font.Color := clWhite;
          CenterTextOut(w, h, 'The game has been paused.');
        End;
        // Anzeigen des zu kaufenden Objekts
        If assigned(FBuyingObject) Then Begin
          x := (fsx + fCursorPos.x - fMapL) Div MapBlockSize;
          y := (fsy + fCursorPos.y - fMapT) Div MapBlockSize;
          glPushMatrix;
          glTranslatef(x * MapBlockSize - fsx + fMapL, y * MapBlockSize - fsy + fMapT, ctd_Buy_Preview_Layer);
          FBuyingObject.Render(false);
          If fBuyingObject Is TBuilding Then Begin // Bei gebäuden zusätzlich noch den Radius und die überdeckten Kacheln
            TBuilding(FBuyingObject).RenderRange;
            glTranslatef(0, 0, ctd_EPSILON);
            glEnable(GL_BLEND);
            glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
            glBindTexture(GL_TEXTURE_2D, 0);
            For i := 0 To round(FBuyingObject.Width) - 1 Do Begin
              For j := 0 To round(FBuyingObject.Height) - 1 Do Begin
                If fMap.CoordIsBuildable2(x + i, y - j) Then Begin
                  glColor4f(0, 0.25, 0, 0.75); // Baubar
                End
                Else Begin
                  glColor4f(1, 0, 0, 0.5); // Nicht Baubar
                End;
                glBegin(GL_QUADS);
                glVertex2f(i * MapBlockSize, -j * MapBlockSize + MapBlockSize);
                glVertex2f((i + 1) * MapBlockSize, -j * MapBlockSize + MapBlockSize);
                glVertex2f((i + 1) * MapBlockSize, -(j + 1) * MapBlockSize + MapBlockSize);
                glVertex2f(i * MapBlockSize, -(j + 1) * MapBlockSize + MapBlockSize);
                glend;
              End;
            End;
            glDisable(GL_BLEND);
          End;
          glPopMatrix;
        End;
        If fShowBuildableTiles Or (ShowBuildableTilesDuringBuild And (assigned(fBuyingObject))) Then Begin
          For i := 0 To fMap.Width - 1 Do Begin
            For j := 0 To fMap.Height - 1 Do Begin
              If Not fMap.CoordIsBuildable2(i, j) Then Begin
                glPushMatrix;
                glTranslatef(i * MapBlockSize - fsx + fMapL, j * MapBlockSize - fsy + fMapT, ctd_Buy_Preview_Layer);
                glTranslatef(0, 0, ctd_EPSILON);
                glEnable(GL_BLEND);
                glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
                glBindTexture(GL_TEXTURE_2D, 0);
                glColor4f(1, 0, 0, 0.5); // Nicht Baubar
                glBegin(GL_QUADS);
                glVertex2f(0, MapBlockSize);
                glVertex2f((0 + 1) * MapBlockSize, -0 * MapBlockSize + MapBlockSize);
                glVertex2f((0 + 1) * MapBlockSize, -(0 + 1) * MapBlockSize + MapBlockSize);
                glVertex2f(0 * MapBlockSize, -(0 + 1) * MapBlockSize + MapBlockSize);
                glend;
                glDisable(GL_BLEND);
                glPopMatrix;
              End;
            End;
          End;
        End;
        RenderGameInfos;
        // Anzeigen Tooltip für Obj unter Mausposition
        If fBuyMenuToolTipp <> '' Then Begin
          RenderToolTipp(fCursorPos.x, fCursorPos.y, fBuyMenuToolTipp);
        End;
        If fStrategyToolTipp <> '' Then Begin
          RenderToolTipp(fStrategyToolTippPos.x, fStrategyToolTippPos.y, fStrategyToolTipp);
        End;
        If assigned(FHintObject.Obj) Then Begin
          If GetTick() - FHintObject.Time > DefaultShowToolTippTime Then Begin
            i := FHintObject.Obj.Owner;
            hi := FHintObject.Obj.getHint;
            If (i >= 0) And (i < length(fPlayerInfos)) Then Begin
              hi.Name := 'Owner: ' + fPlayerInfos[i].Name + LineEnding + LineEnding + hi.Name;
            End;
            RenderHint(fCursorPos.x, fCursorPos.y, hi);

            If FHintObject.Obj Is TBuilding Then Begin
              If HintShowBuildingRange Then Begin
                glPushMatrix;
                glTranslatef(FHintObject.Obj.Position.x * MapBlockSize - fsx + fMapL, FHintObject.Obj.Position.y * MapBlockSize - fsy + fMapT, ctd_MapBlackOutLayer - ctd_EPSILON);
                TBuilding(FHintObject.Obj).RenderRange();
                glPopMatrix;
              End;
            End
            Else Begin
              If FHintObject.Obj Is THero Then Begin
                If HintShowHeroRange Then Begin
                  glPushMatrix;
                  glTranslatef(FHintObject.Obj.Position.x * MapBlockSize - fsx + fMapL, FHintObject.Obj.Position.y * MapBlockSize - fsy + fMapT, ctd_MapBlackOutLayer - ctd_EPSILON);
                  THero(FHintObject.Obj).RenderRange();
                  glPopMatrix;
                End;
              End
              Else Begin
                // Nichts ?
              End;
            End;
          End;
        End;
        // Anzeigen der Spielernamen an den Spawnpunkten
        If ShowPlayerStartNames Or (GetTick() < fStartRoundTick + PlayerDisplayTimeOnStartGame) Then Begin
          glPushMatrix;
          glTranslatef(-fsx, -fsy, 0);
          For i := 0 To high(fPlayerInfos) Do Begin
            RenderToolTipp(
              map.Waypoints[i, 0].Point.x * MapBlockSize - integer(round(OpenGL_ASCII_Font.TextWidth(fPlayerInfos[i].Name) * fontscale / 2)) + fMapL, map.Waypoints[i, 0].Point.y * MapBlockSize + fMapT,
              fPlayerInfos[i].Name
              );
          End;
          glPopMatrix;
        End;
        If fMap.heroCount <> 0 Then Begin
          If fLeftMousePressed Then Begin
            glDisable(GL_DEPTH_TEST);
            glBindTexture(GL_TEXTURE_2D, 0);
            glcolor3f(1, 1, 1);
            glbegin(GL_LINE_LOOP);
            glvertex2f(min(fmDownPos.x, fCursorPos.X), min(fmDownPos.y, fCursorPos.y));
            glvertex2f(min(fmDownPos.x, fCursorPos.X), max(fmDownPos.y, fCursorPos.y));
            glvertex2f(max(fmDownPos.x, fCursorPos.X), max(fmDownPos.y, fCursorPos.y));
            glvertex2f(max(fmDownPos.x, fCursorPos.X), min(fmDownPos.y, fCursorPos.y));
            glend;
            glEnable(GL_DEPTH_TEST);
          End;
        End;
      End;
    gs_Loading: Begin
        OpenGL_ASCII_Font.Color := clYellow;
        CenterTextOut(w, h, 'Loading, please wait..');
        // Anzeige eines kleinen Fortschrittsbalken
        If (fFileReceivingGoalCount >= fFileReceivingCount) And (fFileReceivingGoalCount <> 0) Then Begin
          glBindTexture(GL_TEXTURE_2D, 0);
          f := 199 * fFileReceivingCount / fFileReceivingGoalCount;
          If f < 100 Then Begin
            glColor3f(1, 0.25, 0.25);
          End
          Else Begin
            If f < 175 Then Begin
              glColor3f(1, 1, 0.25);
            End
            Else Begin
              glColor3f(0.25, 1, 0.25);
            End;
          End;
          glBegin(GL_QUADS);
          glVertex2f(w Div 2 - 100, h Div 2 + 15);
          glVertex2f(w Div 2 - 100, h Div 2 + 15 + 24);
          glVertex2f(w Div 2 - 100 + f, h Div 2 + 15 + 24);
          glVertex2f(w Div 2 - 100 + f, h Div 2 + 15);
          glend();
          glColor3f(1, 0.5, 0.5);
          glBegin(GL_LINE_LOOP);
          glVertex2f(w Div 2 - 100, h Div 2 + 14);
          glVertex2f(w Div 2 - 100, h Div 2 + 15 + 25);
          glVertex2f(w Div 2 + 100, h Div 2 + 15 + 25);
          glVertex2f(w Div 2 + 100, h Div 2 + 15);
          glend();
        End;
        // Laden ist Fertig, wir wechseln mal in den Vorherigen State
        If (fFileReceivingGoalCount <= fFileReceivingCount) Then Begin
          fgameState := fstatebeforeloading;
        End;
      End;
    gs_WaitToStart,
      gs_EditMode: Begin
        If assigned(fMap) Then Begin
          // Anzeigen des zu kaufenden Objets
          fmap.ShowWaypoints := fgameState = gs_EditMode;
          If (fgameState = gs_EditMode) Or (Not DarkOtherBuildings) Then Begin
            fmap.Render(fsx, fsy, fMapL, fMapT, ShowGrid, false, -1); // -- Muss als erstes gemacht werden, da es "ungenaue" Ränder hat.
          End
          Else Begin
            fmap.Render(fsx, fsy, fMapL, fMapT, ShowGrid, false, fPlayerIndex); // -- Muss als erstes gemacht werden, da es "ungenaue" Ränder hat.
          End;
          fSplashMarks.Render(fsx, fsy, fMapL, fMapT);

          If form4.CheckBox2.Checked Then Begin // Cursor für Terrain Modifikationen
            x := (fsx + fCursorPos.x) Div MapBlockSize;
            y := (fsy + fCursorPos.y) Div MapBlockSize;
            glPushMatrix;
            glTranslatef(x * MapBlockSize - fsx, y * MapBlockSize - fsy, ctd_Buy_Preview_Layer);
            i := form4.ScrollBar1.Position;
            glTranslatef(0, 0, ctd_EPSILON);
            glEnable(GL_BLEND);
            glColor4f(1, 0, 0, 0.5);
            glBindTexture(GL_TEXTURE_2D, 0);
            glBegin(GL_QUADS);
            glVertex2f(-i * MapBlockSize, -i * MapBlockSize);
            glVertex2f((i + 1) * MapBlockSize, -i * MapBlockSize);
            glVertex2f((i + 1) * MapBlockSize, (i + 1) * MapBlockSize);
            glVertex2f(-i * MapBlockSize, (i + 1) * MapBlockSize);
            glend;
            glDisable(GL_BLEND);
            glPopMatrix;
          End;

          If form4.CheckBox7.Checked Then Begin // Waypoint Flächen Modifikationen
            // Cursor
            x := (fsx + fCursorPos.x) Div MapBlockSize;
            y := (fsy + fCursorPos.y) Div MapBlockSize;
            glPushMatrix;
            glTranslatef(x * MapBlockSize - fsx, y * MapBlockSize - fsy, ctd_Buy_Preview_Layer);
            i := form4.ScrollBar2.Position;
            glTranslatef(0, 0, ctd_EPSILON);
            glEnable(GL_BLEND);
            glColor4f(0, 1, 0, 0.5);
            glBindTexture(GL_TEXTURE_2D, 0);
            glBegin(GL_QUADS);
            glVertex2f(-i * MapBlockSize, -i * MapBlockSize);
            glVertex2f((i + 1) * MapBlockSize, -i * MapBlockSize);
            glVertex2f((i + 1) * MapBlockSize, (i + 1) * MapBlockSize);
            glVertex2f(-i * MapBlockSize, (i + 1) * MapBlockSize);
            glend;
            glDisable(GL_BLEND);
            glPopMatrix;
            // Die Karten Ansicht
            For i := 0 To fMap.Width - 1 Do Begin
              For j := 0 To fMap.Height - 1 Do Begin
                If fMap.fTerrain[i, j].WArea Then Begin
                  glPushMatrix;
                  glTranslatef(i * MapBlockSize - fsx + fMapL, j * MapBlockSize - fsy + fMapT, ctd_Buy_Preview_Layer);
                  glTranslatef(0, 0, ctd_EPSILON);
                  glEnable(GL_BLEND);
                  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
                  glBindTexture(GL_TEXTURE_2D, 0);
                  glColor4f(0, 0.75, 0, 0.5); // Feld ist ein Flächenwegpunkt
                  glBegin(GL_QUADS);
                  glVertex2f(0, MapBlockSize);
                  glVertex2f((0 + 1) * MapBlockSize, -0 * MapBlockSize + MapBlockSize);
                  glVertex2f((0 + 1) * MapBlockSize, -(0 + 1) * MapBlockSize + MapBlockSize);
                  glVertex2f(0 * MapBlockSize, -(0 + 1) * MapBlockSize + MapBlockSize);
                  glend;
                  glDisable(GL_BLEND);
                  glPopMatrix;
                End;
              End;
            End;
          End;

          If form4.CheckBox6.Checked And (assigned(PlacementeObject)) And form4.RadioButton4.Checked Then Begin // Placement preview
            x := (fsx + fCursorPos.x) Div MapBlockSize;
            y := (fsy + fCursorPos.y) Div MapBlockSize;
            glPushMatrix;
            glTranslatef(x * MapBlockSize - fsx, y * MapBlockSize - fsy, ctd_Buy_Preview_Layer);
            PlacementeObject.Render(false);
            glPopMatrix;
          End;

          RenderBlackOutMapBorder;
          If (fgameState = gs_EditMode) Or (Not DarkOtherBuildings) Then Begin
            fMap.RenderPreview(fPreviewL + 1, fPreviewT + 1, fPreviewW - 2, fPreviewH - 2, fsx, fsy, fMapW, fMapH - fMapT, -1);
          End
          Else Begin
            fMap.RenderPreview(fPreviewL + 1, fPreviewT + 1, fPreviewW - 2, fPreviewH - 2, fsx, fsy, fMapW, fMapH - fMapT, fPlayerIndex);
          End;
          fSplashMarks.RenderPreview(fPreviewL + 1, fPreviewT + 1, fPreviewW - 2, fPreviewH - 2, fmap.Width, fmap.Height);
          RenderMenuTabButton();
        End
        Else Begin
          fNewMapButton.Render();
          fLoadMapButton.Render();
          fLoadGameButton.Render();
          // Print instructions
          OpenGL_ASCII_Font.Color := clYellow;
          CenterTextOut(w, h, 'Please create or load a map, or load a old game.');
          If VersionInfoString <> '' Then Begin
            RenderToolTipp(w - 10, h - 10, VersionInfoString);
          End;
          // Print Connected Player Names
          OpenGL_ASCII_Font.Color := clGray;
          s := '';
          For i := 0 To high(fPlayerInfos) Do Begin
            s := s + '   ' + fPlayerInfos[i].Name + LineEnding;
          End;
          TextOut(0, 32, 'Online Players:' + LineEnding + s);
        End;
      End;
  End;
  fSplashhints.Render();
  WidgetSetExit2d();
End;

Procedure Tctd.Resize;
Var
  offset, ww, w, h, i: Integer;
  s: Single;
  p: String;
Begin
  If (fwold = -1) Or (fhold = -1) Then exit; // Aufruf obwohl noch nicht initialisiert
  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'textures' + PathDelim;
  offset := 0;
  fTargetsInfo.Top := 0;
  fTargetsInfo.Left := 0;
  fTargetsInfo.Height := 64;
  fTargetsInfo.Width := 128;
  fCoinsInfo.Top := 0;
  fCoinsInfo.Height := 64;
  fCoinsInfo.Width := 128 + 64;
  fLivesInfo.Top := 0;
  fLivesInfo.Width := 128;
  fLivesInfo.Height := 64;
  fWaveinfo.Top := 0;
  fWaveinfo.Width := 144; // 128 + 64;
  fWaveinfo.Height := 48; // 64;
  fHostButton.Top := fhold Div 2 - 2 * fJoinButton.Height;
  fHostButton.Left := (fwold - fJoinButton.Width) Div 2;
  fJoinButton.Top := fhold Div 2 + fJoinButton.Height;
  fJoinButton.Left := (fwold - fJoinButton.Width) Div 2;
  fNewMapButton.Top := fhold Div 2 - 4 * fNewMapButton.Height;
  fNewMapButton.Left := (fwold - fNewMapButton.Width) Div 2;
  fLoadMapButton.Top := fhold Div 2 - 2 * fNewMapButton.Height;
  fLoadMapButton.Left := (fwold - fNewMapButton.Width) Div 2;
  fLoadGameButton.Top := fhold Div 2 + fNewMapButton.Height;
  fLoadGameButton.Left := (fwold - fNewMapButton.Width) Div 2;

  // wold und hold sind hier bereits die neue Breite / Höhe
  Case fMenuPosition Of
    mpBottom: Begin
        If Not fSidemenuVisible Then Begin
          offset := round(fhold * 0.3);
          Tab_Image.SetImage(OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabUp, smStretchHard));
        End
        Else Begin
          Tab_Image.SetImage(OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabDown, smStretchHard));
        End;
        fCoinsInfo.Left := fwold - fCoinsInfo.Width;
        (*
         ************************
         *                      *
         *     Spielfeld        *
         *                      * 70 % Hähe
         *                      *
         *                      *
         ************************
         * Map * Baumenü * Desc.* 30 % Höhe
         *     *         *      *
         ************************
           30%     40%      30%
        *)
        fPreviewL := 0;
        fPreviewT := round(fhold * 0.7) + offset;
        fPreviewW := round(fwold * 0.3);
        fPreviewH := round(fhold * 0.3);

        fMapL := 0;
        If fgameState = gs_Gaming Then Begin
          fMapT := 64;
        End
        Else Begin
          fMapT := 0;
        End;
        fMapW := fwold;
        fMapH := round(fhold * 0.7) + offset - fMapT;

        fBuildL := round(fwold * 0.3);
        fBuildT := round(fhold * 0.7) + offset;
        fBuildW := round(fwold * 0.4);
        fBuildH := round(fhold * 0.3);

        fDestL := round(fwold * 0.7);
        fDestT := round(fhold * 0.7) + offset;
        fDestW := round(fwold * 0.3);
        fDestH := round(fhold * 0.3);

        tab_Image.Top := fMapH - Tab_Image.Height + fMapT;
        tab_Image.Left := 0;
      End;
    mpRight: Begin
        If Not fSidemenuVisible Then Begin
          offset := round(fwold * 0.3);
          Tab_Image.SetImage(OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabLeft, smStretchHard));
          fCoinsInfo.Left := fwold - fCoinsInfo.Width;
        End
        Else Begin
          Tab_Image.SetImage(OpenGL_GraphikEngine.LoadAlphaGraphik(p + TabRight, smStretchHard));
          fCoinsInfo.Left := fwold - fCoinsInfo.Width - round(fwold * 0.3);
        End;
        (*
               70 % Breite          30 % Breite
         **********************************
         *                      *   Map   * 30%
         *     Spielfeld        ***********
         *     (Fmap)           * Baumenü * 40%
         *                      ***********
         *                      * Desc    * 30%
         **********************************
        *)
        fMapL := 0;
        If fgameState = gs_Gaming Then Begin
          fMapT := 64;
        End
        Else Begin
          fMapT := 0;
        End;
        fMapW := round(fwold * 0.7) + offset;
        fMapH := fhold;

        fPreviewL := round(fwold * 0.7) + offset;
        fPreviewT := 0;
        fPreviewW := round(fwold * 0.3);
        fPreviewH := round(fhold * 0.3);

        fBuildL := round(fwold * 0.7) + offset;
        fBuildT := round(fhold * 0.3);
        fBuildW := round(fwold * 0.3);
        fBuildH := round(fhold * 0.4);

        fDestL := round(fwold * 0.7) + offset;
        fDestT := round(fhold * 0.7);
        fDestW := round(fwold * 0.3);
        fDestH := round(fhold * 0.3);

        If fgameState = gs_Gaming Then Begin
          tab_Image.Top := 64;
        End
        Else Begin
          tab_Image.Top := 0;
        End;
        tab_Image.Left := round(fwold * 0.7) - Tab_Image.Width + offset;
      End;
  End;

  fLivesInfo.Left := (fMapW - fLivesInfo.Width) Div 2;
  fWaveinfo.left := max(min(fLivesInfo.Left - fWaveinfo.Width, (fMapW - fLivesInfo.Width) Div 4), fTargetsInfo.Width);

  // Der Inhalt des Desc Bereichs
  Level_Up_Image.left := fDestL;
  Level_Up_Image.Top := fDestT;
  Level_Up_Image.Width := round(fDestW / 2) - 6;
  Level_Up_Image.Height := 25;

  sell_Image.left := Level_Up_Image.left + Level_Up_Image.Width + 6;
  sell_Image.Top := Level_Up_Image.Top;
  sell_Image.Width := Level_Up_Image.Width;
  sell_Image.Height := 25;

  fSplashhints.Position := point(0, fMapH - 16);
  Check_Scrollborders();
  w := round((fwold * 0.3 / length(BuildingStrategyButtons))) - 6;
  ww := w + 6;
  h := round(fhold * 0.3 / length(BuildingStrategyButtons));
  For i := 0 To high(BuildingStrategyButtons) Do Begin
    BuildingStrategyButtons[i].Width := w;
    BuildingStrategyButtons[i].Height := h;
    BuildingStrategyButtons[i].Top := fhold - h - 6 + offset; // Sonderfall da nicht von den 16- Hauptkonstanten abhängig
    BuildingStrategyButtons[i].Left := round(fwold * (0.7) + ww / 2 - w / 2 + i * ww);
  End;
  // Die breite des Trennstriches im Describtion Bereich (trennt Aktuelles Stage von nächst höhrem Stage ab)
  s := max(1, OpenGL_ASCII_Font.TextWidth('-') * fontscale);
  fDashSeparator := '';
  For i := 0 To ceil(fDestW / s) Do Begin
    fDashSeparator := fDashSeparator + '-';
  End;
  // Ende Der Inhalt des Desc Bereichs
End;

Procedure Tctd.CreateBuildMenu(Wave: integer);
Var
  i: integer;
  ap: String;
  ClearBuying: Boolean; // True, wenn das fBuyingObject nicht Kaufbar ist => Platt machen
Begin
  log('Tctd.CreateBuildMenu : ' + inttostr(wave), llTrace);
  // Alles alte Aufräumen
  For i := 0 To high(FBuyMenu.Items) Do Begin
    FBuyMenu.Items[i].obj.Free;
  End;
  setlength(FBuyMenu.Items, 0);
  ClearBuying := assigned(FBuyingObject);
  // Und neu erstellen
  Ap := MapFolder + MapName + PathDelim;
  FBuyMenu.MaxItemSize := point(0, 0);
  For i := 0 To fMap.BuyAblesCount - 1 Do Begin
    If fmap.BuyAbles[i].WaveNum <= wave Then Begin
      setlength(FBuyMenu.Items, high(FBuyMenu.Items) + 2);
      FBuyMenu.Items[high(FBuyMenu.Items)].Count := fmap.BuyAbles[i].Count;
      FBuyMenu.Items[high(FBuyMenu.Items)].Item := fmap.BuyAbles[i].Item;
      FBuyMenu.Items[high(FBuyMenu.Items)].Kind := fMap.BuyAbles[i].Kind;
      Case fMap.BuyAbles[i].Kind Of
        bkBuilding: Begin
            FBuyMenu.Items[high(FBuyMenu.Items)].obj := TBuilding.create();
            FBuyMenu.Items[high(FBuyMenu.Items)].obj.LoadFromFile(ap + fmap.BuyAbles[i].Item);
            FBuyMenu.Items[high(FBuyMenu.Items)].Category := TBuilding(FBuyMenu.Items[high(FBuyMenu.Items)].obj).Category;
          End;
        bkHero: Begin
            FBuyMenu.Items[high(FBuyMenu.Items)].obj := Thero.create();
            FBuyMenu.Items[high(FBuyMenu.Items)].obj.LoadFromFile(ap + fmap.BuyAbles[i].Item);
            FBuyMenu.Items[high(FBuyMenu.Items)].Category := 'Hero';
          End;
      Else Begin
          Raise exception.create('Missing implementation.');
        End;
      End;
      FBuyMenu.MaxItemSize.x := max(FBuyMenu.MaxItemSize.x, round(FBuyMenu.Items[high(FBuyMenu.Items)].obj.Width));
      FBuyMenu.MaxItemSize.y := max(FBuyMenu.MaxItemSize.y, round(FBuyMenu.Items[high(FBuyMenu.Items)].obj.Height));
      If ClearBuying Then Begin // Man Kann ein Gebäude Kaufen, welches den Selben Dateinamen hat wie das Aktuell gewählte -> Das Clearing wird abgebrichen
        If FBuyingObject.Filename = ap + fmap.BuyAbles[i].Item Then ClearBuying := false;
      End;
    End;
  End;
  // Alles Platt machen, was mit der Maus selektion zu tun hat..
  If ClearBuying Then Begin
    DeselectCursor()
  End;
  logleave;
End;

Function Tctd.GetMapObjUnderCursor(x, y: integer): tctd_mapopbject;
Begin
  result := Nil;
  // Ist die Maus überhaupt im Map Bereich ?
  If uctd_common.PointInRect(point(x, y), rect(fMapL, fMapT, fMapL + fMapW, fMapT + fMapH)) Then Begin
    result := fmap.GetObjUnderCursor(x + fsx - fMapL, y + fsy - fMapT);
  End;
End;

Function Tctd.GetBuildObjUnderCursor(x, y: integer): tctd_mapopbject;
Var
  i: Integer;
Begin
  result := Nil;
  If uctd_common.PointInRect(point(x, y), rect(fBuildL, fBuildT, fBuildL + fBuildW, fBuildT + fBuildH)) Then Begin
    x := x - fBuildL;
    y := y - fBuildT;
    x := x Div (MapBlockSize * FBuyMenu.MaxItemSize.x);
    y := y Div (MapBlockSize * FBuyMenu.MaxItemSize.y);
    // Berechnen des Index im FBuildingMenu Array
    i := y * (fBuildW Div (MapBlockSize * FBuyMenu.MaxItemSize.x)) + x;
    If (i >= 0) And (i <= High(FBuyMenu.Items)) Then Begin
      result := FBuyMenu.Items[i].obj;
    End;
  End;
End;

Procedure Tctd.SetBuyingObject(obj: tctd_mapopbject);
Begin
  If Not assigned(fBuyingObject) Then Begin
    If obj Is TBuilding Then Begin
      fBuyingObject := TBuilding.create();
    End
    Else Begin
      If obj Is THero Then Begin
        fBuyingObject := THero.create();
      End
      Else Begin
        Raise exception.Create('Tctd.SetBuyingObject: missing implementation.');
      End;
    End;
  End;
  fBuyingObject.LoadFromFile(obj.Filename);
  fSideMenuObject := fBuyingObject;
  fSelectedBuildings := Nil;
  fSelectedHeros := Nil;
  If fPlayerInfos[fPlayerIndex].Cash < fBuyingObject.GetBuyCost() Then Begin
    SplashHint('Warning, not enough money to buy ' + fBuyingObject.name, v3(1, 0, 0));
  End;
End;

Procedure Tctd.SetheroTargets(x, y: Single);
Var
  m: TMemoryStream;
  i: Integer;
Begin
  m := TMemoryStream.Create;
  m.Write(PlayerIndex, SizeOf(PlayerIndex));
  m.Write(x, SizeOf(x));
  m.Write(y, SizeOf(y));
  For i := 0 To high(fSelectedHeros) Do Begin
    If fSelectedHeros[i].Owner = fPlayerIndex Then Begin // Wir dürfen nur die Eigenen Heros steuern ;)
      m.write(fSelectedHeros[i].MapHeroIndex, sizeof(fSelectedHeros[i].MapHeroIndex));
    End;
  End;
  If m.size <> (2 * SizeOf(x) + sizeof(PlayerIndex)) Then Begin
    SendChunk(miSetHeroTargets, m);
  End
  Else Begin
    m.free;
  End;
End;

Procedure Tctd.BuyBuilding(x, y: integer; Obj: TBuilding);
Var
  m: TMemoryStream;
Begin
  log(format('Tctd.BuyBuilding %dx%d %s', [x, y, extractfilename(Obj.Filename)]), llTrace);
  m := TMemoryStream.Create;
  m.WriteAnsiString(extractfilename(Obj.Filename));
  m.Write(x, SizeOf(x));
  m.Write(y, SizeOf(y));
  SendChunk(miBuyBuilding, m);
  LogLeave;
End;

Procedure Tctd.BuyHero(x, y: integer; Obj: THero);
Var
  m: TMemoryStream;
Begin
  log(format('Tctd.BuyHero %dx%d %s', [x, y, extractfilename(Obj.Filename)]), llTrace);
  m := TMemoryStream.Create;
  m.WriteAnsiString(extractfilename(Obj.Filename));
  m.Write(x, SizeOf(x));
  m.Write(y, SizeOf(y));
  SendChunk(miBuyHero, m);
  LogLeave;
End;

Procedure Tctd.AddBuilding(x, y: integer; Name: String; Owner: integer);
Var
  b: TBuilding;
Begin
  log(format('Tctd.AddBuilding %d, %d/%d %s', [Owner, x, y, Name]), llTrace);
  b := TBuilding.create();
  b.LoadFromFile(MapFolder + MapName + PathDelim + Name);
  b.Owner := Owner;
  If Not fMap.AddBuilding(x, y, b) Then Begin
    (*
     * Eigentlich hat der Server alles Geprüft, das Gebäude muss hier immer Akzeptiert werden
     * Wenn aber doch nicht, dann geben wir es wenigstens frei ...
     *)
    logshow(format('Could not add Building, that was accepted by the server [%d, %d/%d %s]', [Owner, x, y, Name]), llCritical);
    b.free;
  End;
  LogLeave;
End;

Procedure Tctd.AddHero(x, y: integer; Name: String; Owner: integer);
Var
  h: THero;
Begin
  log(format('Tctd.AddHero %d, %d/%d %s', [Owner, x, y, Name]), llTrace);
  h := THero.create();
  h.LoadFromFile(MapFolder + MapName + PathDelim + Name);
  h.Owner := Owner;
  If Not fMap.AddHero(x, y, h) Then Begin
    (*
     * Eigentlich hat der Server alles Geprüft, das Gebäude muss hier immer Akzeptiert werden
     * Wenn aber doch nicht, dann geben wir es wenigstens frei ...
     *)
    logshow(format('Could not add Hero, that was accepted by the server [%d, %d/%d %s]', [Owner, x, y, Name]), llCritical);
    h.free;
  End;
  LogLeave;
End;

Procedure Tctd.SwitchToEditMode;
Begin
  fLeftMousePressed := false;
  fgameState := gs_EditMode;
  fSelectedBuildings := Nil;
  fSelectedHeros := Nil;
  If assigned(fMap) Then
    fmap.ClearMoveables;
  If assigned(OnForceEditMode) Then
    OnForceEditMode(self);
  Resize;
End;

Function Tctd.fGetPlayerCount: integer;
Begin
  result := length(fPlayerInfos);
End;

Procedure Tctd.NewMap(w, h: Integer; n: String);
Var
  i: Integer;
  m: tmemorystream;
  p: String;
  png: TPortableNetworkGraphic;
Begin
  log(format('Tctd.NewMap %dx%d %s', [w, h, n]), llTrace);
  m := TMemoryStream.Create;
  i := w;
  m.Write(i, sizeof(i));
  i := h;
  m.Write(i, sizeof(i));
  m.WriteAnsiString(n);
  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'textures' + PathDelim;
  png := TPortableNetworkGraphic.Create;
  png.LoadFromFile(p + 'dc1.png');
  png.SaveToStream(m);
  png.LoadFromFile(p + 'dc2.png');
  png.SaveToStream(m);
  png.LoadFromFile(p + 'dc3.png');
  png.SaveToStream(m);
  png.LoadFromFile(p + 'dc4.png');
  png.SaveToStream(m);
  png.free;
  SendChunk(miNewMap, m);
  LogLeave;
End;

Procedure Tctd.LoadMap(MapName: String);
Var
  m: TMemoryStream;
Begin
  fstatebeforeloading := fgameState;
  fgameState := gs_Loading;
  m := TMemoryStream.Create;
  m.WriteAnsiString(MapName);
  SendChunk(miRequestLoadMap, m);
End;

Procedure Tctd.getMapList(Callback: TOnGetStringListEvent);
Var
  m: TMemoryStream;
  Identifier: integer;
Begin
  log('Tctd.getMapList', llTrace);
  m := TMemoryStream.Create;
  Identifier := GetNextGlobalStreamQueueID Shl 16;
  If Not fsh.AddWaitIdentifier(Identifier, Callback) Then Begin
    LogShow('Tctd.getMapList Identifier already exists.', llCritical);
  End;
  SendChunk(miRequestMapList Or Identifier, m);
  LogLeave;
End;

Procedure Tctd.getMapPrieviewInfo(MapName: String;
  Callback: TOnGetMapPrieviewInfoEvent);
Var
  m: TMemoryStream;
  Identifier: integer;
Begin
  log('Tctd.getMapPrieviewInfo : ' + MapName, llTrace);
  m := TMemoryStream.Create;
  m.WriteAnsiString(MapName);
  Identifier := GetNextGlobalStreamQueueID Shl 16;
  If Not fsh.AddWaitIdentifier(Identifier, Callback) Then Begin
    LogShow('Tctd.getMapPrieviewInfo(' + MapName + ') Identifier already exists.', llCritical);
  End;
  SendChunk(miRequestMapPreviewInfo Or Identifier, m);
  LogLeave;
End;

Procedure Tctd.CloneMapWave(SourceWaveNum, DestWaveNum: integer);
Var
  m: TMemoryStream;
Begin
  log('Tctd.CloneMapWave : ' + inttostr(SourceWaveNum) + ' -> ' + inttostr(DestWaveNum), llTrace);
  m := TMemoryStream.Create;
  m.Write(SourceWaveNum, sizeof(SourceWaveNum));
  m.Write(DestWaveNum, sizeof(DestWaveNum));
  SendChunk(miCloneMapWave, m);
  DoWaveClone(SourceWaveNum, DestWaveNum);
  LogLeave;
End;

Procedure Tctd.UpdateMapProperty(MapProperty: integer; Const Stream: TStream);
Var
  m: TMemoryStream;
Begin
  log('Tctd.UpdateMapProperty : ' + MessageMapPropertyToString(MapProperty), llTrace);
  If BlockMapUpdateSending Then Begin
    stream.Free; // Wir senden zwar nicht, aber der Übergebene Stream muss dennoch freigegeben werden.
    LogLeave;
    exit;
  End;
  m := TMemoryStream.Create;
  m.Write(MapProperty, sizeof(MapProperty));
  m.Write(fServerUid, sizeof(fServerUid));
  If assigned(stream) Then Begin
    stream.Position := 0;
    m.CopyFrom(stream, stream.Size);
    stream.Free;
  End;
  SendChunk(miUpdateMapProperty, m);
  LogLeave;
End;

Procedure Tctd.InitiateNewGame(Difficulty: integer);
Var
  m: TMemoryStream;
Begin
  log('Tctd.InitiateNewGame : ' + inttostr(Difficulty), llTrace);
  m := TMemoryStream.Create;
  m.Write(Difficulty, SizeOf(Difficulty));
  SendChunk(miInitiateNewGame, m);
  LogLeave;
End;

Procedure Tctd.RestartRound;
Begin
  log('Tctd.RestartRound', llTrace);
  SendChunk(miRestartLastWave, Nil);
  LogLeave;
End;

Procedure Tctd.Continue;
Begin
  log('Tctd.Continue', llTrace);
  SendChunk(miContinue, Nil);
  LogLeave;
End;

Procedure Tctd.AbortWave;
Begin
  log('Tctd.Continue', llTrace);
  SendChunk(miAbortWave, Nil);
  LogLeave;
End;

Procedure Tctd.Splashhint(Const Text: String; Color: TVector3);
Begin
  log('Tctd.Splashhint : ' + Text, llTrace);
  fSplashhints.AddText(text, Color);
  LogLeave;
End;

Procedure Tctd.splashAirLevels;
Var
  s: String;
Begin
  log('Tctd.splashAirLevels', llTrace);
  If assigned(fmap) Then Begin
    s := 'Airlevels : ' + fmap.getAirLevels;
  End
  Else Begin
    s := 'No map loaded.';
  End;
  form13.AddMessage(s);
  Splashhint(s, DefaultSplashHintColor);
  LogLeave;
End;

Procedure Tctd.splashBossLevels;
Var
  s: String;
Begin
  log('Tctd.splashBossLevels', llTrace);
  If assigned(fmap) Then Begin
    s := 'Bosslevels : ' + fmap.getBossLevels;
  End
  Else Begin
    s := 'No map loaded.';
  End;
  form13.AddMessage(s);
  Splashhint(s, DefaultSplashHintColor);
  LogLeave;
End;

Procedure Tctd.splashBonusLevels;
Var
  s: String;
Begin
  log('Tctd.splashBonusLevels', llTrace);
  If assigned(fmap) Then Begin
    s := 'Bonuslevels : ' + fmap.getBonusLevels;
  End
  Else Begin
    s := 'No map loaded.';
  End;
  form13.AddMessage(s);
  Splashhint(s, DefaultSplashHintColor);
  LogLeave;
End;

Procedure Tctd.RequestPingTimes;
Begin
  log('Tctd.RequestPingTimes', llTrace);
  SendChunk(miReQuestPingTimes, Nil);
  LogLeave;
End;

Procedure Tctd.SendKillCommand(PlayerName: String);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.WriteAnsiString(PlayerName);
  SendChunk(miKickPlayerOut, m);
End;

Procedure Tctd.TogglePause;
Begin
  log('Tctd.TogglePause', llTrace);
  If fgameState = gs_Gaming Then Begin
    SendChunk(miTogglePause, Nil);
  End;
  LogLeave;
End;

Procedure Tctd.SendChat(Message: String);
Var
  m: TMemoryStream;
Begin
  log('Tctd.SendChat : ' + Message, llTrace);
  m := TMemoryStream.Create;
  m.WriteAnsiString(Serialize(Message));
  SendChunk(miChatMessage, m);
  LogLeave;
End;

Procedure Tctd.TransferFile(SourceFile: String; Dest: String;
  Callback: TOnGetBooleanEvent);
Var
  crc: UInt32;
  m: TMemoryStream;
  Identifier: integer;
  r: TOnGetBooleanEventRecord;
Begin
  log('Tctd.TransferFile', llTrace);
  If FileExistsUTF8(SourceFile) And (SourceFile <> '') Then Begin
    crc := CRC_of_File(SourceFile);
    m := TMemorystream.create;
    m.WriteAnsiString(MapName);
    m.WriteAnsiString(Dest);
    m.Write(crc, sizeof(crc));
    Identifier := GetNextGlobalStreamQueueID() Shl 16;
    r.Callback := Callback;
    r.s := SourceFile;
    r.d := Dest;
    If Not fsh.AddWaitIdentifier(Identifier, r) Then Begin
      LogShow('Tctd.getMapList Identifier already exists.', llCritical);
    End;
    SendChunk(miRequestFileTransfer Or Identifier, m);
  End
  Else Begin
    Callback(self, false);
  End;
  LogLeave;
End;

Procedure Tctd.GetFileList(Mask: String; Callback: TOnGetStringListEvent);
Var
  m: TMemoryStream;
  Identifier: integer;
Begin
  log('Tctd.GetFileList : ' + mask, llTrace);
  m := TMemoryStream.Create;
  m.WriteAnsiString(Mask);
  Identifier := GetNextGlobalStreamQueueID Shl 16;
  If Not fsh.AddWaitIdentifier(Identifier, Callback) Then Begin
    LogShow('Tctd.GetFileList(' + Mask + ') Identifier already exists.', llCritical);
  End;
  SendChunk(miRequestFileList Or Identifier, m);
  LogLeave;
End;

Procedure Tctd.GetSavegames(Callback: TOnGetStreamEvent);
Var
  m: TMemoryStream;
  Identifier: integer;
Begin
  log('Tctd.GetSavegames', llTrace);
  m := TMemoryStream.Create;
  Identifier := GetNextGlobalStreamQueueID Shl 16;
  If Not fsh.AddWaitIdentifier(Identifier, Callback) Then Begin
    LogShow('Tctd.GetSavegames Identifier already exists.', llCritical);
  End;
  SendChunk(miRequestSavegames Or Identifier, m);
  LogLeave;
End;

Procedure Tctd.SaveGame(Filename: String);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.WriteAnsiString(Filename);
  SendChunk(miSaveGame, m);
End;

Procedure Tctd.LoadGame(Filename: String);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.WriteAnsiString(Filename);
  SendChunk(miLoadGame, m);
End;

Procedure Tctd.DelSaveGame(Filename: String);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.WriteAnsiString(Filename);
  SendChunk(midelSaveGame, m);
End;

Procedure Tctd.DelAllSaveGames;
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  SendChunk(midelSaveGames, m);
End;

Procedure Tctd.DelOpponent(Opp: String);
Var
  m: TMemoryStream;
Begin
  If assigned(fMap) Then fMap.delOpponent(Opp);
  m := TMemoryStream.Create;
  m.WriteAnsiString(opp);
  SendChunk(miDelOpponent, m);
End;

Procedure Tctd.DelBuilding(Geb: String);
Var
  m: TMemoryStream;
Begin
  If assigned(fMap) Then fMap.delBuilding(geb);
  m := TMemoryStream.Create;
  m.WriteAnsiString(geb);
  SendChunk(miDelBuilding, m);
End;

Procedure Tctd.DelHero(Hero: String);
Var
  m: TMemoryStream;
Begin
  If assigned(fMap) Then fMap.delHero(Hero);
  m := TMemoryStream.Create;
  m.WriteAnsiString(Hero);
  SendChunk(miDelHero, m);
End;

Procedure Tctd.TransferCash(DestPlayer, Count: integer);
Var
  i: integer;
  m: TMemoryStream;
Begin
  If (DestPlayer <> fPlayerIndex) Then Begin
    m := TMemoryStream.Create;
    i := fPlayerInfos[DestPlayer].uid;
    m.Write(i, SizeOf(i));
    i := count;
    m.Write(i, SizeOf(i));
    SendChunk(miTransferCash, m);
  End;
End;

Procedure Tctd.TransferCompleteMapTerrain(Const Stream: TStream);
Begin
  SendChunk(miTransferCompleteMapTerrain, Stream);
End;

Procedure Tctd.DoSell;
Begin
  OnSellButtonClick(Nil);
End;

Procedure Tctd.DoHeroStop;
Begin
  // Die Koordinate -1,-1 ist der "Stop" ;-)
  SetheroTargets(-1, -1);
End;

Procedure Tctd.DoSelectAllOwnHeros;
Begin
  If Not assigned(fMap) Then exit;
  If (fgameState = gs_Gaming) Then Begin
    fSelectedHeros := fMap.GetAllHerosOfOwner(fPlayerIndex);
    If Assigned(fSelectedHeros) Then Begin
      fSelectedBuildings := Nil;
      fSideMenuObject := fSelectedHeros[0]; // den 1. Anwählen sonst sieht man das nicht
      If Assigned(fBuyingObject) Then Begin
        fBuyingObject.free;
        fBuyingObject := Nil;
      End;
    End;
  End;
End;

Procedure Tctd.DoUpdate;
Begin
  OnLevelUpButtonClick(Nil);
End;

Procedure Tctd.IncSpeed;
Begin
  If fgameState = gs_Gaming Then
    SendChunk(miIncSpeed, Nil);
End;

Procedure Tctd.DecSpeed;
Begin
  If fgameState = gs_Gaming Then
    SendChunk(midecSpeed, Nil);
End;

Procedure Tctd.RequestPlayerInfos;
Begin
  SendChunk(miRefreshPlayerStats, Nil);
End;

Procedure Tctd.ChangePlayerPos(aPlayerindex: integer; Up: Boolean);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.Write(aPlayerindex, SizeOf(integer));
  If Up Then
    aPlayerindex := 1
  Else
    aPlayerindex := 0;
  m.Write(aPlayerindex, SizeOf(integer));
  SendChunk(miRequestPlayerPosChange, m);
End;

Procedure Tctd.CleanupUnusedOpponets();
Begin
  SendChunk(miCleanupUnusedOpponents, Nil);
End;

Initialization

  ini := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'settings.ini');

Finalization

  ini.free;
  ini := Nil;

End.


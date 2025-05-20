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
Unit uctd_map;

{$MODE objfpc}{$H+}

{$I apptype.inc}

(* -- Lazarus Bug in Auswertung von apptype.inc -- Dieser Block darf "aktiviert" nie ins GIT
{$UNDEF Client}
{$DEFINE Server}
//*)

Interface

Uses
  Classes, SysUtils
  , IniFiles
{$IFDEF Client}
  , dglOpenGL
  , uopengl_graphikengine
  , uOpenGL_ASCII_Font
  , uopengl_animation
{$ENDIF}
  , uctd_mapobject, uctd_building, uctd_opp, uctd_hero, uvectormath, uctd_common
{$IFDEF Server}
  , uctd_bullet
{$ENDIF}
  ;

Const
  MapRootName = 'data.map';
  Field_unreached = high(Integer); // Information für die Höhenkarte das der Punkt nicht Erreicht wurde, dieser Wert muss mindestens ((KartenBreite*KartenHöhe)+1) sein
  PLacementIdentiferOffset = 60000; // Der Index ab dem die Identifiert zu Placements werden (der Range ist 16-Bit) -> 5535 Placements auf einer Karte möglich

Type

  TMapType = (
    mtCoop, // Grobklassifizierung der Karte, hier Spielen mehrere Spieler zusammen gegen alle Gegner
    mtSingle, // Hier Spielen Mehrere Spieler, aber im Prinzip jeder für sich.
    // Bei diesen Karten müssen die Spieler die Labyrinthe selbst bauen
    mtCoopMaze, // Grobklassifizierung der Karte, hier Spielen mehrere Spieler zusammen gegen alle Gegner
    mtSingleMaze, // Hier Spielen Mehrere Spieler, aber im Prinzip jeder für sich.
    mtUnknown // Nur zum Fehlerhandling
    );

  TWaveOpponent = Record
    opponent: String; // Dateiname des Gegners
    Count: integer; // Anzahl der Gegner die gesamt emitiert werden sollen
    refund: Integer; // Geld das es für eine Vernichtete Einheit gibt
    UnitsPerSpawn: integer; // Anzahl an Gegnern die Gleichzeitig emittiert werden sollen
    Spawndelta: integer; // Zeit in MS zwischen 2 emittierungen
    SpawnDelay: integer; // Zeit in ms bis das erste mal emittiert werden soll
  End;

  TWave = Record
    ChashOnStart: integer;
    WaveHint: String;
    Opponents: Array Of TWaveOpponent;
  End;

  TBuyAbleKind = (bkBuilding, bkHero);

  TBuyAble = Record
    Item: String;
    Kind: TBuyAbleKind;
    WaveNum: integer;
    Count: integer;
  End;

  // Informationen welche Pro Gridpunk in der Karte Verfügbar sind.
  TFieldItem = Record
    WArea: Boolean; // Wenn True, dann ist die Kachel eine Waypoint Flächen Area, wenn False, dann nicht
    data: integer; // Begehbar / Nicht begehbar, was in der Karte Gespeichert wird
    Blocked: Boolean; // True, wenn hier ein Gebäude Gebaut wurde (also nicht Walkable, und nicht mehr Buildable) -- wird nicht gespeichert
    DynBlocked: Boolean; // True, wenn sich hier mindestens 1 Opponent befindet
    // Jede Koordinate hat für jeden Spieler und alle seine Wegpunkte einen Höhenwert, dieser zeigt den Abstand zum nächsten Wegpunkt an 0 = erreicht
    WaypointHeightMap: Array Of Array Of integer;
    AlreadyChecked: Boolean; // Flag für die Wegpunktflächenberechnung, wird dynamisch initialisiert -- Wird nicht gespeichert
  End;

  TIndexMapper = Record
    Index: Integer;
    Filename: String;
    // Die Daten
{$IFDEF Client}
    Image: TGraphikItem; // Statisch
{$ENDIF}
    SizeX: single; // Statisch
    SizeY: single; // Statisch
    TotalLifePoints: integer; // Statisch
    CanFly: Boolean; // Statisch
    Hint: String; // Statisch -- ist schon Deserialisiert
    obj: TOpponent; // Statisch für das Hint Rendering (ersetzt quasi alles oben) ..

    Pos: TVector2; // Dynamisch
    Angle: Integer; // Dynamisch
    LifePoints: integer; // Dynamisch
  End;

  TBulletIndex = Record
    Name: String;
{$IFDEF Client}
    Animation: TOpenGL_Animation;
{$ENDIF}
    fimage: integer;
    Width: Single;
    Height: Single;
    Speed: Single;
  End;

  { TIniHighscoreEngine }

  // Differenzierung nach Anzahl Spieler
  // Abgespeichert wird: Spielername/n, Datum, Spielzeit (Simulated), erreichte Wave, Difficulty

  THighScoreDataSet = Record
    Name: String;
    TimeStamp: TDateTime;
    Points: int64;
    PlayTimeinS: int64;
    Rounds: integer;
    Difficulty: integer; // 0 = Easy, 1 = Normal, 2 = Hard
    LivesLost: integer;
  End;

  TPlayerHighscore = Record
    PlayerCount: integer;
    DataSets: Array Of THighScoreDataSet;
  End;

  TIniHighscoreEngine = Class
  private
    fDataSets: Array Of TPlayerHighscore;
    Function getPlayerCatCount: integer;
    Function CalcHighscoreHash: integer;
  public
    Property PlayerCatCount: integer read getPlayerCatCount;

    Function PlayerCat(Index: integer): TPlayerHighscore;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Clear; // Löscht alle Einträge
    Procedure AddHighScore(aPlayerName: String; aTimeStamp: TDateTime; aPoints,
      aPlaytimeinS: int64; aRound, aDifficulty, PlayerCount, aLivesLost: integer);

    // Zum Datenaustausch mit den Clients
    Procedure SaveToStream(Const Stream: TStream);
    Procedure LoadFromStream(Const Stream: TStream);

    // Zum tatsächlichen Abspeichern auf der Festplatte
    Procedure LoadFromIniFile(Const Ini: tIniFile);
    Procedure SaveToIniFile(Const Ini: tIniFile);
  End;

  TCheckResult = Record
    Warnings: Boolean;
    Errors: Boolean;
  End;

  TWayPoint = Record
    Point: TPoint;
    Field: Array Of TPoint;
  End;

  { TMap }

  TMap = Class
  private
    fChanged: Boolean;
    FBackTex: String;
    fDC1Tex, fdc2Tex, fdc3Tex, fdc4Tex: String; // Die Texturen für die Damage Classen 1-4
    fDescription: String;
    fMapType: TMapType;
    fMaxPlayer: integer;
    fLives: Array[0..2] Of integer;

{$IFDEF Client}
    fOpenGLBackTex: TGraphikItem;
    fFTerrainBackTex: integer;
    fOpenGLDC1Tex, fOpenGLdc2Tex, fOpenGLdc3Tex, fOpenGLdc4Tex: TGraphikItem;
    fOpenGLWidth, fOpenGLHeight: integer;
    fRenderOpponents: Array Of TRenderOpponent;
    fRenderBullets: Array Of TRenderBullet;
    FIndexMapper: Array Of TIndexMapper;
{$ENDIF}
    Fbuildings: Array Of TBuilding; // Die Liste der durch Benutzer gebauten Gebäude
    fHeroes: Array Of THero;
    FBulletIndexes: Array Of TBulletIndex; // Der Index des jeweiligen Geschosses damit Client und Server die Richtigen Texturen haben ...
{$IFDEF Server}
    fHighscoreEngine: TIniHighscoreEngine;
    fOpponents: TOpponentInfoArray;
    FBullets: Array Of TBulletObject;
    fLastOppMoveTime: int64;
    fLastHeroMoveTime: int64;
    fLastBulletTime: int64;
    FPausing: Boolean;
    fPauseTime: int64;
    fHandleBuildingTime: int64;
    fRatingCount: integer; // Anzahl der Durchgeführten Ratings
    fRatingSum: integer; // integral über die Ratings
{$ENDIF}
    Procedure FixBuyableSorting;
    Function GetHeight: integer;
    Function GetLives(Index: integer): integer;
{$IFDEF Client}
    Function getHeroCount: integer;
    Function getOpenGLDamageClassTex(index: integer): TGraphikItem;
{$ENDIF}
    Function GetWidth: integer;

    Function getBuyAble(index: integer): TBuyAble;
    Function GetBuyAblesCount: integer;
    Procedure SetBackTex(AValue: String);
    Procedure SetDC1Tex(AValue: String);
    Procedure SetDC2Tex(AValue: String);
    Procedure SetDC3Tex(AValue: String);
    Procedure SetDC4Tex(AValue: String);
    Procedure SetDescription(AValue: String);
    Procedure SetLives(Index: integer; AValue: integer);
    Procedure SetMapType(AValue: TMapType);
    Procedure SetMaxPlayer(AValue: integer);
    Procedure SortPlacementByYCoordinate;

{$IFDEF Client}
    Procedure RenderAbstract(blockw, blockh: Single; Grid, HideLifePoints: Boolean; UserIndex: integer); // Rendert alles, was auf der Großen und der Previewkarte Sichtbar ist.
{$ENDIF}
    Function getOpponentCount: integer;
    Function getFieldHeight(x, y, Player, WayPoint: integer): integer;
{$IFDEF Server}
    Procedure CreateBullet(Const Target: TOpponent; Const Pos: TVector2; Const aOwner: tctd_mapopbject);
    Procedure KillOpponent(Const Opponent: TOpponent);
    Procedure ResetRating;
    Function GetRating(): Single;
    Procedure MoveAllHeroes;
{$ENDIF}
    Procedure SortBuildingsByYCoordinate;
    Procedure UpdatePlacemantBlocked(); // Aktualisiert die .Blocked eigenschaft in Fterrain
    Function getfield(x, y: integer): integer;
    Function GetListOfAllUsedFiles(IgnoreFile: String): TStringList; // Gibt eine Vollständige Liste aller Deteien die irgendwie von der Karte Geladen werden (alle Opps, Gebs , ...)
  public
    fTerrain: Array Of Array Of TFieldItem;
    fBuyAbles: Array Of TBuyAble; // Die Liste der Kaufbaren Objekte
    fPlacements: Array Of tctd_mapopbject;

    ShowBackTex: Boolean;
    EditTerrain: Boolean;

    Difficulty: Integer; // Wird in HandleInitiateNewRound gesetzt, der Schwierigkeitsgrad mit dem die Karte gerade gespielt wird

    ShowWaypoints: boolean; // Globale Steuerung der Anzeige der Wegpunkte "ViewWaypoints" differenziert dann genauer
    ViewWaypoints: integer; // -1 = Alle Anzeigen, 0..N-1 = Zeige Wegpunkte des jeweiligen Spielers

    // Todo : Damit Changed auch wirklich wirklich funktioniert -> alles nachfolgende via Properties lösen und dann changed setzen.
    //        \-> So lange das nicht tut ist das "Change" Public und wird von UpdateMap Properties händisch aufgerufen, nicht schön aber funktioniert!

    Waves: Array Of TWave;
    Waypoints: Array Of Array Of TWayPoint; // Die Wegpunkte der Spieler Gegner Dimension 1 : Spieler, Dimension 2 Die Anzahl der Wegpunkte, Angabe in Kachelcoodinaten

    Property BackTex: String read fBackTex write SetBackTex;
    Property BuyAbles[index: integer]: TBuyAble read getBuyAble;
    Property BuyAblesCount: integer read GetBuyAblesCount;
    Property Description: String read fDescription write SetDescription;
    Property DamageClass1Tex: String read fDC1Tex write SetDC1Tex;
    Property DamageClass2Tex: String read fDC2Tex write SetDC2Tex;
    Property DamageClass3Tex: String read fDC3Tex write SetDC3Tex;
    Property DamageClass4Tex: String read fDC4Tex write SetDC4Tex;
{$IFDEF Client}
    Property HeroCount: integer read getHeroCount;
    Property OpenGLDamageClassTex[index: integer]: TGraphikItem read getOpenGLDamageClassTex;
{$ENDIF}
    Property FieldHeight[x, y, Player, WayPoint: integer]: integer read getFieldHeight; // Gibt die Höheninformation der Koordinate in Abhängigkeit vom An zu laufenen Wegpunkt Pro Player Je Kleiner der Wert, desto näher ist man am Wegpunkt
    Property Height: integer read GetHeight;
    Property Lives[Index: integer]: integer read GetLives write SetLives;
    Property MapType: TMapType read fMapType write SetMapType;
    Property MaxPlayer: integer read fMaxPlayer write SetMaxPlayer;
    Property OpponentCount: integer read getOpponentCount; // Die Anzahl der Gegner, die Gerade Auf der Karte sind
    Property Width: integer read GetWidth;

    Constructor Create;
    Destructor Destroy; override;

    Procedure Clear;

    Procedure Load(Const MapName_: String);
    Procedure Save(Const MapName_: String);

    Procedure Resize(NewWidth, NewHeight: integer);
    Function CoordIsBuildable(x, y: integer; Const building: TBuilding): Boolean;
    Function CoordIsBuildable2(x, y: integer): Boolean;
    Function CoordIsWalkAble(x, y: integer): Boolean;
    Function OpponentsEmittedInWave(Wave: integer): integer; // Die Anzahl der Gegner, die Insgesammt in dieser Wave Emiriert werden

    (*
     * !! ACHTUNG !!
     *   Wenn Result ein TOpponent ist, dann muss man dessen Identifiert wegkopieren
     *   Speichert man nur den Pointer, wird der Identifier beim nächsten Erfogreichen Finden überschrieben.
     *   Dies geschieht weil TMap nur alle Unterschiedlichen Gegner listet nicht aber jeden einzeln, und daher für jeden
     *   Gegner immer nur eine gemeinsame klasse mit neuem Identifier rausgegeben wird.
     *)
    Function GetObjUnderCursor(x, y: integer): tctd_mapopbject;
    Function PlacementAtCoord(x, y: integer): tctd_mapopbject;
{$IFDEF Client}
    Procedure Render(sx, sy, x, y: integer; Grid, ShowLifePoints: Boolean; UserIndex: integer);
    Procedure RenderPreview(x, y, w, h, sx, sy, mw, mh: integer; UserIndex: integer); // Gendert eine Vorschau der Karte
    Procedure UpdateMoveables(Const data: TStream);
    Procedure ClearMoveables;

    Function getAirLevels: String;
    Function getBonusLevels: String;
    Function getBossLevels: String;
    Procedure UpdateBackTexCoord(x, y: integer);
    Function UpdateOpponentData(Const op: TOpponent): Boolean;
    Procedure RefreshBackTex;
    Function GetAllBuildingsSameOfSameType(Const b: TBuilding): TBuildingArray;
    Function GetAllHeroesOfSameType(Const h: THero): THeroArray;
    Function GetAllHeroesOfOwner(Owner: integer): THeroArray;
    Function GetAllBuildingsSameOfSameTypeInRect(Const b: TBuilding; r: TRect): TBuildingArray;
    Function GetAllHeroesOfSameTypeInRect(Const h: THero; r: TRect): THeroArray;
    Function GetAllHeroesOfOwnerRect(Owner: Integer; r: TRect): THeroArray;
    Procedure SetBuildingStage(x, y, Stage: integer);
    Procedure SetHeroToLevel(HeroIndex, Level: Integer);
    Procedure ResetAllUpdateBuildings;
    Procedure CreateDamageClassTextures;
    Function WaveOppList(WaveNum: integer): String;
{$ENDIF}
    Procedure SecureWaveOpponent(MinWaveCount, MinOpponentCount: Integer);
    Function CheckForErrors(ShowWarnings: Boolean): TCheckResult;
    Function GetListOfUnusedOpponents(): TStringList;
    Procedure DeleteUnusedOpponents();
    Procedure Pause(value: Boolean); // An und ab schalten der Pause
    Function RemoveBuilding(x, y, Owner: integer): Boolean;
    Procedure deletePlacement(x, y: integer);
    Procedure addPlacement(x, y: integer; ObjName: String; Stage: integer);
    Procedure addBuyable(Item: String; Kind: TBuyAbleKind; Wave, Count: integer);
    Procedure delBuyable(Item: String; Wave, Count: integer);
    Procedure updateBuyable(Item: String; Wave, Count: integer);
    Function delOpponent(Opp: String): Boolean;
    Function delBuilding(Geb: String): Boolean;
    Function delHero(Hero: String): Boolean;
    Procedure DelOppInWave(WaveNum, OppNum: integer);

    (*
     * Berechnet die Wegpunktfelder, muss mindestens 1 mal for dem Aufruf von CalcOpponentPaths aufgerufen werden !
     *)
    Procedure CalcWaypointFields();
    (* Berechnet für alle Wegpunkte die Richtungen in welche die Gegner Laufen wollen
     * Wenn ein Wegpunkt nicht mehr von Start bzw. Ziel Erreichbar ist. Dann wird
     * False Zurückgegeben
     *)
    Function CalcOpponentPaths: Boolean;
{$IFDEF Server}
    Function ForceBuildingsBuildReady: TMemoryStream;
    Function ForceHeroesReady: TMemoryStream; // Die helden sind entweder Fertig gebaut, oder halten an !

    Procedure AddOpponentObject(Const obj: TOpponent; Owner: integer);
    Procedure MoveAllOpponents(Const UpdateEvent: TUpdateEvent);
    Procedure HandleAllBuildings;
    Procedure HandleAllHeroes;
    Procedure HandleAllBullets(Const UpdateEvent: TUpdateEvent);
    Procedure Start(); // Wird durch HandleStartRound aufgerufen
    Procedure GetMovingObjectsState(Const Stream: TSTream);
    Function SaveGameingData(Const Stream: TStream): Boolean;
    // Sucht für ein Gebäude oder ein fliegendes Bullet ein neues Zielobjekt
    Function GetTarget(Const position: TVector2; Const Range: Single;
      Const Power: TPower; Const Slowdown: TSlowDown; Const Earn: integer; Const Strategy: TBuildingStrategy; Const PreverAir: Boolean;
      Const Owner: tctd_mapopbject;
      Const SkipOP: TOpponent
      (* Ein opponent, der auf keinen Fall ausgewählt werden darf.
       * z.B.
       * Das Gebäude hat einen Splash Slowdown schaden/ dann kann es alle Einheiten angreifen die Lebenspunkte werden erst gar nicht mehr geprüft.
       * Genau in diesem Fall hat nun das Geb den Opp gekillt und sucht sich nun einen neuen Gegner, weil noch schaden zu verteilen ist.
       * Gettarget liefert dann den Opponent zurück, welcher gerade gekillt wurde, dieser wird aber Freigegeben => Dangling Reference
       *)
      ): TOpponent;
    Procedure ChangeBuildingStrategy(x, y, Owner: integer; Strategy: TBuildingStrategy; PreverAir: Boolean);
    Procedure ChangeHeroStrategy(HeroIndex, Owner: integer; Strategy: TBuildingStrategy; PreverAir: Boolean);
    Procedure SetheroTarget(HeroIndex, Owner: integer; x, y: Single);
    (*
     * Alles zum Arbeiten mit der Highscore Engine
     *)
    Procedure AddHighScore(aPlayerName: String; aTimeStamp: TDateTime; aPoints,
      aPlaytimeinS: int64; aRound, aDifficulty, PlayerCount, LivesLost: integer);
    Procedure ClearHighScore();
    Procedure GetHighscoreAndRating(Const Stream: TStream);

    Procedure AddRating(Rating: integer); // [1..5]
    (*
     * CreateDifficulty: 0 = easy, 1 = medium, 2 = hard
     * result = '' -> Alles gut, sonst Fehlermeldung
     *)
    Function AddRandomWave(CreateDifficulty: integer): String;
{$ENDIF}
    Procedure LoadGameingData(Const Stream: TStream);
    Procedure ResetAllBuyedObjects;
    Procedure ResetAllMovingObjects;
    Function AddBuilding(x, y: integer; Const building: TBuilding): Boolean;
    Function AddHero(x, y: integer; Const hero: THero): Boolean;
    Procedure CloneWave(SourceWaveNum: integer); // 0 - basiert, Clont SourceWaveNum und fügt sie als "letzte" wave hinten an.
    Procedure ExChangeWaves(w1, w2: integer);
    Procedure CreateMovableObjectList(Wave: integer); // Wird zu jeder neuen Runde aufgerufen und übernimmt auch das Init

    // TODO: Wenn alle Attribute mal "Sauber" implementiert sind, dann kann Change wieder Private werden.!
    Procedure Change(ResetHighScore: Boolean); // Wird jedes mal aufgerufen wenn die Karte "Verändert" wird
  End;

Function MapTypeToInt(v: TMapType): integer;
Function IntToMapType(v: integer): TMapType;

Function JitterCoord(index, ItemCount: integer): TVector2;

Function CompareBuyAble(Const a, b: TBuyAble): integer;

Implementation

Uses Graphics, math, LCLIntf, FileUtil, LazFileUtils
  , ufifo, DateUtils
  ;

Function JitterCoord(index, ItemCount: integer): TVector2;
Var
  ItemsPerRow, ItemsPerCol, x, y: integer;
Begin
{$IFDEF Client}
  SpawnGrid := round(MapBlockSize * 0.6);
{$ENDIF}
  ItemsPerRow := trunc(sqrt(2 * ItemCount));
  ItemsPerCol := ceil(sqrt(0.5 * ItemCount));
  x := index Mod ItemsPerRow;
  y := index Div ItemsPerRow;
  result := v2(x * SpawnGrid - (((ItemsPerRow - 1) * SpawnGrid) / 2), y * SpawnGrid - (((ItemsPerCol - 1) * SpawnGrid) / 2));
End;

Function IntToMapType(v: integer): TMapType;
Begin
  result := mtUnknown;
  Case v Of
    0: result := mtSingle;
    1: result := mtCoop;
    2: result := mtSingleMaze;
    3: result := mtCoopMaze;
  End;
End;

Function MapTypeToInt(v: TMapType): integer;
Begin
  result := -1;
  Case v Of
    mtSingle: result := 0;
    mtCoop: result := 1;
    mtSingleMaze: result := 2;
    mtCoopMaze: result := 3;
  End;
End;

Function CompareBuyAble(Const a, b: TBuyAble): integer;
Begin
  If a.WaveNum = b.WaveNum Then Begin
    result := CompareStr(lowercase(a.Item), lowercase(b.Item));
  End
  Else Begin
    result := a.WaveNum - b.WaveNum;
  End;
End;

{$IFDEF Client}

Procedure Quad(w, h: Single; col: integer; Grid: boolean);
Var
  c: TColor;
Begin
  Case col Of
    Walkable: c := Begehbar_col;
    Buildable: c := Bebaubar_col;
    Walkable + Buildable: c := Beides_Col
  Else
    c := Nichts_Col;
  End;
  glcolor(c);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(w, 0);
  glVertex2f(w, h);
  glVertex2f(0, h);
  glend;
  If Grid Then Begin
    glPushMatrix;
    glTranslatef(0, 0, ctd_EPSILON);
    glColor(Grid_color);
    glBegin(GL_LINE_LOOP);
    glVertex2f(0, 0);
    glVertex2f(w, 0);
    glVertex2f(w, h);
    glVertex2f(0, h);
    glend;
    glPopMatrix;
  End;
End;
{$ENDIF}

Operator = (a, v: TBuyAble): Boolean;
Begin
  result :=
    (a.Count = v.Count) And
    (a.Item = v.Item) And
    (a.WaveNum = v.WaveNum);
End;

{ TIniHighscoreEngine }

Function TIniHighscoreEngine.getPlayerCatCount: integer;
Begin
  result := length(fDataSets);
End;

Function TIniHighscoreEngine.CalcHighscoreHash: integer;
  Function HashOfString(Const Data: String): Integer;
  Var
    i, h: Integer;
  Begin
    result := 0;
    For i := 1 To length(Data) Div 4 Do Begin
      h :=
        ord(Data[(i - 1) * 4 + 1]) Or
      (ord(Data[(i - 1) * 4 + 2]) Shl 8) Or
      (ord(Data[(i - 1) * 4 + 3]) Shl 16) Or
      (ord(Data[(i - 1) * 4 + 4]) Shl 24);
      result := result Xor h;
    End;
    // Die Letzten 1 bis 3 Bytes
    h := 0;
    For i := (length(data) Div 4) * 4 To length(data) Do Begin
      h := (h Shl 8) Or ord(data[i]);
    End;
    result := result Xor h;
  End;

  Function HashOfTHighScoreDataSet(Const DataSet: THighScoreDataSet): integer;
  Begin
    // Keine Ahnung, was das Ding berechnet und ob das Sicher ist, aber es
    // Reduziert THighScoreDataSet auf einen 32-Bit Integer ;)
    result := 0;
    result := result Xor HashOfString(DataSet.Name);
    result := result Xor HashOfString(FormatDateTime('YYYY.MM.DD HH:NN:SS', DataSet.TimeStamp));

    result := result Xor (DataSet.Points And $FFFFFFFF);
    result := result Xor ((DataSet.Points Shr 32) And $FFFFFFFF);

    result := result Xor (DataSet.PlayTimeinS And $FFFFFFFF);
    result := result Xor ((DataSet.PlayTimeinS Shr 32) And $FFFFFFFF);
    result := result Xor DataSet.Rounds;
    result := result Xor DataSet.Difficulty;
    result := result Xor DataSet.LivesLost;
  End;

Var
  i, j: integer;
Begin
  result := 0;
  For i := 0 To high(fDataSets) Do Begin
    For j := 0 To high(fDataSets[i].DataSets) Do Begin
      result := result Xor HashOfTHighScoreDataSet(fDataSets[i].DataSets[j]);
    End;
  End;
End;

Function TIniHighscoreEngine.PlayerCat(Index: integer): TPlayerHighscore;
Begin
  result := fDataSets[index];
End;

Constructor TIniHighscoreEngine.Create;
Begin
  Inherited Create;
  fDataSets := Nil;
End;

Destructor TIniHighscoreEngine.Destroy;
Begin
  Clear;
End;

Procedure TIniHighscoreEngine.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(fDataSets) Do Begin
    setlength(fDataSets[i].DataSets, 0);
  End;
  setlength(fDataSets, 0);
End;

Procedure TIniHighscoreEngine.AddHighScore(aPlayerName: String;
  aTimeStamp: TDateTime; aPoints, aPlaytimeinS: int64; aRound, aDifficulty,
  PlayerCount, aLivesLost: integer);
Var
  index, i: integer;
Begin
  // 1. Suchen ob es die "Anzahl Spieler" schon gibt, wenn nicht Anlegen
  index := -1;
  For i := 0 To high(fDataSets) Do Begin
    If fDataSets[i].PlayerCount = PlayerCount Then Begin
      index := i;
      break;
    End;
  End;
  If index = -1 Then Begin
    SetLength(fDataSets, high(fDataSets) + 2);
    index := high(fDataSets);
    fDataSets[index].PlayerCount := PlayerCount;
    fDataSets[index].DataSets := Nil;
  End;
  // 2. Anfügen des Datensatzes
  setlength(fDataSets[index].DataSets, high(fDataSets[index].DataSets) + 2);
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].Name := aPlayerName;
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].TimeStamp := aTimeStamp;
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].Points := aPoints;
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].PlayTimeinS := aPlaytimeinS;
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].Rounds := aRound;
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].Difficulty := aDifficulty;
  fDataSets[index].DataSets[high(fDataSets[index].DataSets)].LivesLost := aLivesLost;
  // TODO: 3. Sortieren nach Spieleranzahl Aufsteigend
End;

Procedure TIniHighscoreEngine.SaveToStream(Const Stream: TStream);
Var
  i, j: integer;
Begin
  i := Length(fDataSets);
  stream.Write(i, SizeOf(i));
  For i := 0 To high(fDataSets) Do Begin
    j := fDataSets[i].PlayerCount;
    stream.Write(j, SizeOf(j));
    j := length(fDataSets[i].DataSets);
    stream.Write(j, SizeOf(j));
    For j := 0 To high(fDataSets[i].DataSets) Do Begin
      stream.WriteAnsiString(fDataSets[i].DataSets[j].Name);
      stream.write(fDataSets[i].DataSets[j].TimeStamp, sizeof(fDataSets[i].DataSets[j].TimeStamp));
      stream.write(fDataSets[i].DataSets[j].Points, sizeof(fDataSets[i].DataSets[j].Points));
      stream.write(fDataSets[i].DataSets[j].PlayTimeinS, sizeof(fDataSets[i].DataSets[j].PlayTimeinS));
      stream.write(fDataSets[i].DataSets[j].Rounds, sizeof(fDataSets[i].DataSets[j].Rounds));
      stream.write(fDataSets[i].DataSets[j].Difficulty, sizeof(fDataSets[i].DataSets[j].Difficulty));
      stream.write(fDataSets[i].DataSets[j].LivesLost, sizeof(fDataSets[i].DataSets[j].LivesLost));
    End;
  End;
End;

Procedure TIniHighscoreEngine.LoadFromStream(Const Stream: TStream);
Var
  i, j: integer;
Begin
  Clear;
  i := 0;
  stream.Read(i, SizeOf(i));
  setlength(fDataSets, i);
  For i := 0 To high(fDataSets) Do Begin
    j := 0;
    stream.Read(j, SizeOf(j));
    fDataSets[i].PlayerCount := j;
    j := 0;
    stream.Read(j, SizeOf(j));
    setlength(fDataSets[i].DataSets, j);
    For j := 0 To high(fDataSets[i].DataSets) Do Begin
      fDataSets[i].DataSets[j].Name := stream.ReadAnsiString();
      stream.Read(fDataSets[i].DataSets[j].TimeStamp, sizeof(fDataSets[i].DataSets[j].TimeStamp));
      stream.Read(fDataSets[i].DataSets[j].Points, sizeof(fDataSets[i].DataSets[j].Points));
      stream.Read(fDataSets[i].DataSets[j].PlayTimeinS, sizeof(fDataSets[i].DataSets[j].PlayTimeinS));
      stream.Read(fDataSets[i].DataSets[j].Rounds, sizeof(fDataSets[i].DataSets[j].Rounds));
      stream.Read(fDataSets[i].DataSets[j].Difficulty, sizeof(fDataSets[i].DataSets[j].Difficulty));
      stream.Read(fDataSets[i].DataSets[j].LivesLost, sizeof(fDataSets[i].DataSets[j].LivesLost));
    End;
  End;
End;

Procedure TIniHighscoreEngine.LoadFromIniFile(Const Ini: tIniFile);
Var
  hash, i, j: Integer;
  s, pn: String;
Begin
  Clear;
  setlength(fDataSets, ini.ReadInteger('HighScore', 'PlayerCats', 0));
  For i := 0 To high(fDataSets) Do Begin
    fDataSets[i].PlayerCount := ini.ReadInteger('Highscore', 'PlayerCat' + inttostr(i), -1);
    setlength(fDataSets[i].DataSets, ini.ReadInteger('HighScore', 'PlayerCat' + inttostr(i) + 'Count', 0));
    For j := 0 To high(fDataSets[i].DataSets) Do Begin
      Pn := 'PlayerCat' + inttostr(i) + '_' + inttostr(j);
      fDataSets[i].DataSets[j].Name := ini.ReadString('HighScore', pn + 'Name', '');
      s := ini.ReadString('HighScore', pn + 'Time', '');
      Try
        fDataSets[i].DataSets[j].TimeStamp := ScanDateTime('YYYY.MM.DD HH:NN:SS', s);
      Except
        fDataSets[i].DataSets[j].TimeStamp := now();
      End;
      fDataSets[i].DataSets[j].Points := ini.ReadInt64('HighScore', pn + 'Points', 0);
      fDataSets[i].DataSets[j].PlayTimeinS := ini.ReadInt64('HighScore', pn + 'Duration', 0);
      fDataSets[i].DataSets[j].Rounds := ini.ReadInteger('HighScore', pn + 'Rounds', 0);
      fDataSets[i].DataSets[j].Difficulty := ini.ReadInteger('HighScore', pn + 'Difficulty', 0);
      fDataSets[i].DataSets[j].LivesLost := ini.ReadInteger('HighScore', pn + 'LivesLost', 0);
    End;
  End;
  hash := CalcHighscoreHash();
  If hash <> ini.ReadInteger('HighScore', 'Hash', hash + 1) Then Begin
    log('Highscores are not accepted, reset stats.', llWarning);
    Clear;
  End;
End;

Procedure TIniHighscoreEngine.SaveToIniFile(Const Ini: tIniFile);
Var
  i, j: Integer;
  pn: String;
Begin
  // Die Section Highscore komplett löschen, damit die Altlasten entsorgt werden..
  ini.EraseSection('HighScore');
  // Neu Schreiben der Aktuellen Daten
  ini.WriteInteger('HighScore', 'PlayerCats', length(fDataSets));
  For i := 0 To high(fDataSets) Do Begin
    ini.WriteInteger('Highscore', 'PlayerCat' + inttostr(i), fDataSets[i].PlayerCount);
    ini.WriteInteger('HighScore', 'PlayerCat' + inttostr(i) + 'Count', length(fDataSets[i].DataSets));
    For j := 0 To high(fDataSets[i].DataSets) Do Begin
      Pn := 'PlayerCat' + inttostr(i) + '_' + inttostr(j);
      ini.WriteString('HighScore', pn + 'Name', fDataSets[i].DataSets[j].Name);
      ini.WriteString('HighScore', pn + 'Time', FormatDateTime('YYYY.MM.DD HH:NN:SS', fDataSets[i].DataSets[j].TimeStamp));
      ini.WriteInt64('HighScore', pn + 'Points', fDataSets[i].DataSets[j].Points);
      ini.WriteInt64('HighScore', pn + 'Duration', fDataSets[i].DataSets[j].PlayTimeinS);
      ini.WriteInteger('HighScore', pn + 'Rounds', fDataSets[i].DataSets[j].Rounds);
      ini.WriteInteger('HighScore', pn + 'Difficulty', fDataSets[i].DataSets[j].Difficulty);
      ini.WriteInteger('HighScore', pn + 'LivesLost', fDataSets[i].DataSets[j].LivesLost);
    End;
  End;
  ini.WriteInteger('HighScore', 'Hash', CalcHighscoreHash());
End;

{ TMap }

Constructor TMap.Create;
Begin
  Inherited create;
  fTerrain := Nil;
  FBulletIndexes := Nil;
  fDC1Tex := '';
  fdc2Tex := '';
  fdc3Tex := '';
  fdc4Tex := '';
{$IFDEF Client}
  fOpenGLDC1Tex.Image := 0;
  fOpenGLDC2Tex.Image := 0;
  fOpenGLDC3Tex.Image := 0;
  fOpenGLDC4Tex.Image := 0;
  FIndexMapper := Nil;
  fRenderBullets := Nil;
  fRenderOpponents := Nil;
{$ELSE}
  fHighscoreEngine := TIniHighscoreEngine.create;
  fHeroes := Nil;
{$ENDIF}
  Clear;
End;

Destructor TMap.Destroy;
Begin
  Clear;
{$IFDEF Server}
  fHighscoreEngine.free;
{$ENDIF}
  Inherited Destroy;
End;

Function TMap.GetHeight: integer;
Begin
  result := 0;
  If assigned(fTerrain) Then
    result := length(fTerrain[0]);
End;

Procedure TMap.FixBuyableSorting;

  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    h, p: TBuyAble;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := fBuyAbles[Trunc((li + re) / 2)]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareBuyAble(fBuyAbles[l], p) < 0 Do
          inc(l);
        While CompareBuyAble(fBuyAbles[r], p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := fBuyAbles[l];
          fBuyAbles[l] := fBuyAbles[r];
          fBuyAbles[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  quick(0, High(fBuyAbles));
End;

Procedure TMap.CalcWaypointFields;
Var
  cnt: integer;

  Procedure PushField(x, y, player, Waypoint: integer);
  Begin
    If fTerrain[x, y].AlreadyChecked Then exit;
    fTerrain[x, y].AlreadyChecked := true;
    Waypoints[PLayer, Waypoint].Field[cnt] := Point(x, y);
    inc(cnt);
    If (x > 0) And fTerrain[x - 1, y].WArea Then PushField(x - 1, y, player, Waypoint);
    If (x < Width - 1) And fTerrain[x + 1, y].WArea Then PushField(x + 1, y, player, Waypoint);
    If (y > 0) And fTerrain[x, y - 1].WArea Then PushField(x, y - 1, player, Waypoint);
    If (y < Height - 1) And fTerrain[x, y + 1].WArea Then PushField(x, y + 1, player, Waypoint);
  End;
Var
  y: Integer;
  x: Integer;
  wp: Integer;
  PLayer: Integer;
Begin
  // Für Alle Wegpunkte werden nun die Field Listen Berechnet (Unabhängig von Bebaut, begehbar und all dem Schnickschnack
  For PLayer := 0 To high(Waypoints) Do Begin
    For wp := 0 To high(Waypoints[PLayer]) Do Begin
      // Den Wegpunkt als Fläche betrachten und von ihm aus "fluten" mit 0
      For x := 0 To Width - 1 Do Begin
        For y := 0 To Height - 1 Do Begin
          fTerrain[x, y].AlreadyChecked := false;
        End;
      End;
      setlength(Waypoints[PLayer, wp].Field, Width * Height);
      cnt := 0;
      PushField(Waypoints[PLayer, wp].Point.x, Waypoints[PLayer, wp].Point.Y, PLayer, wp);
      setlength(Waypoints[PLayer, wp].Field, cnt);
    End;
  End;
End;

Function TMap.GetLives(Index: integer): integer;
Begin
  result := fLives[Index];
End;

{$IFDEF Client}

Function TMap.getHeroCount: integer;
Begin
  result := length(fHeroes);
End;

Function TMap.getOpenGLDamageClassTex(index: integer): TGraphikItem;
Begin
  result.Image := 0; // -- Invalid
  Case Index Of
    0: result := fOpenGLDC1Tex;
    1: result := fOpenGLDC2Tex;
    2: result := fOpenGLDC3Tex;
    3: result := fOpenGLDC4Tex;
  End;
End;
{$ENDIF}

Function TMap.OpponentsEmittedInWave(Wave: integer): integer;
Var
  j: integer;
Begin
  result := 0;
  If (wave >= 0) And (wave <= high(Waves)) Then Begin
    For j := 0 To high(Waves[wave].Opponents) Do Begin
      result := result + waves[wave].Opponents[j].Count;
    End;
  End;
End;

Function TMap.GetWidth: integer;
Begin
  result := length(fTerrain);
End;

Function TMap.getBuyAble(index: integer): TBuyAble;
Begin
  Log('TMap.getBuyAble', llTrace);
  If (index >= 0) And (Index <= high(fBuyAbles)) Then Begin
    result := fBuyAbles[index];
  End
  Else Begin
    log('Index out of range.', llFatal);
    LogLeave;
    halt(0);
  End;
  LogLeave;
End;

Function TMap.GetBuyAblesCount: integer;
Begin
  result := high(fBuyAbles) + 1;
End;

Procedure TMap.SetBackTex(AValue: String);
Begin
  log('TMap.SetBackTex = ' + AValue, llTrace);
  If fBackTex = AValue Then Begin
    LogLeave;
    Exit;
  End;
  Change(false);
  // die Alte Karte Löschen
  If (FBackTex <> '') And FileExistsUTF8(MapFolder + MapName + PathDelim + FBackTex) Then Begin
    If Not DeleteFileUTF8(MapFolder + MapName + PathDelim + FBackTex) Then Begin
      log('could not delete old backtex : ' + FBackTex, llError);
    End;
  End;

{$IFDEF Client}
  // Löschen der Alten Textur
  fOpenGLBackTex := OpenGL_GraphikEngine.FindItem(MapFolder + MapName + PathDelim + FBackTex, false);
  If fOpenGLBackTex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLBackTex);
  End;
  fOpenGLBackTex.Image := 0;

  // Laden einer neuen Textur
  If (AValue <> '') And FileExists(MapFolder + MapName + PathDelim + AValue) Then Begin
    fOpenGLBackTex := OpenGL_GraphikEngine.LoadGraphikItem(MapFolder + MapName + PathDelim + AValue, smClamp);
  End
  Else Begin
    log('Could not find : ' + AValue, llError);
  End;
{$ENDIF}
  fBackTex := AValue;
  LogLeave;
End;

Procedure TMap.SetDC1Tex(AValue: String);
Begin
  // TODO: fehlende Implementierung
  If fDC1Tex = AValue Then exit;
  fDC1Tex := AValue;
End;

Procedure TMap.SetDC2Tex(AValue: String);
Begin
  If fDC2Tex = AValue Then exit;

  fDC2Tex := AValue;
End;

Procedure TMap.SetDC3Tex(AValue: String);
Begin
  If fDC3Tex = AValue Then exit;

  fDC3Tex := AValue;
End;

Procedure TMap.SetDC4Tex(AValue: String);
Begin
  If fDC4Tex = AValue Then exit;

  fDC4Tex := AValue;
End;

Procedure TMap.SetDescription(AValue: String);
Begin
  If fDescription = AValue Then Exit;
  fDescription := AValue;
  Change(false);
End;

Procedure TMap.SetLives(Index: integer; AValue: integer);
Begin
  fLives[Index] := AValue;
  Change(true);
End;

Procedure TMap.SetMapType(AValue: TMapType);
Begin
  If fMapType = AValue Then Exit;
  fMapType := AValue;
  Change(true);
End;

Procedure TMap.SetMaxPlayer(AValue: integer);
Begin
  If fMaxPlayer = AValue Then Exit;
  fMaxPlayer := AValue;
  Change(false);
End;

Procedure TMap.Clear;
Var
  Player, wp: Integer;
  i: Integer;
Begin
  ResetAllMovingObjects;
  EditTerrain := false;
{$IFDEF Client}
  If fFTerrainBackTex <> 0 Then Begin
    (*
     * Wenn die Backtex keine Graphik ist dann wurde sie "dynamisch" erstellt und muss auch wieder dynamisch gelöscht werden.
     *)
    If Not OpenGL_GraphikEngine.RemoveGraphik(fFTerrainBackTex) Then Begin
      glDeleteTextures(1, @fFTerrainBackTex);
    End;
  End;
  fFTerrainBackTex := 0;

  If fOpenGLDC1Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC1Tex);
  End;
  fOpenGLDC1Tex.Image := 0;

  If fOpenGLDC2Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC2Tex);
  End;
  fOpenGLDC2Tex.Image := 0;

  If fOpenGLDC3Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC3Tex);
  End;
  fOpenGLDC3Tex.Image := 0;

  If fOpenGLDC4Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC4Tex);
  End;
  fOpenGLDC4Tex.Image := 0;
{$ENDIF}
  For i := 0 To high(Waves) Do Begin
    setlength(waves[i].Opponents, 0)
  End;
  setlength(Waves, 0);
  For i := 0 To high(Waypoints) Do Begin
    setlength(Waypoints[i], 0);
  End;
  setlength(Waypoints, 0);
  For i := 0 To high(fTerrain) Do
    setlength(fTerrain[i], 0);
  For i := 0 To high(fPlacements) Do Begin
    fPlacements[i].Free;
    fPlacements[i] := Nil;
  End;
  setlength(fPlacements, 0);
  setlength(fTerrain, 0);
  ResetAllBuyedObjects; // fTerrain ist ja schon nil ;)
  Description := '';
  MapType := mtUnknown;
  FBackTex := '';
  ViewWaypoints := -1;
  For Player := 0 To high(Waypoints) Do Begin
    For wp := 0 To high(Waypoints[Player]) Do Begin
      setlength(Waypoints[Player, wp].Field, 0);
    End;
  End;
  Change(true);
  fChanged := false;
End;

Procedure TMap.Load(Const MapName_: String);
Var
  ini: tIniFile;
  s: String;
  w, h, j, i: Integer;
  c: Char;
Begin
  log('TMap.Load : ' + MapName_, lltrace);
  Clear;
  MapName := MapName_;
  s := MapFolder + MapName + PathDelim + MapRootName;
  ini := TIniFile.Create(s);
  // Die Algemeinen Daten
  s := trim(ini.ReadString('map', 'size', ''));
  If s = '' Then exit; // Keine Map Datei
  w := strtoint(copy(s, 1, pos('x', s) - 1));
  h := strtoint(copy(s, pos('x', s) + 1, length(s)));
  setlength(fTerrain, w, h);
  For j := 0 To Height - 1 Do Begin
    s := ini.ReadString('data', 'row' + IntToStr(j), '');
    For i := 0 To Width - 1 Do Begin
      If i + 1 <= length(s) Then Begin
        c := s[i + 1];
      End
      Else Begin
        c := '0';
      End;
      fTerrain[i, j].data := strtoint(c);
      fTerrain[i, j].WArea := false;
      If fTerrain[i, j].data >= 4 Then Begin
        fTerrain[i, j].WArea := true;
        fTerrain[i, j].data := fTerrain[i, j].data - 4;
      End;
      fTerrain[i, j].Blocked := false;
      fTerrain[i, j].DynBlocked := false;
    End;
  End;
  //  name := ini.ReadString('map', 'name', '');
  Description := ini.ReadString('map', 'description', '');
  FBackTex := ini.readString('map', 'background', '');
  fDC1Tex := ini.ReadString('map', 'dc1', '');
  fDC2Tex := ini.ReadString('map', 'dc2', '');
  fDC3Tex := ini.ReadString('map', 'dc3', '');
  fDC4Tex := ini.ReadString('map', 'dc4', '');
  MaxPlayer := ini.ReadInteger('map', 'maxplayer', 4);
  Lives[0] := ini.ReadInteger('map', 'lives0', 50);
  Lives[1] := ini.ReadInteger('map', 'lives1', 25);
  Lives[2] := ini.ReadInteger('map', 'lives2', 10);
  MapType := IntToMapType(ini.ReadInteger('map', 'type', 0));

  // Die Wegpunkte
  setlength(Waypoints, MaxPlayer);
  For i := 0 To high(Waypoints) Do Begin
    j := ini.ReadInteger('waypoint' + inttostr(i), 'count', 0);
    setlength(Waypoints[i], j);
    For j := 0 To high(Waypoints[i]) Do Begin
      s := trim(ini.ReadString('waypoint' + inttostr(i), 'pos' + inttostr(j), '0x0'));
      w := strtoint(copy(s, 1, pos('x', s) - 1));
      h := strtoint(copy(s, pos('x', s) + 1, length(s)));
      Waypoints[i, j].Point := point(w, h);
      Waypoints[i, j].Field := Nil;
    End;
  End;

  // Kaufbare Objekte
  setlength(fBuyAbles, ini.ReadInteger('buyables', 'count', 0));
  For i := 0 To high(fBuyAbles) Do Begin
    fBuyAbles[i].Item := ini.ReadString('buyables', 'filename' + IntToStr(i), '');
    fBuyAbles[i].Count := ini.Readinteger('buyables', 'count' + IntToStr(i), 0);
    fBuyAbles[i].WaveNum := ini.Readinteger('buyables', 'wave' + IntToStr(i), 1);
    Case lowercase(ini.ReadString('buyables', 'kind' + inttostr(i), 'building')) Of
      'building': fBuyAbles[i].Kind := bkBuilding;
      'hero': fBuyAbles[i].Kind := bkHero;
    Else Begin
        Raise exception.create('Error, invalid buyablekind: ' + ini.ReadString('buyables', 'kind' + inttostr(i), 'building'));
      End;
    End;
  End;

  // Die Placements
  For i := 0 To high(fPlacements) Do Begin
    fPlacements[i].Free;
    fPlacements[i] := Nil;
  End;
  setlength(fPlacements, ini.ReadInteger('placements', 'count', 0));
  For i := 0 To high(fPlacements) Do Begin
    s := MapFolder + MapName + PathDelim + ini.ReadString('placements', 'placeobj' + inttostr(i), '');
    Case FilenameToType(s) Of
      moBuilding: Begin
          fPlacements[i] := TBuilding.create();
        End;
      moOpponent: Begin
          fPlacements[i] := TOpponent.create();
        End
    Else Begin
        log('Error unknown placement object : ' + s, llfatal);
        logleave;
        exit;
      End;
    End;
    fPlacements[i].LoadFromFile(s);
    fPlacements[i].Position := StringToPos(ini.ReadString('placements', 'placepos' + inttostr(i), '0x0'));
    fPlacements[i].SetStage(ini.ReadInteger('placements', 'placestage' + inttostr(i), 0));
    If fPlacements[i] Is TBuilding Then Begin
      TBuilding(fPlacements[i]).Owner := -1;
    End;
    If fPlacements[i] Is TOpponent Then Begin
      fPlacements[i].Width := (TOpponent(fPlacements[i]).SizeX);
      fPlacements[i].Height := (TOpponent(fPlacements[i]).Sizey);
      // TOpponent(fPlacements[i]).Identifier := PLacementIdentiferOffset + i; -- Braucht nicht gemacht werden, da SortPlacementByYCoordinate das noch mal ändert und dann neu schreibt.
    End;
  End;
  // Die Waves
  setlength(Waves, ini.ReadInteger('map', 'waves', 1));
  For i := 0 To high(Waves) Do Begin
    waves[i].ChashOnStart := ini.ReadInteger('wave' + inttostr(i), 'cashonstart', 0);
    waves[i].WaveHint := ini.ReadString('wave' + inttostr(i), 'hint', '');
    setlength(waves[i].Opponents, ini.ReadInteger('wave' + inttostr(i), 'opponents', 1));
    For j := 0 To high(waves[i].Opponents) Do Begin
      waves[i].Opponents[j].opponent := ini.ReadString('wave' + inttostr(i), 'opponent_filename' + inttostr(j), '');
      waves[i].Opponents[j].Count := ini.Readinteger('wave' + inttostr(i), 'opponent_count' + inttostr(j), 10);
      waves[i].Opponents[j].refund := ini.Readinteger('wave' + inttostr(i), 'opponent_refund' + inttostr(j), 1);
      waves[i].Opponents[j].UnitsPerSpawn := ini.Readinteger('wave' + inttostr(i), 'unitsperspawn' + inttostr(j), 1);
      waves[i].Opponents[j].SpawnDelay := ini.Readinteger('wave' + inttostr(i), 'spawndelay' + inttostr(j), 2500);
      waves[i].Opponents[j].Spawndelta := ini.Readinteger('wave' + inttostr(i), 'spawndelta' + inttostr(j), 1000);
    End;
  End;
{$IFDEF Server}
  fHighscoreEngine.LoadFromIniFile(ini);
  fRatingCount := ini.ReadInteger('Rating', 'Count', 0);
  fRatingSum := ini.ReadInteger('Rating', 'Sum', 0);
{$ENDIF}
  ini.free;
{$IFDEF Client}
  If FileExists(MapFolder + MapName + PathDelim + FBackTex) And (FBackTex <> '') Then Begin
    fOpenGLBackTex := OpenGL_GraphikEngine.LoadGraphikItem(MapFolder + MapName + PathDelim + FBackTex, smClamp);
  End
  Else Begin
    fOpenGLBackTex.Image := 0;
  End;
  RefreshBackTex;
  CreateDamageClassTextures();
{$ENDIF}
  UpdatePlacemantBlocked;
  SortPlacementByYCoordinate;
  CalcWaypointFields;
  logleave;
End;

Procedure TMap.Save(Const MapName_: String);
Var
  ini: TIniFile;
  j, i: Integer;
  s: String;
  sl: TStringList;
Begin
  // Todo : Das fChanged muss noch Vollständig integriert werden, dann kann es auch ausgewertet werden
//  if fChanged then begin
  MapName := MapName_;
  s := MapFolder + MapName + PathDelim + MapRootName;
  ini := TIniFile.Create(s);
  ini.CacheUpdates := true;
  // Löschen des gesammten Datei inhaltes, ist etwas Umständlich
  sl := TStringList.Create;
  ini.ReadSections(sl);
  For i := 0 To sl.Count - 1 Do Begin
    ini.EraseSection(sl[i]);
  End;
  sl.free;
  // Die Generellen Einstellungen
  ini.WriteString('map', 'size', format('%dx%d', [Width, Height]));
  //  ini.WriteString('map', 'name', name);
  ini.WriteString('map', 'description', Description);
  ini.WriteString('map', 'background', FBackTex);
  ini.writeString('map', 'dc1', fDC1Tex);
  ini.writeString('map', 'dc2', fdc2Tex);
  ini.writeString('map', 'dc3', fdc3Tex);
  ini.writeString('map', 'dc4', fdc4Tex);
  ini.WriteInteger('map', 'maxplayer', MaxPlayer);
  ini.WriteInteger('map', 'lives0', Lives[0]);
  ini.WriteInteger('map', 'lives1', Lives[1]);
  ini.WriteInteger('map', 'lives2', Lives[2]);
  ini.WriteInteger('map', 'type', MapTypeToInt(MapType));

  // Speichern der Level Daten
  For j := 0 To Height - 1 Do Begin
    setlength(s, Width);
    For i := 0 To Width - 1 Do Begin
      (*
       * 0..3 = Begehbar ...
       * 4    = Waypoint Area
       *)
      s[i + 1] := inttostr(fTerrain[i, j].data + 4 * ord(fTerrain[i, j].WArea))[1];
    End;
    ini.WriteString('data', 'row' + IntToStr(j), s);
  End;

  // Speichern der Wegpunkt Daten
  For i := 0 To high(Waypoints) Do Begin
    ini.WriteInteger('waypoint' + inttostr(i), 'count', high(Waypoints[i]) + 1);
    For j := 0 To high(Waypoints[i]) Do Begin
      ini.WriteString('waypoint' + inttostr(i), 'pos' + inttostr(j), format('%dx%d', [Waypoints[i, j].Point.x, Waypoints[i, j].Point.y]));
    End;
  End;

  // Speichern der Placements
  ini.WriteInteger('placements', 'count', high(fPlacements) + 1);
  For i := 0 To high(fPlacements) Do Begin
    ini.WriteString('placements', 'placeobj' + inttostr(i), ExtractFileName(fPlacements[i].Filename));
    ini.WriteString('placements', 'placepos' + inttostr(i), format('%dx%d', [round(fPlacements[i].Position.x), round(fPlacements[i].Position.y)]));
    If fPlacements[i] Is TBuilding Then
      ini.Writeinteger('placements', 'placestage' + inttostr(i), TBuilding(fPlacements[i]).Stage);
  End;
  // Kaufbare Objekte
  ini.WriteInteger('buyables', 'count', high(fBuyAbles) + 1);
  For i := 0 To high(fBuyAbles) Do Begin
    ini.WriteString('buyables', 'filename' + IntToStr(i), fBuyAbles[i].Item);
    ini.Writeinteger('buyables', 'count' + IntToStr(i), fBuyAbles[i].Count);
    ini.Writeinteger('buyables', 'wave' + IntToStr(i), fBuyAbles[i].WaveNum);
    Case fBuyAbles[i].Kind Of
      bkBuilding: ini.WriteString('buyables', 'kind' + IntToStr(i), 'building');
      bkHero: ini.WriteString('buyables', 'kind' + IntToStr(i), 'hero');
    Else Begin
        Raise Exception.Create('TMap.Save: missing implementation.');
      End;
    End;
  End;

  // Die Waves
  ini.writeInteger('map', 'waves', high(Waves) + 1);
  For i := 0 To high(Waves) Do Begin
    ini.WriteInteger('wave' + inttostr(i), 'cashonstart', waves[i].ChashOnStart);
    ini.WriteInteger('wave' + inttostr(i), 'opponents', high(waves[i].Opponents) + 1);
    ini.WriteString('wave' + inttostr(i), 'hint', waves[i].WaveHint);
    For j := 0 To high(waves[i].Opponents) Do Begin
      ini.WriteString('wave' + inttostr(i), 'opponent_filename' + inttostr(j), waves[i].Opponents[j].opponent);
      ini.Writeinteger('wave' + inttostr(i), 'opponent_count' + inttostr(j), waves[i].Opponents[j].Count);
      ini.Writeinteger('wave' + inttostr(i), 'opponent_refund' + inttostr(j), waves[i].Opponents[j].refund);
      ini.Writeinteger('wave' + inttostr(i), 'unitsperspawn' + inttostr(j), waves[i].Opponents[j].UnitsPerSpawn);
      ini.Writeinteger('wave' + inttostr(i), 'spawndelay' + inttostr(j), waves[i].Opponents[j].SpawnDelay);
      ini.Writeinteger('wave' + inttostr(i), 'spawndelta' + inttostr(j), waves[i].Opponents[j].Spawndelta);
    End;
  End;
{$IFDEF Server}
  fHighscoreEngine.SaveToIniFile(ini);
  ini.WriteInteger('Rating', 'Count', fRatingCount);
  ini.WriteInteger('Rating', 'Sum', fRatingSum);
{$ENDIF}
  ini.UpdateFile;
  ini.free;
  //  end;
  fChanged := false;
End;

Procedure TMap.Resize(NewWidth, NewHeight: integer);
Var
  ow, oh: integer;
  i: Integer;
  j: Integer;
Begin
  If (NewHeight <> Height) Or (NewWidth <> Width) Then Begin
    Change(true);
    ow := Width;
    oh := Height;
    setlength(fTerrain, NewWidth, NewHeight);
    For i := ow To NewWidth - 1 Do Begin
      For j := oh To NewHeight - 1 Do Begin
        fTerrain[i, j].data := Walkable Or Buildable;
        fTerrain[i, j].WArea := false;
        fTerrain[i, j].blocked := false;
      End;
    End;
    For j := oh To NewHeight - 1 Do Begin
      For i := ow To NewWidth - 1 Do Begin
        fTerrain[i, j].data := Walkable Or Buildable;
        fTerrain[i, j].blocked := false;
        fTerrain[i, j].WArea := false;
      End;
    End;
{$IFDEF Client}
    RefreshBackTex;
{$ENDIF}
  End;
End;

Function TMap.PlacementAtCoord(x, y: integer): tctd_mapopbject;
Var
  i: Integer;
Begin
  result := Nil;
  // Todo: um das hier "Korrekter" zu machen müsste man ggf zwischen TBuildung und TOpponent unterscheiden, so gehts aber erst mal.
  For i := 0 To high(fPlacements) Do Begin
    If (fPlacements[i].Position.x - ceil(fPlacements[i].Width) <= x - 1) And (fPlacements[i].Position.x >= x - 1) And
      (fPlacements[i].Position.y - ceil(fPlacements[i].Height) < y) And (fPlacements[i].Position.y >= y) Then Begin
      result := fPlacements[i];
      exit;
    End;
  End;
End;

{$IFDEF Server}

Function TMap.ForceBuildingsBuildReady: TMemoryStream;
Var
  x, y, s, i: Integer;
Begin
  result := TMemoryStream.Create;
  For i := 0 To high(Fbuildings) Do Begin
    If Fbuildings[i].ForceIncStage(true) Then Begin
      // Sicherstellen, dass am Ende alle Gebs genau diesen Stage haben
      x := round(Fbuildings[i].Position.x);
      y := round(Fbuildings[i].Position.y);
      s := Fbuildings[i].Stage;
      result.Write(x, SizeOf(x));
      result.Write(y, SizeOf(y));
      result.Write(s, SizeOf(s));
    End;
  End;
  log('Updated Buildings in last wave : ' + inttostr(result.Size Div (3 * sizeof(integer))), lldebug);
End;

Function TMap.ForceHeroesReady: TMemoryStream;
Var
  s, i: Integer;
Begin
  result := TMemoryStream.Create;
  For i := 0 To high(fHeroes) Do Begin
    fHeroes[i].TargetPos := v2(-1, -1); // Egal wie alle Helden Soppen das laufen
    If fHeroes[i].ForceBuilded() Then Begin
      // Sicherstellen, dass am Ende alle Gebs genau diesen Stage haben
      s := fHeroes[i].Level;
      result.Write(i, SizeOf(i));
      result.Write(s, SizeOf(s));
    End;
  End;
  log('Updated Heroes in last wave : ' + inttostr(result.Size Div (3 * sizeof(integer))), lldebug);
End;
{$ENDIF}

Procedure TMap.Change(ResetHighScore: Boolean);
Begin
  fChanged := true;
{$IFDEF Server}
  If ResetHighScore Then Begin
    fHighscoreEngine.Clear;
    ResetRating;
  End;
{$ENDIF}
End;

Procedure TMap.SortPlacementByYCoordinate;
  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: Single;
    h: tctd_mapopbject;
  Begin
    If Li < Re Then Begin
      p := fPlacements[Trunc((li + re) / 2)].Position.y; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While fPlacements[l].Position.y > p Do
          inc(l);
        While fPlacements[r].Position.y < p Do
          dec(r);
        If L <= R Then Begin
          h := fPlacements[l];
          fPlacements[l] := fPlacements[r];
          fPlacements[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Var
  i: integer;
Begin
  Quick(0, high(fPlacements));
  // Wieder Reparieren der Placement Identifier, da diese Array Positionsabhängig sind.
  For i := 0 To high(fPlacements) Do Begin
    If fPlacements[i] Is TOpponent Then Begin
      TOpponent(fPlacements[i]).Identifier := PLacementIdentiferOffset + i;
    End;
  End;
End;

Function TMap.getfield(x, y: integer): integer;
Begin
  If (x >= 0) And (y >= 0) And (x <= high(fTerrain)) And (y <= high(fTerrain[x])) Then Begin
    result := fTerrain[x, y].data;
  End
  Else Begin
    result := 0; // -- Einfach Nichts
  End;
End;

Function TMap.GetListOfAllUsedFiles(IgnoreFile: String): TStringList;
Var
  ap: String;

  Procedure AddOppFiles(aOpp: String);
  Var
    i: integer;
    opp: TOpponent;
    sl: TStringList;
  Begin
    If (aOpp = IgnoreFile) Or (trim(aOpp) = '') Then exit;
    If pos(ap + aopp, result.text) <> 0 Then exit;
    result.add(ap + aOpp);
    opp := TOpponent.create();
    opp.LoadFromFile(ap + aOpp);
    sl := opp.ListOfImages();
    For i := 0 To sl.count - 1 Do Begin
      result.add(ap + sl[i]);
    End;
    sl.free;
    opp.free;
  End;

  Procedure AddGebFiles(aGeb: String);
  Var
    geb: TBuilding;
    sl: TStringList;
    i: integer;
  Begin
    If (aGeb = IgnoreFile) Or (trim(aGeb) = '') Then exit;
    If pos(ap + aGeb, result.text) <> 0 Then exit;
    result.add(ap + aGeb);
    geb := TBuilding.create();
    geb.LoadFromFile(ap + aGeb);
    sl := geb.ListOfImages();
    For i := 0 To sl.count - 1 Do Begin
      result.add(ap + sl[i]);
    End;
    sl.free;
    geb.free;
  End;

  Procedure AddHeroFiles(aHero: String);
  Var
    h: THero;
    sl: TStringList;
    i: integer;
  Begin
    If (aHero = IgnoreFile) Or (trim(aHero) = '') Then exit;
    If pos(ap + aHero, result.text) <> 0 Then exit;
    result.add(ap + aHero);
    h := THero.create();
    h.LoadFromFile(ap + aHero);
    sl := h.ListOfImages();
    For i := 0 To sl.count - 1 Do Begin
      result.add(ap + sl[i]);
    End;
    sl.free;
    h.free;
  End;

Var
  i, j: Integer;
Begin
  ap := MapFolder + MapName + PathDelim;
  result := TStringList.Create;
  result.Duplicates := dupIgnore;
  // Opponents
  For i := 0 To high(Waves) Do Begin
    For j := 0 To high(waves[i].Opponents) Do Begin
      AddOppFiles(waves[i].Opponents[j].opponent);
    End;
  End;
  // Buildings
  For i := 0 To high(fBuyAbles) Do Begin
    Case fBuyAbles[i].Kind Of
      bkBuilding: AddGebFiles(fBuyAbles[i].Item);
      bkHero: AddHeroFiles(fBuyAbles[i].Item);
    Else Begin
        Raise exception.create('TMap.GetListOfAllUsedFiles: missing implementation.');
      End;
    End;
  End;
  // Placements
  For i := 0 To high(fPlacements) Do Begin
    Case FilenameToType(ap + extractfilename(fPlacements[i].Filename)) Of
      moBuilding: Begin
          AddGebFiles(ExtractFileName(fPlacements[i].Filename));
        End;
      moOpponent: Begin
          AddOppFiles(ExtractFileName(fPlacements[i].Filename));
        End;
    End;
  End;
  // Backtex
  If (FBackTex <> IgnoreFile) And (FBackTex <> '') Then Begin
    result.add(ap + FBackTex);
  End;
End;

Function TMap.GetListOfUnusedOpponents: TStringList;
Var
  ap: String; // Der Pfad der Anwendung in welchem Gearbeitet werden darf.
  sl1, sl2: TStringList;
  i, j: Integer;

Begin
  result := TStringList.Create;
  ap := MapFolder + MapName + PathDelim;

  sl1 := findallfiles(ap, '*.opp', false); // Alle Gegner die im Verzeichnis sind
  sl2 := TStringList.Create; // Sammeln aller Gegner die es auf der Karte gibt
  sl2.Duplicates := dupIgnore;
  For i := 0 To high(Waves) Do Begin
    For j := 0 To high(Waves[i].Opponents) Do Begin
      sl2.Add(Waves[i].Opponents[j].opponent);
    End;
  End;

  For i := 0 To sl1.Count - 1 Do Begin
    If pos(ExtractFileName(sl1[i]), sl2.Text) = 0 Then Begin
      result.add(sl1[i])
    End;
  End;
  sl1.free;
  sl2.free;
End;

Procedure TMap.DeleteUnusedOpponents();
Var
  sl: TStringList;
  i: Integer;
Begin
  sl := GetListOfUnusedOpponents();
  For i := 0 To sl.Count - 1 Do Begin
    delOpponent(ExtractFileName(sl[i]));
  End;
  sl.free;
End;

Function TMap.CoordIsBuildable(x, y: integer; Const building: TBuilding
  ): Boolean;
Var
{$IFDEF Server}
  xx, yy,
{$ENDIF}
  i, j: Integer;
{$IFDEF Client}
  sc: Boolean;
  bblocked: Array Of Array Of Boolean;
{$ENDIF}
Begin
  result := true;
  // Sind alle überdekten Pixel Bebaubar ?
  For i := 0 To round(building.Width) - 1 Do Begin
    For j := 0 To round(building.Height) - 1 Do Begin
      If (getfield(x + i, y - j) And Buildable) = 0 Then Begin
        result := false;
        exit;
      End;
      If (x + i >= 0) And
        (x + i <= high(fTerrain)) And
        (y - j >= 0) And
        (y - j <= high(fTerrain[x])) And
        ((fTerrain[x + i, y - j].blocked
{$IFDEF Client}
        // Da der Client alle Opponents Rendert, merkt er sich deren Überdeckung dort auch gleich
        // Das macht das Zusätzliche Prüfen hier natürlich einfach ;)
        ) Or (fTerrain[x + i, y - j].DynBlocked
{$ENDIF}
        )) Then Begin
        result := false;
        exit;
      End;
    End;
  End;
  // Wird auch nichts von Gegnern blockiert ?
{$IFDEF Client}
  If (Not assigned(FIndexMapper)) And assigned(fRenderOpponents) Then Begin
    log('Tmap.Render : uninitialzed FIndexMapper, deleting all opponents.', llCritical);
    setlength(fRenderOpponents, 0);
  End;
  // Der Client muss nun zusätzlich noch prüfen ob der Server den Bau Ablehnt, weil dann die Gegner nicht mehr ins Ziel kommen
  // Dieser Code ist entnommen aus TMap.AddBuilding
  sc := true; // Merken ob überhaupt irgend ein Begehbares Feld Gelöscht wird
  // Löschen und Bakup der Begehbarkeit
  bblocked := Nil;
  setlength(bblocked, round(building.Width), round(building.Height));
  For i := 0 To round(building.Width) - 1 Do
    For j := 0 To round(building.Height) - 1 Do Begin
      If (fTerrain[x + i, y - j].data And Walkable) <> 0 Then sc := false; // Jop, mindestens ein Feld wird nachher nicht mehr begehbar sein
      bblocked[i, j] := fTerrain[x + i, y - j].blocked;
      fTerrain[x + i, y - j].blocked := true;
    End;
  // Neue Wegpfade Berechnen
  If Not sc Then Begin // Ist True, wenn kein Fehld seine Begehbarkeit ändert, dann brauchen wir auch keine neuen Wege berechnen
    result := CalcOpponentPaths();
  End;
  // Die Bebaurbarkeit muss auf jeden Fall wieder hergestellt werden !!
  For i := 0 To round(building.Width) - 1 Do
    For j := 0 To round(building.Height) - 1 Do Begin
      fTerrain[x + i, y - j].blocked := bblocked[i, j];
    End;
{$ENDIF}
{$IFDEF Server}
  For i := 0 To high(fOpponents) Do Begin
    xx := round(fOpponents[i].Obj.Position.x) Div MapBlockSize;
    yy := round(fOpponents[i].Obj.Position.y) Div MapBlockSize;
    If (Not fOpponents[i].Obj.Canfly) And // Nur Bodeneinheiten dürfen blockieren
    (
      (xx >= x) And (xx < x + building.Width) And
      (yy > y - building.Height) And (yy <= y)
      ) Then Begin
      result := false;
      exit;
    End;
  End;
{$ENDIF}
End;

Function TMap.CoordIsBuildable2(x, y: integer): Boolean;
Begin
  result := true;
  If (getfield(x, y) And Buildable) = 0 Then Begin
    result := false;
    exit;
  End;
  If (x >= 0) And
    (x <= high(fTerrain)) And
    (y >= 0) And
    (y <= high(fTerrain[x])) And
    ((fTerrain[x, y].blocked
{$IFDEF Client}
    // Da der Client alle Opponents Rendert, merkt er sich deren Überdeckung dort auch gleich
    // Das macht das Zusätzliche Prüfen hier natürlich einfach ;)
    ) Or (fTerrain[x, y].DynBlocked
{$ENDIF}
    )) Then Begin
    result := false;
    exit;
  End;
End;

Function TMap.CoordIsWalkAble(x, y: integer): Boolean;
Begin
  result := false;
  If (x >= 0) And (x <= high(fterrain)) And
    (y >= 0) And (y <= high(fterrain[x])) Then Begin
    // Prüfen auf Begehbarkeit durch Karte
    If ((fterrain[x, y].data And Walkable) = Walkable) Then
      result := true;
    If fterrain[x, y].blocked Then result := false; // Hier Steht ein Gebäude
  End;
End;

Function TMap.getFieldHeight(x, y, Player, WayPoint: integer): integer;
Begin
  result := Field_unreached;
  If (x >= 0) And (x <= high(fterrain)) And
    (y >= 0) And (y <= high(fterrain[x])) And
    (player >= 0) And (player <= high(fterrain[x, y].WaypointHeightMap)) And
    (WayPoint >= 0) And (Waypoint <= high(fterrain[x, y].WaypointHeightMap[player])) Then
    result := fterrain[x, y].WaypointHeightMap[player, WayPoint];
End;

Procedure TMap.UpdatePlacemantBlocked;
Var
  i, x, y: Integer;
Begin
  For i := 0 To high(fPlacements) Do Begin
    For x := 0 To ceil(fPlacements[i].Width) - 1 Do
      For y := 0 To ceil(fPlacements[i].Height) - 1 Do Begin
        fTerrain[x + round(fPlacements[i].Position.x), y + round(fPlacements[i].Position.y)].Blocked := true;
      End;
  End;
End;

Procedure TMap.SortBuildingsByYCoordinate;
  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: Single;
    h: TBuilding;
  Begin
    If Li < Re Then Begin
      p := Fbuildings[Trunc((li + re) / 2)].Position.y; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While Fbuildings[l].Position.y > p Do
          inc(l);
        While Fbuildings[r].Position.y < p Do
          dec(r);
        If L <= R Then Begin
          h := Fbuildings[l];
          Fbuildings[l] := Fbuildings[r];
          Fbuildings[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  Quick(0, high(Fbuildings));
End;

{$IFDEF Server}

Procedure TMap.KillOpponent(Const Opponent: TOpponent);
Var
  i: Integer;
  j: Integer;
  tg: TOpponent;
Begin
  // Suchen aller Bullets, die nun ihr Target verlieren
  For i := high(FBullets) Downto 0 Do
    If FBullets[i].Target = Opponent Then Begin
      tg := GetTarget(
        fBullets[i].position, fBullets[i].Splash,
        FBullets[i].Power, FBullets[i].SlowDown, FBullets[i].Earn, FBullets[i].Strategy, FBullets[i].PreverAir,
        FBullets[i].Owner, Opponent
        );
      If assigned(tg) Then Begin
        FBullets[i].Target := tg;
      End
      Else Begin
        // Der Bullet geht auch, nichts mehr zum anwisieren
        FBullets[i].free;
        For j := i To high(FBullets) - 1 Do
          FBullets[j] := FBullets[j + 1];
        SetLength(FBullets, high(FBullets));
      End;
    End;
  // Nun den Opponent platt machen
  For i := 0 To high(fOpponents) Do Begin
    If fOpponents[i].Obj = Opponent Then Begin
      fOpponents[i].Obj.free;
      For j := i To high(fOpponents) - 1 Do
        fOpponents[j] := fOpponents[j + 1];
      setlength(fOpponents, high(fOpponents));
      break;
    End;
  End;
End;

{$ENDIF}

Procedure TMap.ResetAllBuyedObjects;
Var
  i: Integer;
  j: Integer;
Begin
  For i := 0 To high(Fbuildings) Do Begin
    Fbuildings[i].Free;
  End;
  setlength(Fbuildings, 0);
  For i := 0 To high(fHeroes) Do Begin
    fHeroes[i].Free;
  End;
  setlength(fHeroes, 0);
  For i := 0 To high(fTerrain) Do Begin
    For j := 0 To high(fTerrain[i]) Do Begin
      fTerrain[i, j].blocked := false;
    End;
  End;
End;

Procedure TMap.ResetAllMovingObjects;
Var
  i: Integer;
Begin
{$IFDEF Server}
  For i := 0 To high(FBullets) Do Begin
    FBullets[i].Free;
  End;
  setlength(FBullets, 0);
  For i := 0 To high(fOpponents) Do Begin
    fOpponents[i].Obj.free;
    fOpponents[i].Obj := Nil;
  End;
  SetLength(fOpponents, 0);
{$ENDIF}
{$IFDEF Client}
  setlength(fRenderOpponents, 0);
  setlength(fRenderBullets, 0);
  For i := 0 To high(FIndexMapper) Do Begin
    If assigned(FIndexMapper[i].obj) Then Begin
      FIndexMapper[i].obj.free;
      FIndexMapper[i].obj := Nil;
    End;
  End;
  setlength(FIndexMapper, 0);
  For i := 0 To high(FBulletIndexes) Do Begin
    If assigned(FBulletIndexes[i].Animation) Then FBulletIndexes[i].Animation.free;
  End;
{$ENDIF}
  setlength(FBulletIndexes, 0);
End;

{$IFDEF Client}

Function TMap.getOpponentCount: integer;
Begin
  result := length(fRenderOpponents);
End;
{$ENDIF}

{$IFDEF Server}

Function TMap.getOpponentCount: integer;
Begin
  result := High(fOpponents) + 1;
End;
{$ENDIF}

{$IFDEF Client}

Procedure TMap.UpdateMoveables(Const data: TStream);
Var
  o: TRenderOpponent;
  b: TRenderBullet;
  h: TRenderHero;
  i: Integer;
  u16: uint16;
  u32: uint32;
Begin
  (*
   * Liest aus was unter TMap.GetMovingObjectsState generiert wurde !
   *)
  // 1. Lesen der Beweglichen Objekte
  u16 := 0;
  data.Read(u16, sizeof(u16));
  SetLength(fRenderOpponents, u16);
  For i := 0 To high(fRenderOpponents) Do Begin
    o.Index := 65535;
    data.Read(o, sizeof(o));
    fRenderOpponents[i] := o;
  End;
  // 2. Lesen der Bullets
  u32 := 0;
  data.Read(u32, sizeof(u32));
  SetLength(fRenderBullets, u32);
  For i := 0 To high(fRenderBullets) Do Begin
    b.Index := 65535;
    data.Read(b, sizeof(b));
    fRenderBullets[i] := b;
  End;
  // 3. Die Helden
  u16 := 0;
  data.Read(u16, sizeof(u16));
  If u16 <> length(fHeroes) Then Begin
    Raise exception.create('Oh oh, da ist wohl was schief gegangen...');
  End;
  For i := 0 To high(fHeroes) Do Begin
    h.Position := v2(-1, -1);
    data.Read(h, sizeof(h));
    fHeroes[i].ApplyRenderData(h);
  End;
  //  SetLength(fRenderHeroes, u16);
  //  For i := 0 To high(fRenderHeroes) Do Begin
  //    h.Index := 65535;
  //    data.Read(h, sizeof(h));
  //    fRenderHeroes[i] := h;
  //  End;
End;

Procedure TMap.ClearMoveables;
Begin
  setlength(fRenderOpponents, 0);
  setlength(fRenderBullets, 0);
End;

Function ValueToColor(Value: integer): TColor;
Begin
  Case value Of
    Walkable: result := Begehbar_col;
    Buildable: result := Bebaubar_col;
    Walkable + Buildable: result := Beides_Col
  Else
    result := Nichts_Col;
  End;
End;

Procedure TMap.RefreshBackTex;
Var
  Data: Array Of Array[0..2] Of Byte;
  t, i, j: integer;
  c: TColor;
Begin
  If fFTerrainBackTex = 0 Then Begin
    glGenTextures(1, @fFTerrainBackTex);
  End;
  fOpenGLWidth := Ceilp2(width);
  fOpenGLHeight := Ceilp2(height);
  data := Nil;
  setlength(data, fOpenGLWidth * fOpenGLHeight);
  t := 0;
  For j := 0 To fOpenGLHeight - 1 Do
    For i := 0 To fOpenGLWidth - 1 Do Begin
      c := ValueToColor(fTerrain[min(i, width - 1), min(j, height - 1)].data);
      data[t, 0] := c And $FF;
      data[t, 1] := (c Shr 8) And $FF;
      data[t, 2] := (c Shr 16) And $FF;
      inc(t);
    End;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(gl_texture_2d, fFTerrainBackTex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, gl_RGB, fOpenGLWidth, fOpenGLHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, @Data[0, 0]);
  setlength(data, 0);
End;

Function TMap.GetAllBuildingsSameOfSameType(Const b: TBuilding): TBuildingArray;
Var
  i, c: Integer;
Begin
  result := Nil;
  setlength(result, high(Fbuildings) + 1);
  c := 0;
  For i := 0 To high(Fbuildings) Do Begin
    If (Fbuildings[i].fUpdating.State = usIdleInactive) And (b.Owner = Fbuildings[i].Owner) And (b.Stage = Fbuildings[i].Stage) And (b.Filename = Fbuildings[i].Filename) Then Begin
      result[c] := Fbuildings[i];
      inc(c);
    End;
  End;
  setlength(result, c);
End;

Function TMap.GetAllHeroesOfSameType(Const h: THero): THeroArray;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHeroes) Do Begin
    If (fHeroes[i].Filename = h.Filename) And (h.Owner = fHeroes[i].Owner) Then Begin
      setlength(result, high(result) + 2);
      result[high(Result)] := fHeroes[i];
    End;
  End;
End;

Function TMap.GetAllHeroesOfOwner(Owner: integer): THeroArray;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHeroes) Do Begin
    If fHeroes[i].Owner = Owner Then Begin
      setlength(result, high(result) + 2);
      result[high(Result)] := fHeroes[i];
    End;
  End;
End;

Function TMap.GetAllBuildingsSameOfSameTypeInRect(Const b: TBuilding; r: TRect
  ): TBuildingArray;
Var
  i, c: Integer;
Begin
  result := Nil;
  setlength(result, high(Fbuildings) + 1);
  c := 0;
  For i := 0 To high(Fbuildings) Do Begin
    If (Fbuildings[i].fUpdating.State = usIdleInactive) And (b.Owner = Fbuildings[i].Owner) And (b.Stage = Fbuildings[i].Stage) And (b.Filename = Fbuildings[i].Filename) Then Begin
      If (Fbuildings[i].Position.x >= r.Left) And
        (Fbuildings[i].Position.x <= r.Right) And
        (Fbuildings[i].Position.y >= r.Top) And
        (Fbuildings[i].Position.y <= r.Bottom) Then Begin
        result[c] := Fbuildings[i];
        inc(c);
      End;
    End;
  End;
  setlength(result, c);
End;

Function TMap.GetAllHeroesOfSameTypeInRect(Const h: THero; r: TRect): THeroArray;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHeroes) Do Begin
    // TODO: Da muss noch der Positionsshift mit rein !
    If (fHeroes[i].Filename = h.Filename) And (h.Owner = fHeroes[i].Owner) Then Begin
      If (fHeroes[i].Position.x >= r.Left) And
        (fHeroes[i].Position.x <= r.Right) And
        (fHeroes[i].Position.y >= r.Top) And
        (fHeroes[i].Position.y <= r.Bottom) Then Begin
        setlength(result, high(result) + 2);
        result[high(Result)] := fHeroes[i];
      End;
    End;
  End;
End;

Function TMap.GetAllHeroesOfOwnerRect(Owner: Integer; r: TRect): THeroArray;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fHeroes) Do Begin
    // TODO: Da muss noch der Positionsshift mit rein !
    If (Owner = fHeroes[i].Owner) Then Begin
      If (fHeroes[i].Position.x >= r.Left) And
        (fHeroes[i].Position.x <= r.Right) And
        (fHeroes[i].Position.y >= r.Top) And
        (fHeroes[i].Position.y <= r.Bottom) Then Begin
        setlength(result, high(result) + 2);
        result[high(Result)] := fHeroes[i];
      End;
    End;
  End;
End;

Procedure TMap.SetBuildingStage(x, y, Stage: integer);
Var
  i: integer;
Begin
  For i := 0 To high(Fbuildings) Do Begin
    If (round(Fbuildings[i].Position.x) = x) And
      (round(Fbuildings[i].Position.y) = y) Then Begin
      Fbuildings[i].fUpdating.State := usInProgress;
      Fbuildings[i].fUpdating.FinState := Stage;
      Fbuildings[i].ForceIncStage(false);
      break;
    End;
  End;
End;

Procedure TMap.SetHeroToLevel(HeroIndex, Level: Integer);
Begin
  If (HeroIndex < 0) Or (HeroIndex >= Length(fHeroes)) Then Begin
    Raise Exception.Create('TMap.SetHeroToLevel: Index out of bounds.');
  End;
  fHeroes[HeroIndex].Level := Level;
End;

Procedure TMap.ResetAllUpdateBuildings;
Var
  i: integer;
Begin
  For i := 0 To high(Fbuildings) Do Begin
    Fbuildings[i].ForceIncStage(false);
  End;
End;

Procedure TMap.CreateDamageClassTextures;
Begin
  If fOpenGLDC1Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC1Tex);
  End;
  fOpenGLDC1Tex.Image := 0;
  If fOpenGLDC2Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC2Tex);
  End;
  fOpenGLDC2Tex.Image := 0;
  If fOpenGLDC3Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC3Tex);
  End;
  fOpenGLDC3Tex.Image := 0;
  If fOpenGLDC4Tex.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fOpenGLDC4Tex);
  End;
  fOpenGLDC4Tex.Image := 0;
  If fDC1Tex <> '' Then Begin
    fOpenGLDC1Tex := OpenGL_GraphikEngine.LoadGraphikItem(MapFolder + MapName + PathDelim + fDC1Tex, smClamp);
  End;
  If fDC2Tex <> '' Then Begin
    fOpenGLDC2Tex := OpenGL_GraphikEngine.LoadGraphikItem(MapFolder + MapName + PathDelim + fDC2Tex, smClamp);
  End;
  If fDC3Tex <> '' Then Begin
    fOpenGLDC3Tex := OpenGL_GraphikEngine.LoadGraphikItem(MapFolder + MapName + PathDelim + fDC3Tex, smClamp);
  End;
  If fDC4Tex <> '' Then Begin
    fOpenGLDC4Tex := OpenGL_GraphikEngine.LoadGraphikItem(MapFolder + MapName + PathDelim + fDC4Tex, smClamp);
  End;
End;

Function TMap.WaveOppList(WaveNum: integer): String;
Var
  i: Integer;
Begin
  result := '';
  If (WaveNum >= 0) And (WaveNum <= high(Waves)) Then Begin
    For i := 0 To high(Waves[WaveNum].Opponents) Do Begin
      // Ein Bisschen Einrücken zwecks besserer Lesbarkeit
      result := result + '  ' + ExtractFileNameOnly(Waves[WaveNum].Opponents[i].opponent) + ': ' + inttostr(Waves[WaveNum].Opponents[i].Count) + LineEnding;
    End;
    // Das Result mit CRT aufhört ist absicht, dann ist ein kleiner Abstand zum Wave Hint, so denn einer Definiert ist.
  End;
End;

Procedure TMap.UpdateBackTexCoord(x, y: integer);
Var
  c: TColor;
  data: Array[0..2] Of Byte;
Begin
  c := ValueToColor(fTerrain[x, y].data);
  data[0] := c And $FF;
  data[1] := (c Shr 8) And $FF;
  data[2] := (c Shr 16) And $FF;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(gl_texture_2d, fFTerrainBackTex);
  glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, @data[0]);
End;

Function TMap.UpdateOpponentData(Const op: TOpponent): Boolean;
Var
  i: integer;
Begin
  result := false;
  If op.Identifier >= PLacementIdentiferOffset Then Begin // Der Gegner ist ein Placement, da stimmen die Daten schon
    i := op.Identifier - PLacementIdentiferOffset;
    If (i <= high(fPlacements)) And (fPlacements[i] Is TOpponent) Then Begin
      op.LifePoints := TOpponent(fPlacements[i]).LifePoints;
      op.Position := TOpponent(fPlacements[i]).Position;
      op.SizeX := TOpponent(fPlacements[i]).SizeX;
      op.SizeY := TOpponent(fPlacements[i]).SizeY;
      result := true;
    End;
    exit;
  End;
  For i := 0 To high(fRenderOpponents) Do Begin
    If fRenderOpponents[i].Identifier = op.Identifier Then Begin
      op.LifePoints := fRenderOpponents[i].LifePoints;
      op.Position := fRenderOpponents[i].Position;
      op.SizeX := FIndexMapper[fRenderOpponents[i].Index].SizeX;
      op.Sizey := FIndexMapper[fRenderOpponents[i].Index].Sizey;
      result := true;
      break;
    End;
  End;
End;

Procedure TMap.RenderAbstract(blockw, blockh: Single; Grid,
  HideLifePoints: Boolean; UserIndex: integer);
Var
  i, j: integer;
  k, l: Single;
  b: Boolean;
Begin
  glEnable(GL_DEPTH_TEST);
  glPushMatrix;
  // So Skallieren, dass alles nachfolgende in MAP_BLOCK_WIDTH koordinaten gerendert werden kann.
  glScalef(blockw, blockh, 1);
  // Die Kacheln Tiefe = 0
  If ShowBackTex And (fOpenGLBackTex.Image <> 0) Then Begin
    glColor3f(1, 1, 1);
    glPushMatrix;
    glScalef(MapBlockSize * Width / fOpenGLBackTex.OrigWidth, MapBlockSize * Height / fOpenGLBackTex.OrigHeight, 1); // So Scallieren dass es die Gesamte Karte überdeckt
    RenderQuad(0, 0, fOpenGLBackTex);
    glPopMatrix;
  End;
  If ((Not ShowBackTex) And (fOpenGLBackTex.Image <> 0)) Or (fOpenGLBackTex.Image = 0) Or EditTerrain Then Begin
    // Der Hintergrund
    glPushMatrix;
    (*
     * Im Designmodus gibt es den Fall, dass der Kartenersteller
     * die Feldeigenschaften an den Hintergrund anpassen will, dann
     * nutzen wir ein Alphablending um dies zu unterstützen ;)
     *)
    If ShowBackTex And (fOpenGLBackTex.Image <> 0) Then Begin
      glColor4f(1, 1, 1, 0.5);
      glTranslatef(0, 0, ctd_EPSILON / 2);
      B := glIsEnabled(gl_Blend);
      If Not (b) Then glenable(gl_Blend);
      glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
    End
    Else Begin
      glColor4f(1, 1, 1, 1);
    End;
    k := Width / fOpenGLWidth;
    l := height / fOpenGLHeight;
    glBindTexture(GL_TEXTURE_2D, fFTerrainBackTex);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(0, 0);
    glTexCoord2f(k, 0);
    glVertex2f(width * MapBlockSize, 0);
    glTexCoord2f(k, l);
    glVertex2f(width * MapBlockSize, Height * MapBlockSize);
    glTexCoord2f(0, l);
    glVertex2f(0, Height * MapBlockSize);
    glEnd;
    glPopMatrix;
    If ShowBackTex And (fOpenGLBackTex.Image <> 0) Then Begin
      If Not b Then gldisable(gl_blend);
    End;
  End;
  // Grid ?
  If Grid Then Begin // Tiefe  + Epsilon
    glPushMatrix;
    glBindTexture(GL_TEXTURE_2D, 0);
    glTranslatef(0, 0, ctd_EPSILON);
    glBegin(GL_LINES);
    glColor(Grid_color);
    k := width * MapBlockSize;
    l := Height * MapBlockSize;
    For i := 1 To Width Do Begin
      glVertex2f(i * MapBlockSize, 0);
      glVertex2f(i * MapBlockSize, l);
    End;
    For i := 1 To Height Do Begin
      glVertex2f(0, i * MapBlockSize);
      glVertex2f(k, i * MapBlockSize);
    End;
    glEnd;
    glPopMatrix;
  End;
  // Die Placements
  glTranslatef(0, 0, 2 * ctd_EPSILON);
  For i := 0 To high(fPlacements) Do Begin
    glPushMatrix;
    glTranslatef(MapBlockSize * fPlacements[i].Position.x, MapBlockSize * fPlacements[i].Position.y, 0);
    fPlacements[i].Render(false);
    glPopMatrix;
  End;
  // Reset aller Felder Zwecks markierung durch Bodeneinheiten
  For i := 0 To Width - 1 Do
    For j := 0 To Height - 1 Do
      fTerrain[i, j].DynBlocked := false;

  // Alle Boden Einheiten
  glTranslatef(0, 0, ctd_EPSILON);
  If (Not assigned(FIndexMapper)) And assigned(fRenderOpponents) Then Begin
    log('Tmap.Render : uninitialzed FIndexMapper, deleting all opponents.', llCritical);
    setlength(fRenderOpponents, 0);
  End;
  For i := 0 To high(fRenderOpponents) Do Begin
    If Not FIndexMapper[fRenderOpponents[i].Index].CanFly Then Begin
      glPushMatrix;
      // Der Gegner wird immer Zentriert an seiner Position dargestellt
      k := (MapBlockSize - MapBlockSize * FIndexMapper[fRenderOpponents[i].Index].SizeX) / 2;
      l := (MapBlockSize - MapBlockSize * FIndexMapper[fRenderOpponents[i].Index].Sizey) / 2;
      fTerrain[max(0, min(width - 1, round(fRenderOpponents[i].Position.x))), max(0, min(height - 1, round(fRenderOpponents[i].Position.y)))].DynBlocked := true;
      glTranslatef(fRenderOpponents[i].Position.x * MapBlockSize + k, (fRenderOpponents[i].Position.y) * MapBlockSize - l, 0);
      If assigned(FIndexMapper[fRenderOpponents[i].Index].obj.Animation) Then Begin
        RenderMoveableAnim(
          fRenderOpponents[i].AnimationOffset,
          FIndexMapper[fRenderOpponents[i].Index].obj.Animation,
          fRenderOpponents[i].Angle,
          FIndexMapper[fRenderOpponents[i].Index].SizeX,
          FIndexMapper[fRenderOpponents[i].Index].SizeY,
          min(1, (fRenderOpponents[i].LifePoints[0] + fRenderOpponents[i].LifePoints[1] + fRenderOpponents[i].LifePoints[2] + fRenderOpponents[i].LifePoints[3]) / FIndexMapper[fRenderOpponents[i].Index].TotalLifePoints),
          Not HideLifePoints);
      End
      Else Begin
        RenderMoveableitem(
          FIndexMapper[fRenderOpponents[i].Index].Image,
          -fRenderOpponents[i].Angle,
          FIndexMapper[fRenderOpponents[i].Index].SizeX,
          FIndexMapper[fRenderOpponents[i].Index].SizeY,
          min(1, (fRenderOpponents[i].LifePoints[0] + fRenderOpponents[i].LifePoints[1] + fRenderOpponents[i].LifePoints[2] + fRenderOpponents[i].LifePoints[3]) / FIndexMapper[fRenderOpponents[i].Index].TotalLifePoints),
          Not HideLifePoints);
      End;
      glPopMatrix;
    End;
  End;

  // Alle Gebäude
  glTranslatef(0, 0, ctd_EPSILON);
  For i := 0 To high(Fbuildings) Do Begin
    glPushMatrix;
    glTranslatef(Fbuildings[i].Position.x * MapBlockSize, Fbuildings[i].Position.y * MapBlockSize, 0);
    Fbuildings[i].Render((UserIndex <> -1) And (UserIndex <> Fbuildings[i].Owner));
    glPopMatrix;
  End;

  // Alle Flug Einheiten
  glTranslatef(0, 0, ctd_EPSILON);

  For i := 0 To high(fHeroes) Do Begin
    glPushMatrix;
    glTranslatef(fHeroes[i].Position.x * MapBlockSize, fHeroes[i].Position.y * MapBlockSize, 0);
    fHeroes[i].ShowLifePoints := Not HideLifePoints;
    fHeroes[i].Render((UserIndex <> -1) And (UserIndex <> fHeroes[i].Owner));
    glPopMatrix;
  End;

  For i := 0 To high(fRenderOpponents) Do Begin
    If FIndexMapper[fRenderOpponents[i].Index].CanFly Then Begin
      glPushMatrix;
      // Der Gegner wird immer Zentriert an seiner Position dargestellt
      k := (MapBlockSize - MapBlockSize * FIndexMapper[fRenderOpponents[i].Index].SizeX) / 2;
      l := (MapBlockSize - MapBlockSize * FIndexMapper[fRenderOpponents[i].Index].Sizey) / 2;
      glTranslatef(fRenderOpponents[i].Position.x * MapBlockSize + k, (fRenderOpponents[i].Position.y) * MapBlockSize - l, 0);
      If assigned(FIndexMapper[fRenderOpponents[i].Index].obj.Animation) Then Begin
        RenderMoveableAnim(
          fRenderOpponents[i].AnimationOffset,
          FIndexMapper[fRenderOpponents[i].Index].obj.Animation,
          fRenderOpponents[i].Angle,
          FIndexMapper[fRenderOpponents[i].Index].SizeX,
          FIndexMapper[fRenderOpponents[i].Index].SizeY,
          min(1, (fRenderOpponents[i].LifePoints[0] + fRenderOpponents[i].LifePoints[1] + fRenderOpponents[i].LifePoints[2] + fRenderOpponents[i].LifePoints[3]) / FIndexMapper[fRenderOpponents[i].Index].TotalLifePoints),
          Not HideLifePoints);
      End
      Else Begin
        RenderMoveableItem(
          FIndexMapper[fRenderOpponents[i].Index].Image,
          -fRenderOpponents[i].Angle,
          FIndexMapper[fRenderOpponents[i].Index].SizeX,
          FIndexMapper[fRenderOpponents[i].Index].SizeY,
          min(1, (fRenderOpponents[i].LifePoints[0] + fRenderOpponents[i].LifePoints[1] + fRenderOpponents[i].LifePoints[2] + fRenderOpponents[i].LifePoints[3]) / FIndexMapper[fRenderOpponents[i].Index].TotalLifePoints),
          Not HideLifePoints);
      End;
      glPopMatrix;
    End;
  End;

  glPopMatrix; // -- Pop aus der Skallierten Ansicht
End;

Procedure TMap.Render(sx, sy, x, y: integer; Grid, ShowLifePoints: Boolean;
  UserIndex: integer);
Var
  i, j: Integer;
Begin
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(x - sx, y - sy, ctd_Map_Layer);
  // Das Algemeine Rendern der Karte
  RenderAbstract(1, 1, grid, Not ShowLifePoints, UserIndex);
  // Anfahren Bullet Layer
  glPushMatrix;
  glTranslatef(0, 0, 5 * ctd_Epsilon);
  glColor4f(1, 1, 1, 1);
  // Alle Geschosse Rendern
  If (Not assigned(FBulletIndexes)) And (assigned(fRenderBullets)) Then Begin
    log('Tmap.Render : uninitialzed FBulletIndexes, deleting all bullets.', llCritical);
    setlength(fRenderBullets, 0);
  End;
  For i := 0 To high(fRenderBullets) Do Begin
    If FBulletIndexes[fRenderBullets[i].Index].speed = 0 Then Begin
      glPushMatrix;
      glTranslatef(0, 0, -4 * ctd_Epsilon);
      If assigned(FBulletIndexes[fRenderBullets[i].Index].Animation) Then Begin
        FBulletIndexes[fRenderBullets[i].Index].Animation.AnimationOffset := fRenderBullets[i].AnimationOffset;
        RenderAnim(
          point(round(fRenderBullets[i].position.x * MapBlockSize), round(fRenderBullets[i].position.y * MapBlockSize)),
          round(FBulletIndexes[fRenderBullets[i].Index].Width * MapBlockSize),
          round(FBulletIndexes[fRenderBullets[i].Index].Height * MapBlockSize),
          FBulletIndexes[fRenderBullets[i].Index].Animation, fRenderBullets[i].Angle
          );
      End
      Else Begin
        RenderObj(
          point(round(fRenderBullets[i].position.x * MapBlockSize), round(fRenderBullets[i].position.y * MapBlockSize)),
          round(FBulletIndexes[fRenderBullets[i].Index].Width * MapBlockSize),
          round(FBulletIndexes[fRenderBullets[i].Index].Height * MapBlockSize),
          FBulletIndexes[fRenderBullets[i].Index].Fimage, fRenderBullets[i].Angle);
      End;
      glPopMatrix;
    End
    Else Begin
      If assigned(FBulletIndexes[fRenderBullets[i].Index].Animation) Then Begin
        FBulletIndexes[fRenderBullets[i].Index].Animation.AnimationOffset := fRenderBullets[i].AnimationOffset;
        RenderAnim(
          point(round(fRenderBullets[i].position.x * MapBlockSize), round(fRenderBullets[i].position.y * MapBlockSize)),
          round(FBulletIndexes[fRenderBullets[i].Index].Width * MapBlockSize),
          round(FBulletIndexes[fRenderBullets[i].Index].Height * MapBlockSize),
          FBulletIndexes[fRenderBullets[i].Index].Animation, fRenderBullets[i].Angle
          );
      End
      Else Begin
        RenderObj(
          point(round(fRenderBullets[i].position.x * MapBlockSize), round(fRenderBullets[i].position.y * MapBlockSize)),
          round(FBulletIndexes[fRenderBullets[i].Index].Width * MapBlockSize),
          round(FBulletIndexes[fRenderBullets[i].Index].Height * MapBlockSize),
          FBulletIndexes[fRenderBullets[i].Index].Fimage, fRenderBullets[i].Angle);
      End;
    End;
  End;
  glPopMatrix;
  If ShowWaypoints Then Begin
    glPushMatrix;
    glTranslatef(0, 0, 6 * ctd_Epsilon);
    If ViewWaypoints = -1 Then Begin // Rendern aller Wegpunkte
      For i := 0 To high(Waypoints) Do Begin
        If high(Waypoints[i]) <> -1 Then Begin
          For j := 0 To high(Waypoints[i]) Do Begin
            If j = 0 Then Begin
              glColor4f(1, 1, 1, 1);
              RenderObjItem(point(Waypoints[i, j].Point.x * MapBlockSize + MapBlockSize Div 2, Waypoints[i, j].Point.y * MapBlockSize),
                MapBlockSize, MapBlockSize,
                OpenGL_GraphikEngine.FindItem(PlayerStartPointTex));
              OpenGL_ASCII_Font.Color := clred;
              OpenGL_ASCII_Font.Textout(Waypoints[i, j].Point.x * MapBlockSize + integer(round(MapBlockSize * 1.5)), Waypoints[i, j].Point.y * MapBlockSize + MapBlockSize Div 2 - integer(round(OpenGL_ASCII_Font.TextHeight('P') / 2)), 'P' + inttostr(i + 1));
              glBindTexture(GL_TEXTURE_2D, 0);
              glColor4f(1, 0, 0, 1);
              glbegin(GL_LINE_STRIP);
            End;
            glVertex2f(Waypoints[i, j].Point.x * MapBlockSize + MapBlockSize / 2, Waypoints[i, j].Point.y * MapBlockSize + MapBlockSize / 2);
          End;
          glend;
        End;
      End;
    End
    Else Begin // Rendern der Wegpunkte von Spieler ViewWaypoints
      If (ViewWaypoints >= 0) And (ViewWaypoints <= high(Waypoints)) And (high(Waypoints[ViewWaypoints]) <> -1) Then Begin
        // Die einzelnen Wegpunkte beschriften
        glColor4f(1, 1, 1, 1);
        OpenGL_ASCII_Font.Color := clred;
        For j := 0 To high(Waypoints[ViewWaypoints]) Do Begin
          If j = 0 Then Begin
            RenderObjItem(point(Waypoints[ViewWaypoints, j].Point.x * MapBlockSize + MapBlockSize Div 2, Waypoints[ViewWaypoints, j].Point.y * MapBlockSize),
              MapBlockSize, MapBlockSize,
              OpenGL_GraphikEngine.FindItem(PlayerStartPointTex));
            OpenGL_ASCII_Font.Textout(Waypoints[ViewWaypoints, j].Point.x * MapBlockSize + integer(round(MapBlockSize * 1.5)), Waypoints[ViewWaypoints, j].Point.y * MapBlockSize + MapBlockSize Div 2 - integer(round(OpenGL_ASCII_Font.TextHeight('P') / 2)), 'P' + inttostr(ViewWaypoints + 1));
          End
          Else Begin
            OpenGL_ASCII_Font.Textout(Waypoints[ViewWaypoints, j].Point.x * MapBlockSize + integer(round(MapBlockSize * 1.0)), Waypoints[ViewWaypoints, j].Point.y * MapBlockSize + MapBlockSize Div 2 - integer(round(OpenGL_ASCII_Font.TextHeight('P') / 2)), inttostr(j + 1));
          End;
        End;
        // Die Eigentliche Linie Malen
        glBindTexture(GL_TEXTURE_2D, 0);
        glColor4f(1, 0, 0, 1);
        glbegin(GL_LINE_STRIP);
        For j := 0 To high(Waypoints[ViewWaypoints]) Do Begin
          glVertex2f(Waypoints[ViewWaypoints, j].Point.x * MapBlockSize + MapBlockSize / 2, Waypoints[ViewWaypoints, j].Point.y * MapBlockSize + MapBlockSize / 2);
        End;
        glend;
      End;
    End;
    glPopMatrix;
  End; // -- Ende View Waypoints
  glPopMatrix; // -- Pop aus Map Koordinatensystem
End;

Procedure TMap.RenderPreview(x, y, w, h, sx, sy, mw, mh: integer;
  UserIndex: integer);
Var
  t, l, b, r: Single;
Begin
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(x, y, ctd_Menu_Layer + ctd_Epsilon);
  RenderAbstract(w / ((high(fTerrain) + 1) * MapBlockSize), h / ((high(fTerrain[0]) + 1) * MapBlockSize), false, true, UserIndex);
  // Rendern des Rahmens, welcher Anzeigt welchen Bildauschnitt man gerade sieht
  glPushMatrix;
  // So Skallieren, dass alles nachfolgende in MAP_BLOCK_WIDTH Koordinaten gerendert werden kann.
  glScalef(w / ((high(fTerrain) + 1) * MapBlockSize), h / ((high(fTerrain[0]) + 1) * MapBlockSize), 1);
  glTranslatef(sx, sy, ctd_Epsilon);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  glColor4f(0.25, 0.25, 0.25, 0.5);
  glLineWidth(3);
  glBindTexture(GL_TEXTURE_2D, 0);
  t := 0;
  l := 0;
  b := t + min(((high(fTerrain[0]) + 1) * MapBlockSize), mh);
  r := l + min(((high(fTerrain) + 1) * MapBlockSize), mw);
  glbegin(GL_LINE_LOOP);
  glVertex2f(l, t);
  glVertex2f(r, t);
  glVertex2f(r, b);
  glVertex2f(l, b);
  glend;
  glLineWidth(1);
  glPopMatrix;
  glPopMatrix;
  glDisable(GL_BLEND);
End;

{$ENDIF}

Procedure TMap.SecureWaveOpponent(MinWaveCount, MinOpponentCount: Integer);
Begin
  If length(Waves) < MinWaveCount Then setlength(Waves, MinWaveCount);
  If length(Waves[MinWaveCount - 1].Opponents) < MinOpponentCount Then setlength(Waves[MinWaveCount - 1].Opponents, MinOpponentCount);
End;

Function TMap.CheckForErrors(ShowWarnings: Boolean): TCheckResult;
Var
  warning, // Global, damit Warnungen immer angefügt werden können
  ap: String; // Der Pfad der Anwendung in welchem Gearbeitet werden darf.
  OverAllLivePoints, OverAllGebDamagePoints: integer; // Bildet Bitweise ab, welche Schadensklassen attakiert werden können, und welche "Leben" enthalten.

  Function inttocat(Value: integer): String; // Konvertiert  OverAllLivePoints, OverAllGebDamagePoints -> [x,x,x,x]
  Var
    i: Integer;
  Begin
    // Todo: Das könnte "Intuitiver" werden, ist aber schon besser wie nur die Zahl an zu geben..
    result := '[';
    (*
     * i = 0 = stitch
     *     1 = poison
     *     2 = magic
     *     3 = air
     *)
    For i := 0 To 3 Do Begin
      If (value And (1 Shl i)) <> 0 Then Begin
        result := result + 'X';
      End
      Else Begin
        result := result + 'O';
      End;
      If i <> 3 Then Begin
        result := result + ',';
      End;
    End;
    result := result + ']';
  End;

  Function CheckHero(Const Filename: String): Boolean;
  Var
    s: String;
    h: THero;
    sl: TStringList;
    j: Integer;
  Begin
    // TODO: Alles Relevante prüfen was mit Helden zu tun hat.
    result := true;
    s := ap + Filename;
    If (Not FileExistsUTF8(s)) Or (filename = '') Then Begin
      LogShow(format('Unable to load hero: %s', [s]), llError);
      result := false;
      exit;
    End;
    h := THero.create();
    h.LoadFromFile(s);
    If length(h.name) > 22 Then Begin
      warning := warning + LineEnding + filename + ' - Name to long, will be unreadable in game hints.';
    End;
    If high(h.Levels) = -1 Then Begin
      LogShow(format('No levels definied hero "%s"', [Filename]), llError);
      h.free;
      result := false;
      exit;
    End;
    // hier gehts weiter


    sl := h.ListOfImages();
    For j := 0 To sl.count - 1 Do Begin
      If (Not FileExistsUTF8(ap + sl[j])) Or (trim(sl[j]) = '') Then Begin
        LogShow(format('Unable to load texture "%s" from hero "%s"', [sl[j], Filename]), llError);
        sl.free;
        h.free;
        result := false;
        exit;
      End;
    End;
    sl.free;
    h.free;
  End;

  Function CheckBuilding(Const Filename: String): boolean;
  Var
    b: TBuilding;
    sl: TStringList;
    j: Integer;
    i: Integer;
    s: String;
  Begin
    // Todo : Alles Relevante für ein Gebäude Prüfen, es fehlen noch Sinnvolle Werte für was auch immer ..
    result := True;
    s := ap + Filename;
    If (Not FileExistsUTF8(s)) Or (filename = '') Then Begin
      LogShow(format('Unable to load building: %s', [s]), llError);
      result := false;
      exit;
    End;
    b := TBuilding.create();
    b.LoadFromFile(s);
    If length(b.name) > 22 Then Begin
      warning := warning + LineEnding + filename + ' - Name to long, will be unreadable in game hints.';
    End;

    // Todo : Ist die Breite der Gebäude Descriptions über alle Stages <= 22 Zeichen -> Sonst Warnung Text nicht vollständig lesbar
    // Todo : Ist die Höhe der Gebäude Descriptions über alle Stages > 5 Zeilen -> Warnung Text nicht vollständig lesbar
    If high(b.stages) = -1 Then Begin
      LogShow(format('No stage definied building "%s"', [Filename]), llError);
      b.free;
      result := false;
      exit;
    End;

    For i := 0 To high(b.Stages) Do Begin
      If b.stages[i].BuildTime > 1000 * 60 Then Begin
        warning := warning + LineEnding + filename + ' - Build time stage ' + inttostr(i + 1) + 'is really long > 1min.';
      End;
      If (b.Stages[i].range <> 0) And
        (b.Stages[i].bulletpower[0] = 0) And
        (b.Stages[i].bulletpower[1] = 0) And
        (b.Stages[i].bulletpower[2] = 0) And
        (b.Stages[i].bulletpower[3] = 0) And
        (b.Stages[i].earn = 0) And
        (b.Stages[i].SlowDown.slowdownstatic = 1.0) And
        (b.Stages[i].SlowDown.slowdowndynamic = 1.0) Then Begin
        LogShow(format('range <> 0, but sum bulletpower = 0  and no earn or slowdowning"%s", stage %d', [Filename, i + 1]), llError);
        b.free;
        result := false;
        exit;
      End;
      For j := 0 To 3 Do Begin
        If b.Stages[i].bulletpower[j] > 0 Then Begin
          OverAllGebDamagePoints := OverAllGebDamagePoints Or (1 Shl j);
        End;
        If b.Stages[i].bulletpower[j] < 0 Then Begin
          LogShow(format('bulletpower < 0, "%s", stage %d', [Filename, i + 1]), llError);
          b.free;
          result := false;
          exit;
        End;
      End;
      If (b.Stages[i].range > 0) And (b.Stages[i].bulletspeed = 0) Then Begin
        LogShow(format('bullet is unable to move "%s", stage %d', [Filename, i + 1]), llError);
        b.free;
        result := false;
        exit;
      End;
      If b.Stages[i].earn < 0 Then Begin
        LogShow(format('Earn is negativ "%s", stage %d', [Filename, i + 1]), llError);
        b.free;
        result := false;
        exit;
      End;
      If b.Stages[i].Cost <= 0 Then Begin
        LogShow(format('Cost is invalid "%s", stage %d', [Filename, i + 1]), llError);
        b.free;
        result := false;
        exit;
      End;
      // Todo: Prüfen, dass die Stagewidth immer Gleich ist, und die Height >= height, bei allen anderen Warnungen ausgeben
    End;

    sl := b.ListOfImages();
    For j := 0 To sl.count - 1 Do Begin
      If (Not FileExistsUTF8(ap + sl[j])) Or (trim(sl[j]) = '') Then Begin
        LogShow(format('Unable to load texture "%s" from building "%s"', [sl[j], Filename]), llError);
        sl.free;
        b.free;
        result := false;
        exit;
      End;
    End;
    sl.free;
    b.free;
  End;

  Function CheckOpponent(Const Filename: String): Boolean;
  Var
    op: TOpponent;
    j: integer;
  Begin
    // Todo : Alles Relevante für einen Gegner Prüfen, es fehlen noch Sinnvolle werte für was auch immer ..
    result := true;
    If FileExistsUTF8(ap + Filename) And (filename <> '') Then Begin
      op := TOpponent.create();
      op.LoadFromFile(ap + Filename);
      If Not FileExistsUTF8(ap + op.Image) Then Begin
        LogShow('Unable to load image for opponent: ' + Filename, llError);
        op.free;
        result := false;
        exit;
      End;
      // Todo : Warnung wenn Hintbreite > 22 Zeichen, 5 Zeilen
      If ((op.LifePoints[0] = 0) And (op.LifePoints[1] = 0) And (op.LifePoints[2] = 0) And (op.LifePoints[3] = 0)) Or
        (op.LifePoints[0] < 0) Or (op.LifePoints[1] < 0) Or (op.LifePoints[2] < 0) Or (op.LifePoints[3] < 0) Then Begin
        LogShow('Invalid livepoint settings for : ' + Filename, llError);
        op.free;
        result := false;
        exit;
      End;
      For j := 0 To 3 Do Begin
        If op.lifepoints[j] > 0 Then Begin
          OverAllLivePoints := OverAllLivePoints Or (1 Shl j);
        End;
      End;

      If (op.speed <= 0) Then Begin
        LogShow('Invalid speed settings for : ' + Filename, llError);
        op.free;
        result := false;
        exit;
      End;
      If (op.SizeX <= 0) Or (op.SizeY <= 0) Then Begin
        LogShow('Invalid size settings for : ' + Filename, llError);
        op.free;
        result := false;
        exit;
      End;
      op.free;
    End
    Else Begin
      LogShow('Unable to load opponent: ' + Filename, llError);
      result := false;
      exit;
    End;
  End;
Var
  //  sl: TStringList;
  m, i, j: Integer;
  bool: boolean;
  by: TBuilding;
  bh: THero;
  sl1, sl2: TStringList;
  msg: String;
  oppsPerWave: integer;
Begin
  log('TMap.CheckForErrors', lltrace);
  result.Errors := true;
  result.Warnings := false;
  ap := MapFolder + MapName + PathDelim;

  OverAllLivePoints := 0;
  OverAllGebDamagePoints := 0;

  If (fDC1Tex = '') Or Not (FileExistsUTF8(ap + fDC1Tex)) Then Begin
    LogShow('Error, missing/invalid image for damageclass 1.', llError);
    logleave;
    exit;
  End;
  If (fDC2Tex = '') Or Not (FileExistsUTF8(ap + fDC2Tex)) Then Begin
    LogShow('Error, missing/invalid image for damageclass 2.', llError);
    logleave;
    exit;
  End;
  If (fDC3Tex = '') Or Not (FileExistsUTF8(ap + fDC3Tex)) Then Begin
    LogShow('Error, missing/invalid image for damageclass 3.', llError);
    logleave;
    exit;
  End;
  If (fDC4Tex = '') Or Not (FileExistsUTF8(ap + fDC4Tex)) Then Begin
    LogShow('Error, missing/invalid image for damageclass 4.', llError);
    logleave;
    exit;
  End;

  //  Hier gehts weiter, der Server logt anstatt, dass er logshow macht.

  // Todo : Alles mögliche Kontrollieren
  // Wavecount > 0
  // Buyables in jeder Wave > 0
  // Lives[0..2] alle >  0
  // Alle Waypoints für alle Spieler Gültig und vorhanden
  // Stages der Placements <= High(stages) ?
  // Als Warnung auch Dateien Suchen, welche unnötigerweise im Verzeichniss liegen

  // Jeden Gegner und Jedes Gebäude Prüfen
  // gegner wie Geb. Summe Power mindestens 1
  // Warnung, wenn ein Opp nicht CanFly aber FlyPower hat

  If fMaxPlayer < 1 Then Begin
    LogShow('Error, invalid value for max player count.', llError);
    logleave;
    exit;
  End;

  // Nur Gespeicherte Karten taugen was !
  If fChanged Then Begin
    LogShow('Error, map is not saved yet.', llError);
    logleave;
    exit;
  End;
  // Gibt es mindestens eine Wave ?
  If high(Waves) = -1 Then Begin
    LogShow('No waves defined.', llError);
    logleave;
    exit;
  End;
  // Todo : Sind alle Waves Vollständig und sinnvoll definiert

  // Todo : Prüfen ob alle Erzeugungen der Wavas (Playercount > 1) auch auf begehbaren Koordinaten landen.
  If (Lives[0] <= 0) Or (Lives[1] <= 0) Then Begin
    LogShow('At Least Normal and Easy must have minimul 1 live.', llError);
    logleave;
    exit;
  End;

  // Sind alle Buyables vollständig und Definiert, und kann man mindestens 1 Gebäude auch kaufen ?
  bool := false;
  m := High(Integer);
  For i := 0 To BuyAblesCount - 1 Do Begin
    Case fBuyAbles[i].Kind Of
      bkBuilding: Begin
          If Not CheckBuilding(BuyAbles[i].Item) Then Begin
            LogLeave;
            exit;
          End;
          If BuyAbles[i].WaveNum <= 0 Then Begin
            bool := true;
            by := TBuilding.create();
            by.LoadFromFile(ap + BuyAbles[i].Item);
            m := min(m, by.Stages[0].Cost);
            by.free;
          End;
        End;
      bkHero: Begin
          If Not CheckHero(BuyAbles[i].Item) Then Begin
            LogLeave;
            exit;
          End;
          If BuyAbles[i].WaveNum <= 0 Then Begin
            bool := true;
            bh := THero.create();
            bh.LoadFromFile(ap + BuyAbles[i].Item);
            m := min(m, bh.Cost);
            bh.free;
          End;
        End;
    Else Begin
        Raise exception.create('Missing implementation.');
      End;
    End;
    // Done : Ist jedes Baubare Gebäude auch nur 1 mal in der Liste vertreten ?
    For j := i + 1 To BuyAblesCount - 1 Do Begin
      If BuyAbles[i].Item = BuyAbles[j].Item Then Begin
        LogShow('No doubles allowed in buy list.', llError);
        logleave;
        exit;
      End;
    End;
  End;
  If Not bool Then Begin
    logShow('In wave 0 nothing is buyable.', llError);
    logleave;
    exit;
  End;
  If m > Waves[0].ChashOnStart Then Begin
    logShow('Buyable buildings in wave 0 are to expensive to buy.', llError);
    logleave;
    exit;
  End;

  If FBackTex <> '' Then Begin
    If Not FileExistsUTF8(ap + FBackTex) Then Begin
      LogShow('Unable to load : ' + FBackTex, llError);
      logleave;
      exit;
    End;
  End;

  // Sind alle Verfügbaren Gegner vollständig und gültig definiert  (Speed > 0, Texturen .. )
  For i := 0 To high(Waves) Do Begin
    oppsPerWave := 0;
    If waves[i].ChashOnStart < 0 Then Begin
      Warning := Warning + format('In wave %d, negativ cash value for cash on start player will loose money, minimum value will be 0.', [i + 1]) + LineEnding;
    End;
    For j := 0 To high(Waves[i].Opponents) Do Begin
      oppsPerWave := oppsPerWave + Waves[i].Opponents[j].Count;
      If Waves[i].Opponents[j].Count < Waves[i].Opponents[j].UnitsPerSpawn Then Begin
        Warning := Warning + format('Warning, wave %d opponent %d "Units per spawn" > "Count", this will affect startposition in negative/undefined way.', [i + 1, j + 1]) + LineEnding;
      End;
      If Waves[i].Opponents[j].Count <= 0 Then Begin
        LogShow(format('In wave %d, opponent %d count is invalid.', [i + 1, j + 1]), llError);
        logleave;
        exit;
      End;
      If Waves[i].Opponents[j].refund < 0 Then Begin
        LogShow(format('In wave %d, opponent %d "Cash per unit" has to be >= 0', [i + 1, j + 1]), llError);
        logleave;
        exit;
      End;
      If Waves[i].Opponents[j].UnitsPerSpawn = 0 Then Begin
        LogShow(format('In wave %d, opponent %d "Units per spawn" is zero.  ', [i + 1, j + 1]), llError);
        logleave;
        exit;
      End;
      If Waves[i].Opponents[j].Spawndelta < 100 Then Begin
        Warning := Warning + format('Warning, in wave %d, creep %d spawndelta is < 100ms better increase "UnitsPerSpawn" ', [i + 1, j + 1]) + LineEnding;
      End;
      // Todo : Kann zu keiner Zeit ein Gegner via Spawn Erzeugt werden, welcher dann auf Blockiertem Gelände landed (geht nur für count > 1) ?

      If Not CheckOpponent(Waves[i].Opponents[j].opponent) Then Begin
        logshow('Error occured in wave : ' + IntToStr(i + 1) + ' opp ' + inttostr(j + 1), llInfo);
        logleave;
        exit;
      End;
    End;
    // Prüfung auf Überlauf von op.identifier
    If oppsPerWave * MaxPlayer >= PLacementIdentiferOffset Then Begin
      LogShow(format('In wave %d, if all player play, there where %d opponents spawend, max allowed number is %d', [i + 1, oppsPerWave * MaxPlayer, PLacementIdentiferOffset - 1]), llError);
      logleave;
      exit;
    End;
  End;

  // Prüfen ob auch alle Schadensklassen der Gegner durch irgend ein Gebäude getroffen werden können.
  For i := 0 To 3 Do Begin
    If (OverAllLivePoints And (1 Shl i) <> 0) And
      (OverAllGebDamagePoints And (1 Shl i) = 0) Then Begin
      logshow(format('Error there are creeps with damage classes, that could not be attacked by any buyable building. enegry %s vs damage %s', [inttocat(OverAllLivePoints), inttocat(OverAllGebDamagePoints)]), llerror);
      logleave;
      exit;
    End;
  End;

  // Prüfen der Placements
  For i := 0 To high(fPlacements) Do Begin
    Case FilenameToType(ap + extractfilename(fPlacements[i].Filename)) Of
      moBuilding: Begin
          If Not CheckBuilding(extractfilename(fPlacements[i].Filename)) Then Begin
            logleave;
            exit;
          End;
        End;
      moOpponent: Begin
          If Not CheckOpponent(extractfilename(fPlacements[i].Filename)) Then Begin
            logleave;
            exit;
          End;
        End
        // TODO: hier fehlt der Hero ?
    Else Begin
        LogShow('Unknown placement : ' + extractfilename(fPlacements[i].Filename), llError);
        logleave;
        exit;
      End;
    End;
  End;

  // Sind die Wegpunkte für Alle Spieler Richtig gesetzt ?
  For i := 0 To high(Waypoints) Do Begin
    If high(Waypoints[i]) < 1 Then Begin
      LogShow('Map holds to less waypoints for player ' + inttostr(i + 1), llError);
      logleave;
      exit;
    End;
  End;
  If MaxPlayer <> length(Waypoints) Then Begin
    LogShow(format('Map supports %d player, but holds only waypoints for %d player', [MaxPlayer, length(Waypoints)]), llError);
    logleave;
    exit;
  End;
  // Können die Gegner überhaupt alle Wege richtig beschreiten ?
  CalcWaypointFields();
  If Not CalcOpponentPaths Then Begin
    LogShow('Invalid Pathstructure opponents can''t walk their way.', llError);
    logleave;
    exit;
  End;
  // Todo : Prüfen ob alle Gegner in Allen Waves auch richtig gespawned werde können (damit die Exception in Handle Opponents nicht kommt)
  //        Dazu die Routine "JitterCoord" verwenden

  // TODO: Prüfen ob 2 Wegpunkte ein und des selben Spielers nicht über eine Wegpunktfläche verbunden werden können, wenn ja -> Fehler

  // Prüfen ob alle Gegner die in die Karte integriert wurden auch Verwendet werden.
  // -> Das wird gemacht, aber nicht hier sondern im Editor nach dem Dialog, hier wird lediglich die "Warnung" getriggert
  sl1 := GetListOfUnusedOpponents();
  If sl1.Count <> 0 Then Begin
    If ShowWarnings Then result.Warnings := True;
  End;
  sl1.free;

  // Prüfen ob Gebäude in der Karte Registriert wurden die nicht Kaufbar sind
  (*
   * Das macht so eigentlich gar keinen Sinn, da die Gebäude ja immer kaufbar sind
   *)
  sl1 := findallfiles(ap, '*.geb;*.hero', false); // Alle Gebäude/ Helden die im Verzeichnis sind
  sl2 := TStringList.Create; // Sammeln aller Gebäude die es auf der Karte gibt
  For i := 0 To high(fBuyAbles) Do Begin
    sl2.add(fBuyAbles[i].Item);
  End;
  msg := '';
  For i := 0 To sl1.Count - 1 Do Begin
    If pos(ExtractFileName(sl1[i]), sl2.Text) = 0 Then Begin
      msg := msg + '  ' + ExtractFileName(sl1[i]) + LineEnding;
    End;
  End;
  msg := trim(msg);
  If msg <> '' Then Begin
    warning := warning + LineEnding +
      'The following buildings / heroes are in the map folder but not used by map:' + LineEnding + '  ' + msg;
  End;
  sl1.free;
  sl2.free;
  sl1 := GetListOfAllUsedFiles('');
  For i := 0 To sl1.Count - 1 Do Begin
    If Not FileExistsUTF8(sl1[i]) Then Begin
      LogShow('File does not exist: ' + sl1[i], llError);
      logleave;
      sl1.free;
      exit;
    End;
  End;
  sl1.free;
  // Todo:    nutzen
  // Anzeige von Warnungen, die Karte ist Spielbar, aber evtl. nicht so wie das der Benutzer erwartet.
  If ShowWarnings And (warning <> '') Then Begin
    result.Warnings := True;
    LogShow(warning, llWarning);
  End;
  result.Errors := false;
  logleave;
End;

Function TMap.CalcOpponentPaths: Boolean;
Type
  TDataSet = Record
    pos: TPoint;
    Height: Integer;
  End;

  TDataSetQueue = specialize TBufferedFifo < TDataSet > ;

Var
  q: TDataSetQueue;

  Procedure Push(x, y, player, Waypoint, h: integer);
  Var
    p: TDataSet;
  Begin
    If // Wir sind Innerhalb der Karte
    (x >= 0) And (x <= high(fTerrain)) And (y >= 0) And (y <= high(fTerrain[x])) And
      // Die Koordinate wurde noch nie besucht, oder der Bisherige weg ist Länger als der Aktuell gefundene
    (//(fTerrain[x, y].WaypointHeightMap[player, Waypoint] = field_unreached) Or -- Brauchts nicht mehr, da das ja Größer ist
      (fTerrain[x, y].WaypointHeightMap[player, Waypoint] > h)) Then Begin
      If CoordIsWalkAble(x, y) Then Begin // Wenn die Koordinate Begangen werden darf
        fTerrain[x, y].WaypointHeightMap[player, Waypoint] := h; // Merken, dass es auch mit dieser "Höhe" an die Koordinate geht
        p.pos := point(x, y);
        p.Height := h;
        q.Push(p);
      End;
    End;
  End;

Var
  pp: TDataSet;
  i, c: Integer;
  j: Integer;
  player: Integer;
  wp: Integer;
Begin
  result := true;
  q := TDataSetQueue.create;
  // Alle Höheninformationen Resetten
  For i := 0 To high(fTerrain) Do Begin
    For j := 0 To high(fTerrain[i]) Do Begin
      // Initialisieren, falls noch nicht geschehen
      If high(fTerrain[i, j].WaypointHeightMap) <> high(Waypoints) Then
        setlength(fTerrain[i, j].WaypointHeightMap, high(Waypoints) + 1);
      For player := 0 To high(Waypoints) Do Begin
        If high(fTerrain[i, j].WaypointHeightMap[player]) <> high(Waypoints[player]) Then
          setlength(fTerrain[i, j].WaypointHeightMap[player], high(Waypoints[player]) + 1);
        For wp := 0 To high(Waypoints[player]) Do Begin
          fTerrain[i, j].WaypointHeightMap[player, wp] := field_unreached;
        End;
      End;
    End;
  End;
  // Alle Wegpunkte und Alle Player die Karte "Fluten" lassen
  For player := 0 To high(Waypoints) Do Begin
    For wp := 1 To high(Waypoints[player]) Do Begin // Der Start Wegpunkt wird nie Angelaufen, also muss seine Höhenkarte auch nicht berechnet werden
      // Den Wegpunkt als Fläche betrachten und von ihm aus "fluten" mit 0
      For c := 0 To high(Waypoints[player, wp].Field) Do Begin
        Push(Waypoints[player, wp].Field[c].X, Waypoints[player, wp].Field[c].y, player, wp, 0);
      End;
      While Not q.isempty Do Begin // Fluten so lange bis alles besucht wurde
        pp := q.Pop;
        // Nur Waagrecht und Senkrecht gehen
        Push(pp.pos.x + 1, pp.pos.y, Player, wp, pp.Height + 1);
        Push(pp.pos.x - 1, pp.pos.y, Player, wp, pp.Height + 1);
        Push(pp.pos.x, pp.pos.y + 1, Player, wp, pp.Height + 1);
        Push(pp.pos.x, pp.pos.y - 1, Player, wp, pp.Height + 1);
      End;
      // Prüfen ob alle Player und alle Wegpunkte jeweils alle anderen Wegpunkte Erreichen können
      // Wenn wir von jedem Wegpunkt aus, den Start bzw Endpunkt erreichen können, dann ist klar, dass wir
      // Auch alle anderen Wegpunkte erreichen können
      If
        (FieldHeight[Waypoints[player, 0].Point.x, Waypoints[player, 0].Point.y, player, wp] = field_unreached) // Der Startpunkt wurde nicht erreicht
      Or
        (FieldHeight[Waypoints[player, high(Waypoints[player])].Point.x, Waypoints[player, high(Waypoints[player])].Point.y, player, wp] = field_unreached) {// Der Endpunkt wurde nicht erreicht} Then Begin
        q.free;
        result := false;
        exit;
      End;
    End;
  End;
  q.free;
End;
{$IFDEF Server}

Procedure TMap.AddOpponentObject(Const obj: TOpponent; Owner: integer);
Var
  i: integer;
Begin
  // Den Besitzer merken, für die Livepoint Engine
  obj.Owner := Owner;
  // Die Lebenspunkte berechnen..
  For i := low(obj.LifePoints) To high(obj.LifePoints) Do Begin
    If obj.LifePoints[i] <> 0 Then Begin
      // verhindern, dass durch die Difficult Stufe die Gegner in Summe "0" Lebenspunkte haben.
      obj.LifePoints[i] := max(1, round(obj.LifePoints[i] * obj.LifeFactors[Difficulty]));
    End;
  End;
  //  obj.ShowLifePoints := fShowLifepoints;
  setlength(fOpponents, high(fOpponents) + 2);
  fOpponents[high(fOpponents)].Obj := obj;
  fOpponents[high(fOpponents)].NextWayPoint := 1;
  fOpponents[high(fOpponents)].NextTimeNoDiagWalk := false;
End;

Procedure TMap.MoveAllOpponents(Const UpdateEvent: TUpdateEvent);
Var
  j, i: Integer;
  steigung, wx, wy: integer;
  gx, gy, odir, oh, wh, ah, x, y, xx, yy: integer;
  ox, oy, ll, l, dx, dy: Single;
  SQR_Min_Dist: Single;
  delta: integer; // Der Zeit
  n: int64;
Begin
  If FPausing Then exit;
  // Die Zeit schreitet voran, in Delta steht wieviel seit dem Letzten mal vergangen ist.
  n := GetTick;
  delta := n - fLastOppMoveTime;
  delta := delta * Speedup; // Berücksichtigen eines evtl existierenden Speedups
  fLastOppMoveTime := n;
  // Hier lassen wir die Viecher laufen
  For i := high(fOpponents) Downto 0 Do Begin
    If fOpponents[i].Obj.Canfly Then Begin
      // Fliegende Gegner sind deutlich einfacher ;)
      // 1. Suchen des "kürzesten" Wegpunktes im WegpunktFeld
      wx := Waypoints[fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint].Field[0].x * MapBlockSize;
      wy := Waypoints[fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint].Field[0].y * MapBlockSize;
      ll := sqr(wx - fOpponents[i].Obj.Position.x) + sqr(wy - fOpponents[i].Obj.Position.y);

      For j := 1 To high(Waypoints[fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint].Field) Do Begin
        xx := Waypoints[fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint].Field[j].x * MapBlockSize;
        yy := Waypoints[fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint].Field[j].Y * MapBlockSize;
        l := sqr(xx - fOpponents[i].Obj.Position.x) + sqr(yy - fOpponents[i].Obj.Position.y);
        If l < ll Then Begin
          ll := l;
          wx := xx;
          wy := yy;
        End;
      End;
      gx := wx; // Speichern des Zielwegpunktes zum Erkennen des umschaltens zum nächsten
      gy := wy;
      // 2. Berechnen Delta
      dx := wx - fOpponents[i].Obj.Position.x;
      dy := wy - fOpponents[i].Obj.Position.y;
      // 3. Berechnen der Länge in Pixeln
      l := sqrt(sqr(dx) + sqr(dy));
      If l = 0 Then l := 1;
      // 4. Normieren auf 1 Pixel
      dx := dx / l;
      dy := dy / l;
      // 5. Berechnen der Schaffbaren Wegstrecke, bei der Geschwindigkeit die unser Opponent hat
      ll := fOpponents[i].Obj.GetInGameSpeed * MapBlockSize; // MapBlockSize Pro Sekunde
      ll := ll * delta / 1000; // Pixel in der Vergangenen Zeit.
      // 6. Wenn wir übers Ziel hinausschießen würden (weil der Benutzer den Opponent zu schnell gemacht hat
      If ll > l Then ll := l;
      // 7. Den Wegfortschritt Berechnen
      dx := dx * ll;
      dy := dy * ll;
      // 8. Tatsächlich fliegen
      fOpponents[i].Obj.Position.x := fOpponents[i].Obj.Position.x + dx;
      fOpponents[i].Obj.Position.y := fOpponents[i].Obj.Position.y + dy;
      fOpponents[i].Obj.Direction := round(radtodeg(arctan2(-dy, dx)));
      // Haben wir den Wegpunkt ereicht ?
      x := round(fOpponents[i].Obj.Position.x) Div MapBlockSize;
      y := round(fOpponents[i].Obj.Position.y) Div MapBlockSize;
      // 9. Berechnen der "Reststrecke" zum Ziel
      fOpponents[i].DistanzeToGoal := l; // Die Wegstrecke zum Nächsten Wegpunkt
      // 9.1 Aufaddieren aller Wegstrecken die nach dem Wegpunkt noch kommen
      For j := fOpponents[i].NextWayPoint To high(Waypoints[fOpponents[i].Obj.Owner]) - 1 Do Begin
        // Distanz zwischen Wegpunkt j und j +1
        wx := Waypoints[fOpponents[i].Obj.Owner, j].Point.x * MapBlockSize;
        wy := Waypoints[fOpponents[i].Obj.Owner, j].Point.y * MapBlockSize;
        dx := Waypoints[fOpponents[i].Obj.Owner, j + 1].Point.x * MapBlockSize;
        dy := Waypoints[fOpponents[i].Obj.Owner, j + 1].Point.y * MapBlockSize;
        dx := dx - wx;
        dy := dy - wy;
        // Aufaddieren aller Wegpunkt distanzen
        fOpponents[i].DistanzeToGoal := fOpponents[i].DistanzeToGoal + sqrt(sqr(dx) + sqr(dy));
      End;
      If (gx = x) And (gy = y) Then Begin // Wir haben den Wegpunkt Erreicht, nächsten anvisieren
        fOpponents[i].NextWayPoint := fOpponents[i].NextWayPoint + 1;
        If fOpponents[i].NextWayPoint > high(Waypoints[fOpponents[i].Obj.Owner]) Then Begin
          // Die Einheit hat es bis zum Ende Geschafft
          If fOpponents[i].Obj.Bonus Then Begin
            // Bonus Einheiten kosten keine Leben
            UpdateEvent(fOpponents[i].Obj.Owner, UpdateIDBonusOpFinished, 0);
          End
          Else Begin
            UpdateEvent(fOpponents[i].Obj.Owner, UpdateIdLifeLost, 0);
          End;
          KillOpponent(fOpponents[i].Obj);
          Continue; // Für diesen Opponent hats sich erledigt, weiter mit den anderen
        End;
      End;
    End
    Else Begin // Auf dem Boden Laufende Gegner
      // Wir befinden uns auf dieser Kachel
      x := round(fOpponents[i].Obj.Position.x) Div MapBlockSize;
      y := round(fOpponents[i].Obj.Position.y) Div MapBlockSize;
      oh := FieldHeight[x, y, fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint];
      ah := oh;
      If oh = 0 Then Begin // Wir haben den Wegpunkt Erreicht, nächsten anvisieren
        fOpponents[i].NextWayPoint := fOpponents[i].NextWayPoint + 1;
        If fOpponents[i].NextWayPoint > high(Waypoints[fOpponents[i].Obj.Owner]) Then Begin
          // Die Einheit hat es bis zum Ende Geschafft
          If fOpponents[i].Obj.Bonus Then Begin // Bonus Einheiten kosten keine Leben
            UpdateEvent(fOpponents[i].Obj.Owner, UpdateIDBonusOpFinished, 0);
          End
          Else Begin
            UpdateEvent(fOpponents[i].Obj.Owner, UpdateIdLifeLost, 0);
          End;
          KillOpponent(fOpponents[i].Obj);
          Continue; // Für diesen Opponent hats sich erledigt, weiter mit den anderen
        End
        Else Begin
          oh := FieldHeight[x, y, fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint];
          ah := oh;
        End;
      End;
      // Wir suchen uns die Nächste Kachel in unserer Umgebung
      wx := -1;
      // TODO: Das könnte man ja echt mal "Effizienter" schreiben
      // Erst Waagrecht / Senkrecht Prüfen
      For j := 0 To 8 Do Begin
        xx := (j Mod 3) - 1;
        yy := (j Div 3) - 1;
        If (abs(xx) = 1) And (abs(yy) = 1) Then Continue;
        wh := FieldHeight[x + xx, y + yy, fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint];
        // Wir suchen den "Abstieg", und unterstützen Diagonalen
        steigung := 1; // Nur Waagrech / Senkrecht gehen
        If ((wh < ah) And (oh - wh <= steigung)) Then Begin
          // Merken des bisher besten
          ah := wh;
          wx := x + xx;
          wy := y + yy;
        End;
      End;
      // Dann auch Diagonal erlauben
      If Not fOpponents[i].NextTimeNoDiagWalk Then Begin
        For j := 0 To 8 Do Begin
          xx := (j Mod 3) - 1;
          yy := (j Div 3) - 1;
          wh := FieldHeight[x + xx, y + yy, fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint];
          // Wir suchen den "Abstieg", und unterstützen Diagonalen
          steigung := 2; // Erlauben Diagonale Bewegungen
          If fOpponents[i].NextTimeNoDiagWalk Then Begin
            steigung := 1; // Nur Waagrech / Senkrecht gehen
          End;
          If ((wh < ah) And (oh - wh <= steigung)) Then Begin
            // Merken des bisher besten
            ah := wh;
            wx := x + xx;
            wy := y + yy;
          End;
        End;
      End;
      If wx = -1 Then Begin
        fOpponents[i].Obj.Position := Waypoints[fOpponents[i].Obj.Owner, 0].Point;
        log('Oppent stands on not walkable map coordinate, reset to spawnpoint.', llCritical);
      End;
      // Nun, da wir Wissen wohin wir wollen lassen wir die Figur bewegen
      // 1. Bestimmen der Zielkoordinaten
      wx := wx * MapBlockSize;
      wy := wy * MapBlockSize;
      // 2. berechnen Der Wegdifferenz die eszu gehen geht
      dx := wx - fOpponents[i].Obj.Position.x;
      dy := wy - fOpponents[i].Obj.Position.y;
      // 3. Berechnen der Länge in Pixeln
      l := sqrt(sqr(dx) + sqr(dy));
      If l = 0 Then l := 1;
      // 4. Normieren auf 1 Pixel
      dx := dx / l;
      dy := dy / l;
      // 5. Berechnen der Schaffbaren Wegstrecke, bei der Geschwindigkeit die unser Opponent hat
      ll := fOpponents[i].Obj.GetInGameSpeed * MapBlockSize; // MapBlockSize Pro Sekunde
      ll := ll * delta / 1000; // Pixel in der Vergangenen Zeit.
      // 6. Wenn wir übers Ziel hinausschießen würden (weil der Benutzer den Opponent zu schnell gemacht hat
      If ll > l Then ll := l;
      // 7. Den Wegfortschritt Berechnen
      dx := dx * ll;
      dy := dy * ll;
      // 8. Tatsächlich Laufen
      ox := fOpponents[i].Obj.Position.x;
      oy := fOpponents[i].Obj.Position.y;
      odir := fOpponents[i].Obj.Direction;
      fOpponents[i].Obj.Position.x := fOpponents[i].Obj.Position.x + dx;
      fOpponents[i].Obj.Position.y := fOpponents[i].Obj.Position.y + dy;
      fOpponents[i].Obj.Direction := round(radtodeg(arctan2(-dy, dx)));
      // 9. Berechnen der "Reststrecke" zum Ziel
      fOpponents[i].DistanzeToGoal := l + ah; // Die Wegstrecke zum Nächsten Wegpunkt
      // 9.1 Aufaddieren aller Wegstrecken die nach dem Wegpunkt noch kommen
      For j := fOpponents[i].NextWayPoint To high(Waypoints[fOpponents[i].Obj.Owner]) - 1 Do Begin
        oh := FieldHeight[Waypoints[fOpponents[i].Obj.Owner, j].Point.x, Waypoints[fOpponents[i].Obj.Owner, j].Point.y, fOpponents[i].Obj.Owner, j + 1];
        fOpponents[i].DistanzeToGoal := fOpponents[i].DistanzeToGoal + oh;
      End;
      // Prüfen ob wir den Gegner nicht ausversehen in eine nicht begehbare Kachel geschoben haben
      x := round(fOpponents[i].Obj.Position.x) Div MapBlockSize;
      y := round(fOpponents[i].Obj.Position.y) Div MapBlockSize;
      oh := FieldHeight[x, y, fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint];
      If oh = Field_unreached Then Begin // Wir sind Diagonal auf ein Nicht begehbares Feld gelaufen
        fOpponents[i].NextTimeNoDiagWalk := true;
        fOpponents[i].NextTimeNoDiagWalkPos := v2(ox, oy);
        fOpponents[i].Obj.Position.x := ox;
        fOpponents[i].Obj.Position.y := oy;
        fOpponents[i].Obj.Direction := odir;
      End;
      // Wieder Abschalten des Diagonal-SperrModus
      If fOpponents[i].NextTimeNoDiagWalk Then Begin
        // Wir sind mindestens eine Kachelbreite von dem Problematischen Punkt entfernt
        If sqr(MapBlockSize) <= sqr(fOpponents[i].Obj.Position.x - fOpponents[i].NextTimeNoDiagWalkPos.x) + sqr(fOpponents[i].Obj.Position.y - fOpponents[i].NextTimeNoDiagWalkPos.y) Then Begin
          fOpponents[i].NextTimeNoDiagWalk := false;
        End;
      End;
    End;
  End;
  // Die Opponents Mögen sich nicht, Einbauen einer gewissen Divergenz
  SQR_Min_Dist := sqr(MinDistanceBetweenOpponents);
  For i := 0 To high(fOpponents) - 1 Do
    For j := i + 1 To high(fOpponents) Do Begin
      l := sqr(fOpponents[i].Obj.Position.x - fOpponents[j].Obj.Position.x) + sqr(fOpponents[i].Obj.Position.y - fOpponents[j].Obj.Position.y);
      // Zwei Gegner sind sich zu nahe gekommen und befinden sich in der Selben Ebene (Flug oder Boden)
      If (l <= SQR_Min_Dist) And (fOpponents[i].Obj.Canfly = fOpponents[j].Obj.Canfly) Then Begin
        l := sqrt(l); // Die eigentliche Strecke Ausrechnen
        If l = 0 Then l := 1;
        // Von i nach j
        dx := fOpponents[i].Obj.Position.x - fOpponents[j].Obj.Position.x;
        dy := fOpponents[i].Obj.Position.y - fOpponents[j].Obj.Position.y;
        If (dx = 0) And (dy = 0) Then dx := 0.1; // stehen die beiden Gegner exakt aufeinander, dann nehmen wir hier "Künstlich" eine Distanz an.
        dx := dx / l;
        dy := dy / l;
        ll := max(fOpponents[i].Obj.GetInGameSpeed, fOpponents[j].Obj.GetInGameSpeed) * MapBlockSize;
        ll := ll * delta / 1000;
        // Beide Auseinanderdrücken
        // Wenn 2 Gegner direkt hintereinander sind und der Vordere Stoppt, und sie in die Gleiche Richtung Wollen, dann
        // Blockiert der Vordere den Hinteren komplett. Damit dies nicht geschieht
        // Werten die beiden Gegner zusätzlich Rechtwinklig zur Bewegungsrichtung mit halber Geschwindigkeit verschoben (2. * ll *0.5 Term)
        ox := fOpponents[i].Obj.Position.x;
        oy := fOpponents[i].Obj.Position.y;
        fOpponents[i].Obj.Position.x := fOpponents[i].Obj.Position.x + dx * ll + dy * ll * 0.5;
        fOpponents[i].Obj.Position.y := fOpponents[i].Obj.Position.y + dy * ll - dx * ll * 0.5;
        // Prüfen ob wir den Gegner nicht ausversehen in eine nicht begehbare Kachel geschoben haben
        x := round(fOpponents[i].Obj.Position.x) Div MapBlockSize;
        y := round(fOpponents[i].Obj.Position.y) Div MapBlockSize;
        oh := FieldHeight[x, y, fOpponents[i].Obj.Owner, fOpponents[i].NextWayPoint];
        If oh = Field_unreached Then Begin
          fOpponents[i].Obj.Position.x := ox;
          fOpponents[i].Obj.Position.y := oy;
        End;
        ox := fOpponents[j].Obj.Position.x;
        oy := fOpponents[j].Obj.Position.y;
        fOpponents[j].Obj.Position.x := fOpponents[j].Obj.Position.x - dx * ll - dy * ll;
        fOpponents[j].Obj.Position.y := fOpponents[j].Obj.Position.y - dy * ll + dx * ll;
        x := round(fOpponents[j].Obj.Position.x) Div MapBlockSize;
        y := round(fOpponents[j].Obj.Position.y) Div MapBlockSize;
        oh := FieldHeight[x, y, fOpponents[j].Obj.Owner, fOpponents[j].NextWayPoint];
        If oh = Field_unreached Then Begin
          fOpponents[j].Obj.Position.x := ox;
          fOpponents[j].Obj.Position.y := oy;
        End;
      End;
    End;
End;

Procedure TMap.CreateBullet(Const Target: TOpponent; Const Pos: TVector2;
  Const aOwner: tctd_mapopbject);
Var
  b: TBuilding;
  h: THero;
Begin
  setlength(FBullets, high(FBullets) + 2);
  FBullets[high(FBullets)] := TBulletObject.Create;
  FBullets[high(FBullets)].Target := Target;
  FBullets[high(FBullets)].AnimationOffset := random(65536);
  FBullets[high(FBullets)].Position := pos;
  FBullets[high(FBullets)].Owner := aOwner;
  If aOwner Is TBuilding Then Begin
    b := tBuilding(aOwner);
    FBullets[high(FBullets)].RenderIndex := b.Stages[b.Stage].fbulletimage;
    FBullets[high(FBullets)].Width := b.Stages[b.Stage].bulletw * MapBlockSize;
    FBullets[high(FBullets)].Height := b.Stages[b.Stage].bulleth * MapBlockSize;
    FBullets[high(FBullets)].Speed := b.Stages[b.Stage].bulletspeed;
    FBullets[high(FBullets)].Power[0] := b.Stages[b.Stage].bulletpower[0];
    FBullets[high(FBullets)].Power[1] := b.Stages[b.Stage].bulletpower[1];
    FBullets[high(FBullets)].Power[2] := b.Stages[b.Stage].bulletpower[2];
    FBullets[high(FBullets)].Power[3] := b.Stages[b.Stage].bulletpower[3];
    FBullets[high(FBullets)].Splash := b.Stages[b.Stage].bulletsplashradius;
    FBullets[high(FBullets)].Earn := b.Stages[b.Stage].earn;
    FBullets[high(FBullets)].Slowdown.SlowDownDynamic := b.Stages[b.Stage].SlowDown.slowdowndynamic;
    FBullets[high(FBullets)].Slowdown.SlowDownStatic := b.Stages[b.Stage].SlowDown.slowdownstatic;
    FBullets[high(FBullets)].Slowdown.SlowDownTime := b.Stages[b.Stage].SlowDown.slowdowntime;
    FBullets[high(FBullets)].Strategy := b.strategy;
    FBullets[high(FBullets)].PreverAir := b.PreverAir;
  End
  Else Begin
    If aOwner Is tHero Then Begin
      h := THero(aOwner);
      FBullets[high(FBullets)].RenderIndex := h.Levels[h.Level].fbulletimage;
      FBullets[high(FBullets)].Width := h.Levels[h.Level].bulletw * MapBlockSize;
      FBullets[high(FBullets)].Height := h.Levels[h.Level].bulleth * MapBlockSize;
      FBullets[high(FBullets)].Speed := h.Levels[h.Level].bulletspeed;
      FBullets[high(FBullets)].Power[0] := h.Levels[h.Level].bulletpower[0];
      FBullets[high(FBullets)].Power[1] := h.Levels[h.Level].bulletpower[1];
      FBullets[high(FBullets)].Power[2] := h.Levels[h.Level].bulletpower[2];
      FBullets[high(FBullets)].Power[3] := h.Levels[h.Level].bulletpower[3];
      FBullets[high(FBullets)].Splash := 0; //h.Levels[h.Level].bulletsplashradius;
      FBullets[high(FBullets)].Earn := 0; //h.Levels[h.Level].earn;
      FBullets[high(FBullets)].Slowdown.SlowDownDynamic := 1; //h.Levels[h.Level].SlowDown.slowdowndynamic;
      FBullets[high(FBullets)].Slowdown.SlowDownStatic := 1; //h.Levels[h.Level].SlowDown.slowdownstatic;
      FBullets[high(FBullets)].Slowdown.SlowDownTime := 0; //h.Levels[h.Level].SlowDown.slowdowntime;
      FBullets[high(FBullets)].Strategy := h.strategy;
      FBullets[high(FBullets)].PreverAir := h.PreverAir;
    End
    Else Begin
      Raise Exception.Create('TMap.CreateBullet: missing implementation');
    End;
  End;
End;

Procedure TMap.HandleAllBuildings;
Var
  r, x, y: Single;
  k, i: Integer;
  p: Tvector2;
  t, t0: int64;
  tg: TOpponent;
Begin
  If FPausing Then exit;
  t := GetTick;
  If Speedup <> 1 Then Begin // Wenn eine Speedup aktiv ist, wird das Delta t0 zum letzten Frame entsprechend Zeitlich skalliert
    t0 := t - fHandleBuildingTime;
    t0 := t0 * (int64(Speedup) - 1);
  End;
  fHandleBuildingTime := t;
  For i := 0 To high(Fbuildings) Do Begin
    Fbuildings[i].Update(); // Falls das Gebäude erst noch gebaut werden muss..
    If (Fbuildings[i].fUpdating.State = usIdleInactive) And (Fbuildings[i].Stages[Fbuildings[i].Stage].range > 0) Then Begin
      If Speedup <> 1 Then Begin // Verschieben der Zuletzt geballert Zeit, beim Speedup in Richtung Vergangenheit
        Fbuildings[i].LastShootTime := Fbuildings[i].LastShootTime - t0;
      End;
      If ((t - Fbuildings[i].LastShootTime) >= Fbuildings[i].Stages[Fbuildings[i].Stage].reloadtime) Then Begin
        // Der Austrittspunkt der Waffe
        p.x := (Fbuildings[i].Position.x + Fbuildings[i].Stages[Fbuildings[i].Stage].bulletLeafPointx);
        p.y := (Fbuildings[i].Position.y - Fbuildings[i].Stages[Fbuildings[i].Stage].bulletLeafPointy + 1);
        r := Fbuildings[i].Stages[Fbuildings[i].Stage].range + 1;
        If Fbuildings[i].Stages[Fbuildings[i].Stage].bulletspeed <= 0 Then Begin
          // Ein Turm macht einen Belagerungsschaden, die Bullets verteilt er Gleichmäßig in seinem Range
          For k := 0 To 100 Do Begin // Wir versuchen 100 mal ein Bullet zu erzeugen, hören aber natürlich nach dem 1. Mal auf
            x := (random(2 * round(r) * 4) / 4) - r;
            y := (random(2 * round(r) * 4) / 4) - r;
            If sqr(x) + sqr(y) <= sqr(r) Then Begin
              // Todo : das Trunc ist nicht gut, da muss was besseres her
              If CoordIsWalkAble(trunc(p.x + x), trunc(p.y + y)) Then Begin // wir haben eine Position gefunden an derer wir den Bullet hinlegen können und theoretisch kann dort ein Gegner vorbeilaufen
                Fbuildings[i].LastShootTime := t;
                CreateBullet(Nil, v2(p.x + x, p.y + y), Fbuildings[i]);
                FBullets[high(FBullets)].Direction := random(360); // Der Bullet bewegt sich nicht, damit nicht alle gleich aussehen werden sie hier "Zugällig" rotiert.
                If Fbuildings[i].Stages[Fbuildings[i].Stage].bulletspeed < 0 Then Begin
                  FBullets[high(FBullets)].Position := v2(p.x, p.y);
                  FBullets[high(FBullets)].pPosition := v2(p.x + x, p.y + y);
                End;
                break;
              End;
            End;
          End;
        End
        Else Begin
          // Wir könnten wieder Schießen, aber ist auch was zum Abballern da ?
          tg := GetTarget(p, r, Fbuildings[i].Stages[Fbuildings[i].Stage].bulletpower, Fbuildings[i].Stages[Fbuildings[i].Stage].SlowDown, Fbuildings[i].Stages[Fbuildings[i].Stage].earn, Fbuildings[i].strategy, Fbuildings[i].PreverAir, Fbuildings[i], Nil);
          If assigned(tg) Then Begin
            Fbuildings[i].LastShootTime := t;
            CreateBullet(tg, p, Fbuildings[i]);
          End;
        End;
      End;
    End;
  End;
End;

Procedure TMap.HandleAllHeroes;
Const
  defSlowDown: TSlowDown = (slowdownstatic: 1.0; slowdowndynamic: 1.0; slowdowntime: 0);
Var
  r: Single;
  i: Integer;
  p: Tvector2;
  t, t0: int64;
  tg: TOpponent;
Begin
  If FPausing Then exit;
  t := GetTick;
  If Speedup <> 1 Then Begin // Wenn eine Speedup aktiv ist, wird das Delta t0 zum letzten Frame entsprechend Zeitlich skalliert
    t0 := t - fHandleBuildingTime;
    t0 := t0 * (int64(Speedup) - 1);
  End;
  fHandleBuildingTime := t;
  MoveAllHeroes;
  For i := 0 To high(Fheroes) Do Begin
    Fheroes[i].Update(); // Falls das Gebäude erst noch gebaut werden muss..
    If (Fheroes[i].Level >= 0) And (Fheroes[i].Levels[Fheroes[i].Level].range > 0) Then Begin
      If Speedup <> 1 Then Begin // Verschieben der Zuletzt geballert Zeit, beim Speedup in Richtung Vergangenheit
        Fheroes[i].LastShootTime := Fheroes[i].LastShootTime - t0;
      End;
      If ((t - Fheroes[i].LastShootTime) >= Fheroes[i].Levels[Fheroes[i].Level].reloadtime) Then Begin
        // Der Austrittspunkt der Waffe
        p.x := (Fheroes[i].Position.x + Fheroes[i].levels[Fheroes[i].Level].w / 2);
        p.y := (Fheroes[i].Position.y + 1 - Fheroes[i].levels[Fheroes[i].Level].h / 2);
        r := Fheroes[i].Levels[Fheroes[i].Level].range + 1;
        // Wir könnten wieder Schießen, aber ist auch was zum Abballern da ?
        tg := GetTarget(p, r, Fheroes[i].Levels[Fheroes[i].Level].bulletpower, defSlowDown, 0, Fheroes[i].strategy, Fheroes[i].PreverAir, Fheroes[i], Nil);
        If assigned(tg) Then Begin
          Fheroes[i].LastShootTime := t;
          CreateBullet(tg, p, Fheroes[i]);
        End;
      End;
    End;
  End;
End;

Procedure TMap.HandleAllBullets(Const UpdateEvent: TUpdateEvent);
Var
  k, i: integer;
  bool: Boolean;
  ownrd, // OwnerDamage
  rfnd, ownr, j: Integer;
  n, delta: Int64;
  tg2, tg: TOpponent;
Begin
  (*
   * Bewegen aller Bullets, und evtl. Auch Einschlagen, und damit Oppontents platt machen.
   *)
  If FPausing Then exit;
  n := GetTick;
  delta := n - fLastBulletTime;
  delta := delta * Speedup;
  fLastBulletTime := n;
  //Suche nach Gegnern, usw ...
  For i := 0 To high(FBullets) Do
    FBullets[i].Handled := false;
  bool := true;
  While bool Do Begin
    bool := false;
    For i := high(FBullets) Downto 0 Do
      If Not FBullets[i].Handled Then Begin
        FBullets[i].Handled := true;
        k := FBullets[i].Update(delta, fOpponents);
        If (k <> 0) Then Begin
          ownr := FBullets[i].Owner.Owner; // Der Owner, der den "Schaden" angerichtet hat = der dem der Bullet gehört hat
          tg := FBullets[i].Target; //Den Angegriffenen merken
          UpdateEvent(ownr, UpdateDamage_1, FBullets[i].DamgeDealt[0]);
          UpdateEvent(ownr, UpdateDamage_2, FBullets[i].DamgeDealt[1]);
          UpdateEvent(ownr, UpdateDamage_3, FBullets[i].DamgeDealt[2]);
          UpdateEvent(ownr, UpdateDamage_4, FBullets[i].DamgeDealt[3]);
          If FBullets[i].Owner Is THero Then Begin
            Thero(FBullets[i].Owner).AddDamageDealt(FBullets[i].DamgeDealt[0] + FBullets[i].DamgeDealt[1] + FBullets[i].DamgeDealt[2] + FBullets[i].DamgeDealt[3]);
          End;
          // Der Owner, dem der Kill zugesprochen wird (weil er den Meisten Damage auf dem Opp generiert hat), im Zweifel ist es der
          // dessen Bullet gerade eingeschlagen hat (nur wenn 2 Owner gleich viel schaden gemacht haben)
          ownrd := FBullets[i].Target.DamageByPlayers[ownr];
          For j := 0 To high(FBullets[i].Target.DamageByPlayers) Do Begin
            // Der Spieler j hat mehr schaden angerichtet als ownr -> also kriegt er den Kill zugesprochen
            If FBullets[i].Target.DamageByPlayers[j] > ownrd Then Begin
              ownrd := FBullets[i].Target.DamageByPlayers[j];
              ownr := j;
            End;
          End;
          rfnd := tg.Refund; // merken wieviel wir beim Vernichten einnehmen würden
          If (FBullets[i].Earn > 0) Then Begin // Es gab einen Einschlag, egal ob vernichtend oder nicht, wir kriegen Kohle..
            UpdateEvent(ownr, UpdateIdBankIncrease, FBullets[i].Earn);
          End;
          If (((k And 4) = 4) Or ((k And 2) = 2)) And ((k And 1) = 0) Then Begin // Wir müssen uns einen neuen Gegner suchen.
            If FBullets[i].Speed = 0 Then Begin // Der Bullet ist nicht beweglich, d.h. sein Target wird wieder = nil gesetzt
              FBullets[i].Target := Nil;
              If FBullets[i].Splash = 0 Then Begin // Hat der Bullet keinen Splash, dann ist er verbraucht und geht ein.
                k := k Or 1;
              End;
            End
            Else Begin // Ein Normaler Beweglicher Bullet sucht sich ein neues Ziel
              tg2 := GetTarget(
                fBullets[i].position, fBullets[i].Splash,
                FBullets[i].Power, FBullets[i].SlowDown, FBullets[i].Earn, FBullets[i].Strategy, FBullets[i].PreverAir,
                FBullets[i].Owner, tg);
              If assigned(tg2) Then Begin // Neues Ziel gefunden, und Los
                FBullets[i].Target := tg2;
              End
              Else Begin // Wir haben keinen neuen Gegner gefunden, also gehen wir ein
                FBullets[i].Target := Nil;
                k := k Or 1;
              End;
            End;
          End;
          If ((k And 1) = 1) Then Begin // Wir haben das nicht überlebt
            FBullets[i].Free;
            For j := i To high(FBullets) - 1 Do
              FBullets[j] := FBullets[j + 1];
            SetLength(FBullets, high(FBullets));
            bool := true;
          End;
          If ((k And 2) = 2) Then Begin // Der Gegner hat das nicht überlebt
            UpdateEvent(ownr, UpdateIdOpponentKilled, rfnd);
            KillOpponent(tg); // Macht den Gegner Platt und berücksichtigt dabei alle Bullets
            // Da Potentiell viele Bullets kaputt gehen können müssen wir aus der for schleife raus, da die ja noch die alten dimensionen hat und potentiell nichts mehr stimmen kann
            bool := true;
          End;
          If bool Then break; // Muss so sein, da sonst evtl. die 2. if nicht ausgewertet würde.
        End;
      End;
  End;
End;

Procedure TMap.Start;
Var
  n: int64;
  i: Integer;
Begin
  // Wir starten was neues, also alles alte vorher Platt machen
  setlength(fOpponents, 0);
  setlength(FBullets, 0);
  FPausing := False;
  n := Gettick;
  fLastOppMoveTime := n;
  fLastHeroMoveTime := n;
  fLastBulletTime := n;
  For i := 0 To high(Fbuildings) Do Begin
    Fbuildings[i].start;
  End;
  For i := 0 To high(Fheroes) Do Begin
    Fheroes[i].start;
  End;
  CalcWaypointFields();
End;

Procedure TMap.GetMovingObjectsState(Const Stream: TSTream);
Var
  i: integer;
  u16: uint16;
  u32: uint32;
Begin
  (*
   * wird von TMap.UpdateMoveables ausgewertet
   *)
  // 1. Opponents
  u16 := length(fOpponents);
  stream.Write(u16, SizeOf(u16));
  For i := 0 To high(fOpponents) Do Begin
    fOpponents[i].Obj.GetMovingState(stream);
  End;
  // 2. Bullets
  u32 := length(FBullets);
  stream.Write(u32, SizeOf(u32));
  For i := 0 To high(FBullets) Do Begin
    FBullets[i].GetMovingState(Stream);
  End;
  // 3. Heroes
  u16 := length(fHeroes);
  stream.Write(u16, SizeOf(u16));
  For i := 0 To high(fHeroes) Do Begin
    fHeroes[i].getMovingState(Stream);
  End;
End;

Function TMap.SaveGameingData(Const Stream: TStream): Boolean;
Var
  i, j: integer;
  b: Boolean;
  s: String;
  f: Single;
Begin
  result := false;
  // Karten Dimensionen für Rudimentärcheks
  i := High(fTerrain) + 1;
  stream.Write(i, SizeOf(i));
  i := High(fTerrain[0]) + 1;
  stream.Write(i, SizeOf(i));
  // Blocked Informationen
  For i := 0 To high(fTerrain) Do
    For j := 0 To high(fTerrain[i]) Do Begin
      b := fTerrain[i, j].blocked;
      stream.Write(b, SizeOf(b));
    End;
  // Alles was zur Herstellung eines Gebäudes notwendig ist.
  i := length(Fbuildings);
  stream.write(i, sizeof(i));
  For i := 0 To high(Fbuildings) Do Begin
    s := extractfilename(Fbuildings[i].Filename);
    stream.WriteAnsiString(s);
    f := Fbuildings[i].Position.x;
    stream.Write(f, SizeOf(f));
    f := Fbuildings[i].Position.y;
    stream.Write(f, SizeOf(f));
    j := Fbuildings[i].Stage;
    stream.Write(j, SizeOf(j));
    j := Fbuildings[i].Owner;
    stream.Write(j, SizeOf(j));
    stream.Write(Fbuildings[i].strategy, SizeOf(Fbuildings[i].strategy));
    b := Fbuildings[i].PreverAir;
    stream.Write(b, SizeOf(b));
  End;
  // Alles was mit den Helden zu tun hat ..
  i := length(fHeroes);
  stream.write(i, sizeof(i));
  For i := 0 To high(fHeroes) Do Begin
    s := extractfilename(fHeroes[i].Filename);
    stream.WriteAnsiString(s);
    f := fHeroes[i].Position.x;
    stream.Write(f, SizeOf(f));
    f := fHeroes[i].Position.y;
    stream.Write(f, SizeOf(f));
    j := fHeroes[i].Level;
    stream.Write(j, SizeOf(j));
    j := fHeroes[i].CollectedDamages;
    stream.Write(j, SizeOf(j));
    j := fHeroes[i].Direction;
    stream.Write(j, SizeOf(j));
    j := fHeroes[i].Owner;
    stream.Write(j, SizeOf(j));
    stream.Write(fHeroes[i].strategy, SizeOf(fHeroes[i].strategy));
    b := fHeroes[i].PreverAir;
    stream.Write(b, SizeOf(b));
  End;
  result := true;
End;

Function TMap.GetTarget(Const position: TVector2; Const Range: Single;
  Const Power: TPower; Const Slowdown: TSlowDown; Const Earn: integer;
  Const Strategy: TBuildingStrategy; Const PreverAir: Boolean;
  Const Owner: tctd_mapopbject; Const SkipOP: TOpponent): TOpponent;

Var
  Kandidats: Array Of integer;
  KandidatsCount: integer;

  (*
   * Wählt einen Geeigneten Kandidaten aus
   *)
  Procedure SelectOpponentByCandidat(OnlyOwnOpponents: Boolean);
  Var
    t, s: Single;
    j: integer;
    NoPrev: Boolean;
    RKandidats: Array Of integer;
    RKandidatsCount: integer;
  Begin
    Case Strategy Of
      bsFirst: Begin
          // Den "kürzesten" präferierten
          s := high(integer); // Egal, wird unten richtig initialisiert aber der Compiler blickt das net
          For j := 0 To KandidatsCount - 1 Do Begin
            If fOpponents[Kandidats[j]].Obj.Canfly = PreverAir Then Begin
              If assigned(result) Then Begin
                If s > fOpponents[Kandidats[j]].DistanzeToGoal Then Begin
                  If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                    result := fOpponents[Kandidats[j]].Obj; // Der erste Fliegende Gegner
                    s := fOpponents[Kandidats[j]].DistanzeToGoal;
                  End;
                End;
              End
              Else Begin
                If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                  result := fOpponents[Kandidats[j]].Obj; // Der erste Fliegende Gegner
                  s := fOpponents[Kandidats[j]].DistanzeToGoal;
                End;
              End;
            End;
          End;
          If Not assigned(result) Then Begin // Haben keinen Präferierten gefunden =>  alle dürfen ran ;)
            For j := 0 To KandidatsCount - 1 Do Begin
              If assigned(result) Then Begin
                If s > fOpponents[Kandidats[j]].DistanzeToGoal Then Begin
                  If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                    result := fOpponents[Kandidats[j]].Obj;
                    s := fOpponents[Kandidats[j]].DistanzeToGoal;
                  End;
                End;
              End
              Else Begin
                If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                  result := fOpponents[Kandidats[j]].Obj;
                  s := fOpponents[Kandidats[j]].DistanzeToGoal;
                End;
              End;
            End;
          End;
        End;
      bsLast: Begin
          s := high(integer); // Egal, wird unten richtig initialisiert aber der Compiler blickt das net
          // Den "längsten" präferierten
          For j := 0 To KandidatsCount - 1 Do Begin
            If fOpponents[Kandidats[j]].Obj.Canfly = PreverAir Then Begin
              If assigned(result) Then Begin
                If s < fOpponents[Kandidats[j]].DistanzeToGoal Then Begin
                  If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                    result := fOpponents[Kandidats[j]].Obj; // Der erste Fliegende Gegner
                    s := fOpponents[Kandidats[j]].DistanzeToGoal;
                  End;
                End;
              End
              Else Begin
                If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                  result := fOpponents[Kandidats[j]].Obj; // Der erste Fliegende Gegner
                  s := fOpponents[Kandidats[j]].DistanzeToGoal;
                End;
              End;
            End;
          End;
          If Not assigned(result) Then Begin // Haben keinen Präferierten gefunden =>  alle dürfen ran ;)
            For j := 0 To KandidatsCount - 1 Do Begin
              If assigned(result) Then Begin
                If s < fOpponents[Kandidats[j]].DistanzeToGoal Then Begin
                  If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                    result := fOpponents[Kandidats[j]].Obj;
                    s := fOpponents[Kandidats[j]].DistanzeToGoal;
                  End;
                End;
              End
              Else Begin
                If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                  result := fOpponents[Kandidats[j]].Obj;
                  s := fOpponents[Kandidats[j]].DistanzeToGoal;
                End;
              End;
            End;
          End;
        End;
      bsFarest: Begin
          // Suchen des 1. entsprechend der Präferierung
          For j := 1 To KandidatsCount - 1 Do Begin
            If fOpponents[Kandidats[j]].Obj.Canfly = PreverAir Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := sqr(fOpponents[Kandidats[j]].Obj.Position.x + fOpponents[Kandidats[j]].Obj.SizeX * MapBlockSize / 2 - position.x)
                  + sqr(fOpponents[Kandidats[j]].Obj.Position.y + MapBlockSize + fOpponents[Kandidats[j]].Obj.Sizey * MapBlockSize / 2 - position.y);
                result := fOpponents[Kandidats[j]].Obj;
                break;
              End;
            End;
          End;
          NoPrev := Not assigned(result); // True, wenn es keine Präverierten Einheiten gibt.
          If Not assigned(result) Then Begin // Wenns keinen Fliegenden Gab dann den 1. anwählen
            If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[0]].Obj.Owner = owner.Owner))) Then Begin
              s := sqr(fOpponents[Kandidats[0]].Obj.Position.x + fOpponents[Kandidats[0]].Obj.SizeX * MapBlockSize / 2 - position.x)
                + sqr(fOpponents[Kandidats[0]].Obj.Position.y + MapBlockSize + fOpponents[Kandidats[0]].Obj.Sizey * MapBlockSize / 2 - position.y);
              result := fOpponents[Kandidats[0]].Obj;
            End;
          End;
          For j := 1 To KandidatsCount - 1 Do Begin
            t := sqr(fOpponents[Kandidats[j]].Obj.Position.x + fOpponents[Kandidats[j]].Obj.SizeX * MapBlockSize / 2 - position.x)
              + sqr(fOpponents[Kandidats[j]].Obj.Position.y + MapBlockSize + fOpponents[Kandidats[j]].Obj.Sizey * MapBlockSize / 2 - position.y);
            If (s < t) And ((fOpponents[Kandidats[j]].Obj.Canfly = PreverAir) Or NoPrev) Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := t;
                Result := fOpponents[Kandidats[j]].Obj;
              End;
            End;
          End;
        End;
      bsNearest: Begin
          // Suchen des 1. entsprechend der Präferierung
          For j := 1 To KandidatsCount - 1 Do Begin
            If fOpponents[Kandidats[j]].Obj.Canfly = PreverAir Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := sqr(fOpponents[Kandidats[j]].Obj.Position.x + fOpponents[Kandidats[j]].Obj.SizeX * MapBlockSize / 2 - position.x)
                  + sqr(fOpponents[Kandidats[j]].Obj.Position.y + MapBlockSize + fOpponents[Kandidats[j]].Obj.Sizey * MapBlockSize / 2 - position.y);
                result := fOpponents[Kandidats[j]].Obj;
                break;
              End;
            End;
          End;
          NoPrev := Not assigned(result); // True, wenn es keine Präverierten Einheiten gibt.
          If Not assigned(result) Then Begin // Wenns keinen Fliegenden Gab dann den 1. anwählen
            If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[0]].Obj.Owner = owner.Owner))) Then Begin
              s := sqr(fOpponents[Kandidats[0]].Obj.Position.x + fOpponents[Kandidats[0]].Obj.SizeX * MapBlockSize / 2 - position.x)
                + sqr(fOpponents[Kandidats[0]].Obj.Position.y + MapBlockSize + fOpponents[Kandidats[0]].Obj.Sizey * MapBlockSize / 2 - position.y);
              result := fOpponents[Kandidats[0]].Obj;
            End;
          End;
          For j := 1 To KandidatsCount - 1 Do Begin
            t := sqr(fOpponents[Kandidats[j]].Obj.Position.x + fOpponents[Kandidats[j]].Obj.SizeX * MapBlockSize / 2 - position.x)
              + sqr(fOpponents[Kandidats[j]].Obj.Position.y + MapBlockSize + fOpponents[Kandidats[j]].Obj.Sizey * MapBlockSize / 2 - position.y);
            If (s > t) And ((fOpponents[Kandidats[j]].Obj.Canfly = PreverAir) Or NoPrev) Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := t;
                Result := fOpponents[Kandidats[j]].Obj;
              End;
            End;
          End;
        End;
      bsStrongest: Begin
          // Suchen des 1. entsprechend der Präferierung
          For j := 1 To KandidatsCount - 1 Do Begin
            If fOpponents[Kandidats[j]].Obj.Canfly = PreverAir Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := fOpponents[Kandidats[j]].Obj.LifePoints[0] + fOpponents[Kandidats[j]].Obj.LifePoints[1] + fOpponents[Kandidats[j]].Obj.LifePoints[2] + fOpponents[Kandidats[j]].Obj.LifePoints[3];
                result := fOpponents[Kandidats[j]].Obj;
                break;
              End;
            End;
          End;
          NoPrev := Not assigned(result); // True, wenn es keine Präverierten Einheiten gibt.
          If Not assigned(result) Then Begin // Wenns keinen Fliegenden Gab dann den 1. anwählen
            If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[0]].Obj.Owner = owner.Owner))) Then Begin
              s := fOpponents[Kandidats[0]].Obj.LifePoints[0] + fOpponents[Kandidats[0]].Obj.LifePoints[1] + fOpponents[Kandidats[0]].Obj.LifePoints[2] + fOpponents[Kandidats[0]].Obj.LifePoints[3];
              result := fOpponents[Kandidats[0]].Obj;
            End;
          End;
          For j := 1 To KandidatsCount - 1 Do Begin
            t := fOpponents[Kandidats[j]].Obj.LifePoints[0] + fOpponents[Kandidats[j]].Obj.LifePoints[1] + fOpponents[Kandidats[j]].Obj.LifePoints[2] + fOpponents[Kandidats[j]].Obj.LifePoints[3];
            If (s < t) And ((fOpponents[Kandidats[j]].Obj.Canfly = PreverAir) Or NoPrev) Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := t;
                Result := fOpponents[Kandidats[j]].Obj;
              End;
            End;
          End;
        End;
      bsWeakest: Begin
          // Suchen des 1. entsprechend der Präferierung
          For j := 1 To KandidatsCount - 1 Do Begin
            If fOpponents[Kandidats[j]].Obj.Canfly = PreverAir Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := fOpponents[Kandidats[j]].Obj.LifePoints[0] + fOpponents[Kandidats[j]].Obj.LifePoints[1] + fOpponents[Kandidats[j]].Obj.LifePoints[2] + fOpponents[Kandidats[j]].Obj.LifePoints[3];
                result := fOpponents[Kandidats[j]].Obj;
                break;
              End;
            End;
          End;
          NoPrev := Not assigned(result); // True, wenn es keine Präverierten Einheiten gibt.
          If Not assigned(result) Then Begin // Wenns keinen Fliegenden Gab dann den 1. anwählen
            If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[0]].Obj.Owner = owner.Owner))) Then Begin
              s := fOpponents[Kandidats[0]].Obj.LifePoints[0] + fOpponents[Kandidats[0]].Obj.LifePoints[1] + fOpponents[Kandidats[0]].Obj.LifePoints[2] + fOpponents[Kandidats[0]].Obj.LifePoints[3];
              result := fOpponents[Kandidats[0]].Obj;
            End;
          End;
          For j := 1 To KandidatsCount - 1 Do Begin
            t := fOpponents[Kandidats[j]].Obj.LifePoints[0] + fOpponents[Kandidats[j]].Obj.LifePoints[1] + fOpponents[Kandidats[j]].Obj.LifePoints[2] + fOpponents[Kandidats[j]].Obj.LifePoints[3];
            If (s > t) And ((fOpponents[Kandidats[j]].Obj.Canfly = PreverAir) Or NoPrev) Then Begin
              If ((Not OnlyOwnOpponents) Or (OnlyOwnOpponents And (fOpponents[Kandidats[j]].Obj.Owner = owner.Owner))) Then Begin
                s := t;
                Result := fOpponents[Kandidats[j]].Obj;
              End;
            End;
          End;
        End;
      bsRandom: Begin
          Result := fOpponents[Kandidats[random(KandidatsCount)]].Obj;
          If (OnlyOwnOpponents And (result.Owner <> owner.Owner)) Then Begin
            RKandidats := Nil;
            setlength(RKandidats, length(Kandidats));
            RKandidatsCount := 0;
            For j := 0 To high(Kandidats) Do Begin
              If fOpponents[Kandidats[j]].Obj.Owner = owner.Owner Then Begin
                RKandidats[RKandidatsCount] := Kandidats[j];
                inc(RKandidatsCount);
              End;
            End;
            If RKandidatsCount = 0 Then Begin
              result := Nil;
            End
            Else Begin
              Result := fOpponents[RKandidats[random(RKandidatsCount)]].Obj;
            End;
            setlength(RKandidats, 0);
          End;
        End;
    End;
  End;
Var
  RangeSquared: Single;
  j, k: integer;
  bb, bool: Boolean;
Begin
  result := Nil;
  If range = 0 Then exit;
  // 1. Liste aller Gegner im Range suchen
  RangeSquared := sqr(Range);
  KandidatsCount := 0;
  Kandidats := Nil;
  setlength(Kandidats, length(fOpponents));
  For j := 0 To high(fOpponents) Do Begin
    If (SkipOP <> fOpponents[j].obj) And
      (sqr(fOpponents[j].Obj.Position.x + fOpponents[j].Obj.SizeX * MapBlockSize / 2 - position.x - 0.5 * MapBlockSize)
      + sqr(fOpponents[j].Obj.Position.y + fOpponents[j].Obj.Sizey * MapBlockSize / 2 - position.y) <= RangeSquared) Then Begin
      bool := (SlowDown.slowdownstatic <> 1) Or (SlowDown.slowdowndynamic <> 1);
      If bool Then Begin // Wenn Slowdownschaden gemacht werden kann, dann schaun ob es sich noch lohnt.
        bool := (fOpponents[j].Obj.isSlowing(Owner, bb) = -1); // True, wenn wir den Gegner noch nicht beschiesen
        bool := bool Or (Not bb); // Wenn wir ihn Beschießen, aber die Dynamic Zeit abgelaufen ist, kann er auch wieder beschossen werden.
      End;
      bool := bool Or (Earn > 0); // Wenn es Kohle gibt, lohnt es sich immer
      If Not bool Then Begin
        For k := 0 To 3 Do Begin
          // Das Gebäude ist in der Lage dem Gegner schaden zu zu fügen
          If (fOpponents[j].Obj.LifePoints[k] > 0) And (power[k] > 0) Then Begin
            bool := true;
            break;
          End;
        End;
      End;
      // Zur Sicherheit, ein bereits gestorbener Opponnent darf definitiv nicht zum Kandidaten werden.
      // Der Gegner kann potentiell Beschossen werden, also in die Liste der Kandidaten
      If bool And ((fOpponents[j].Obj.LifePoints[0] > 0) Or (fOpponents[j].Obj.LifePoints[1] > 0) Or (fOpponents[j].Obj.LifePoints[2] > 0) Or (fOpponents[j].Obj.LifePoints[3] > 0)) Then Begin
        Kandidats[KandidatsCount] := j;
        inc(KandidatsCount);
      End;
    End;
  End;
  If KandidatsCount = 0 Then exit; // Keinen gefunden
  // 2. Je nach Strategie schaun welchen wir nehmen
  // Im Single Modus präferieren wir immer unsere Eigenen Gegner
  // Erst wenn es da keine mehr hat nehmen wir die "anderen" um deren Kills zu klauen ;)
  If MapType In [mtSingle, mtSingleMaze] Then Begin
    SelectOpponentByCandidat(true);
    // Wir haben einen "Eigenen" Gegner gefunden, dann sind wir hier Fertig ;)
    If assigned(result) Then exit;
  End;
  SelectOpponentByCandidat(false);
End;

Procedure TMap.ChangeBuildingStrategy(x, y, Owner: integer;
  Strategy: TBuildingStrategy; PreverAir: Boolean);
Var
  i: Integer;
Begin
  For i := 0 To high(Fbuildings) Do Begin
    If (round(Fbuildings[i].Position.x) = x) And
      (round(Fbuildings[i].Position.y) = y) And
      (Fbuildings[i].Owner = Owner) Then Begin
      Fbuildings[i].strategy := Strategy;
      Fbuildings[i].PreverAir := PreverAir;
      break;
    End;
  End;
End;

Procedure TMap.ChangeHeroStrategy(HeroIndex, Owner: integer;
  Strategy: TBuildingStrategy; PreverAir: Boolean);
Begin
  If (HeroIndex < 0) Or (HeroIndex > high(fHeroes)) Then Begin
    Log('TMap.ChangeHeroStrategy: invalid heroindex', llFatal);
  End;
  If fHeroes[HeroIndex].Owner <> Owner Then Begin
    Log('TMap.ChangeHeroStrategy: invalid owner', llFatal);
  End;
  fHeroes[HeroIndex].strategy := Strategy;
  fHeroes[HeroIndex].PreverAir := PreverAir;
End;

Procedure TMap.SetheroTarget(HeroIndex, Owner: integer; x, y: Single);
Begin
  If (HeroIndex < 0) Or (HeroIndex > high(fHeroes)) Then Begin
    Log('TMap.ChangeHeroStrategy: invalid heroindex', llFatal);
  End;
  If fHeroes[HeroIndex].Owner <> Owner Then Begin
    Log('TMap.ChangeHeroStrategy: invalid owner', llFatal);
  End;
  fHeroes[HeroIndex].targetpos := v2(x, y);
End;

Procedure TMap.AddHighScore(aPlayerName: String; aTimeStamp: TDateTime;
  aPoints, aPlaytimeinS: int64; aRound, aDifficulty, PlayerCount,
  LivesLost: integer);
Begin
  Change(false);
  fHighscoreEngine.AddHighScore(aPlayerName, aTimeStamp, aPoints, aPlaytimeinS, aRound, aDifficulty, PlayerCount, LivesLost);
End;

Procedure TMap.ClearHighScore;
Begin
  Change(true);
End;

Procedure TMap.GetHighscoreAndRating(Const Stream: TStream);
Var
  f: Single;
  i: integer;
Begin
  fHighscoreEngine.SaveToStream(Stream);
  f := GetRating();
  stream.Write(f, SizeOf(f));
  i := fRatingCount;
  stream.Write(i, SizeOf(i));
End;

Procedure TMap.ResetRating;
Begin
  fRatingCount := 0;
  fRatingSum := 0;
End;

Procedure TMap.AddRating(Rating: integer);
Begin
  If (rating >= 1) And (rating <= 5) Then Begin
    fRatingCount := fRatingCount + 1;
    fRatingSum := fRatingSum + Rating;
    Change(false);
  End;
End;

Function TMap.AddRandomWave(CreateDifficulty: integer): String;
  Function Powersum(Const P: TPower): integer;
  Begin
    result := p[0] + p[1] + p[2] + p[3];
  End;

Var
  BuildingList: TStringList;
  BuildingListData: Array Of TBuilding;
  OppList: TStringList;
  OppListData: Array Of TOpponent;
  n, i, j: Integer;
  CheapestBuildingCost, aWaveHitPoints, WaveHitPoints, NewWaveHitPoints: Integer;
  psi, ps: integer;
  pavailable: TPower; // Wenn 1, dann gibt es mindestens 1 Gebäude welches diese Schadensklasse treffen kann
  tries, k: integer;
Begin
  result := ''; // Leerstring = OK
  // 1. Prüfungen ob alles da ist was man für ne neue Wave braucht
  BuildingList := FindAllFiles(MapFolder + MapName, '*.geb', false);
  // - mindestens ein Buyable -> mit dessen Schadensklassen werden dann die opponents gefiltert
  If BuildingList.Count = 0 Then Begin
    result := 'no buyable buildings in map.';
    BuildingList.Free;
    exit;
  End;
  // TODO: - erstellen der Powers die die Gebäude "beschädigen" können
  // - mindestens ein Opponent
  OppList := FindAllFiles(MapFolder + MapName, '*.opp', false);
  If BuildingList.Count = 0 Then Begin
    result := 'no opponents in map.';
    BuildingList.Free;
    OppList.free;
    exit;
  End;
  (*
   * Ab jetzt haben wir alle Verfügbaren Gebs in BuildingList und alle Verfügbaren Opps in OppList
   *)
  pavailable[0] := 0;
  pavailable[1] := 0;
  pavailable[2] := 0;
  pavailable[3] := 0;
  BuildingListData := Nil;
  setlength(BuildingListData, BuildingList.Count);
  For i := 0 To high(BuildingListData) Do Begin
    BuildingListData[i] := TBuilding.create();
    If Not BuildingListData[i].LoadFromFile(BuildingList[i]) Then Begin
      For j := 0 To i Do Begin
        BuildingListData[i].Free;
      End;
      BuildingList.free;
      OppList.free;
      result := 'Could not load: ' + ExtractFileName(BuildingList[i]);
      exit;
    End;
    // - Prüfen ob das Geb mindestens 1 Stage hat !
    If high(BuildingListData[i].Stages) = -1 Then Begin
      For j := 0 To i Do Begin
        BuildingListData[i].Free;
      End;
      BuildingList.free;
      OppList.free;
      result := 'Building is invalid: ' + ExtractFileName(BuildingList[i]);
      exit;
    End;
    If i = 0 Then Begin
      CheapestBuildingCost := BuildingListData[i].Stages[0].Cost;
    End
    Else Begin
      If CheapestBuildingCost > BuildingListData[i].Stages[0].Cost Then Begin
        CheapestBuildingCost := BuildingListData[i].Stages[0].Cost;
      End;
    End;
  End;
  BuildingList.free;
  // Die Gebs raus werfen die erst nach der zu erstellenden Wave verfügbar sein sollen
  For i := high(BuildingListData) Downto 0 Do Begin
    k := -1; // -1 = geb muss entfernt werden
    For j := 0 To high(fBuyAbles) Do Begin
      If ExtractFileName(BuildingListData[i].Filename) = fBuyAbles[j].Item Then Begin
        If fBuyAbles[j].WaveNum <= length(Waves) Then Begin // Das Gebäude darf behalten werden
          k := 1;
        End;
        break;
      End;
    End;
    If k = -1 Then Begin
      BuildingListData[i].Free;
      For j := i To high(BuildingListData) - 1 Do Begin
        BuildingListData[j] := BuildingListData[j + 1];
      End;
      SetLength(BuildingListData, high(BuildingListData));
    End;
  End;
  If length(BuildingListData) = 0 Then Begin
    result := 'No Buildings available for wave.';
    exit;
  End;
  // -- Schauen welche Schadensklassen überhaupt "getroffen" werden können
  For i := 0 To high(BuildingListData) Do Begin
    If BuildingListData[i].Stages[0].bulletpower[0] > 0 Then pavailable[0] := 1;
    If BuildingListData[i].Stages[0].bulletpower[1] > 0 Then pavailable[1] := 1;
    If BuildingListData[i].Stages[0].bulletpower[2] > 0 Then pavailable[2] := 1;
    If BuildingListData[i].Stages[0].bulletpower[3] > 0 Then pavailable[3] := 1;
  End;

  OppListData := Nil;
  setlength(OppListData, OppList.Count);
  For i := 0 To high(OppListData) Do Begin
    OppListData[i] := TOpponent.create();
    If Not OppListData[i].LoadFromFile(OppList[i]) Then Begin
      For j := 0 To high(BuildingListData) Do Begin
        BuildingListData[j].Free;
      End;
      For j := 0 To i Do Begin
        OppListData[i].Free;
      End;
      OppList.free;
      result := 'Could not load: ' + ExtractFileName(OppList[i]);
      exit;
    End;
    If powersum(OppListData[i].LifePoints) <= 0 Then Begin
      For j := 0 To high(BuildingListData) Do Begin
        BuildingListData[j].Free;
      End;
      For j := 0 To i Do Begin
        OppListData[i].Free;
      End;
      OppList.free;
      result := 'Invalid opponent: ' + ExtractFileName(OppList[i]);
      exit;
    End;
  End;
  OppList.free;
  // - aus Filtern der Opps die Schadensklassen haben die nicht "getroffen" werden können
  For i := high(OppListData) Downto 0 Do Begin
    If ((OppListData[i].LifePoints[0] > 0) And (pavailable[0] = 0)) Or
      ((OppListData[i].LifePoints[1] > 0) And (pavailable[1] = 0)) Or
      ((OppListData[i].LifePoints[2] > 0) And (pavailable[2] = 0)) Or
      ((OppListData[i].LifePoints[3] > 0) And (pavailable[3] = 0)) Then Begin
      // Der Gegner hat eine Schadensklasse welche von keinem Turm getroffen werden kann -> Raus damit
      OppListData[i].Free;
      For j := i To high(OppListData) - 1 Do Begin
        OppListData[j] := OppListData[j + 1];
      End;
      setlength(OppListData, high(OppListData));
    End;
  End;
  If length(OppListData) = 0 Then Begin
    For j := 0 To high(BuildingListData) Do Begin
      BuildingListData[j].Free;
    End;
    result := 'There is no opponent available that can be defeated by the available buildings.';
    exit;
  End;
  (*
   * Alle Opponents und Gebs sind in den entsprechenden Listen geladen, nun kann das Erstellen beginnen.
   *)
  (*
   * Die Welle N + 1 wird aus der Welle N heraus erstellt -> Also erst mal Zahlen und Fakten zur "Alten" Wave Sammeln
   *)
  WaveHitPoints := 0;
  n := High(Waves);
  If n = -1 Then Begin
    // Die Karte hat noch gar keine Waves, dann geht es also darum die "initiale" Wave zu definieren
    // Wir suchen den Schwächsten Gegner erstellen diesen 10 mal und geben Geld für 2 * ein gebäude das ihn schlagen kann
    psi := 0;
    ps := Powersum(OppListData[0].LifePoints);
    For i := 1 To high(OppListData) Do Begin
      If ps > Powersum(OppListData[i].LifePoints) Then Begin
        psi := i;
        ps := Powersum(OppListData[i].LifePoints);
      End;
    End;
    SetLength(Waves, 1);
    setlength(Waves[0].Opponents, 1);
    Waves[0].Opponents[0].opponent := ExtractFileName(OppListData[psi].Filename);
    Waves[0].Opponents[0].refund := 1 + Random(2 - CreateDifficulty);
    Waves[0].Opponents[0].Count := 10 + random(CreateDifficulty * 10);
    Waves[0].Opponents[0].SpawnDelay := 5000 - random(CreateDifficulty * 1000);
    Waves[0].Opponents[0].UnitsPerSpawn := 1 + Random(CreateDifficulty);
    Waves[0].Opponents[0].Spawndelta := 1000 - random(CreateDifficulty * 250);
    // TODO: Die gebäude Heuristik könnte theoretisch ein Gebäude wählen das nicht in der Lage ist den Gegner zu besiegen
    // Das Gebäude
    psi := 0;
    ps := BuildingListData[0].Stages[0].Cost;
    For i := 0 To high(BuildingListData) Do Begin
      If ps > BuildingListData[i].Stages[0].Cost Then Begin
        psi := i;
        ps := BuildingListData[i].Stages[0].Cost;
      End;
    End;
    Waves[0].ChashOnStart := 2 * ps + random(3 - CreateDifficulty) * ps;
    Waves[0].WaveHint := ''; // TODO: evtl aus ner Liste von blöden Sprüchen nen Text auswählen ??
    // Alles temporäre frei geben
    For j := 0 To high(BuildingListData) Do Begin
      BuildingListData[j].Free;
    End;
    For j := 0 To high(OppListData) Do Begin
      OppListData[j].Free;
    End;
    exit;
  End
  Else Begin
    For i := 0 To high(Waves[n].Opponents) Do Begin
      For j := 0 To high(OppListData) Do Begin
        If ExtractFileName(OppListData[j].Filename) = Waves[n].Opponents[i].opponent Then Begin
          WaveHitPoints := WaveHitPoints + Powersum(OppListData[j].LifePoints) * Waves[n].Opponents[i].Count;
          break;
        End;
      End;
    End;
  End;
  (*
   * Erstellen der N + 1 ten Wave ;)
   *)
  (*
   * Ab Jetzt Bekannt
   * WaveHitPoints die Hitpoints Aller Gegner die in der Letzten Wave erzeugt wurden.
   *)
  NewWaveHitPoints := round(WaveHitPoints * (1.5 + 0.5 * CreateDifficulty));
  SetLength(Waves, length(Waves) + 1);
  n := High(Waves);
  Waves[n].ChashOnStart := random(3 - Difficulty) * CheapestBuildingCost; // -- Das ist natürlich Problematisch, wenn der User Hecken hat, denn dann kommt hier sehr oft 0 oder 1
  Waves[n].WaveHint := ''; // TODO: evtl aus ner Liste von blöden Sprüchen nen Text auswählen ??
  (*
   * Wir erstellen nun einfach So lange Gegner bis die gewünschte Hit Point Zahl erreicht ist.
   *)
  aWaveHitPoints := 0;
  tries := 0;
  While (aWaveHitPoints < NewWaveHitPoints) And (tries <= 50) Do Begin
    i := random(length(OppListData)); // Einen Zufälligen Gegner auswählen
    If Powersum(OppListData[i].LifePoints) > NewWaveHitPoints - aWaveHitPoints Then Begin // Der Gegner ist zu stark -> neuen Suchen
      inc(tries);
      Continue;
    End;
    // Wir haben einen Gegner gefunden den wir hinzu fügen wollen
    tries := 0;
    // schaun ob wir den Gegner schon mal hinzu gefügt haben
    k := -1;
    For j := 0 To high(Waves[n].Opponents) Do Begin
      If Waves[n].Opponents[j].opponent = ExtractFileName(OppListData[i].Filename) Then Begin
        k := j;
        break;
      End;
    End;
    If k = -1 Then Begin // Bisher noch nicht genutzt
      setlength(Waves[n].Opponents, length(Waves[n].Opponents) + 1);
      k := high(Waves[n].Opponents);
      Waves[n].Opponents[k].opponent := ExtractFileName(OppListData[i].Filename);
      Waves[n].Opponents[k].refund := 1 + Random(2 - CreateDifficulty);
      Waves[n].Opponents[k].Count := 0;
      Waves[n].Opponents[k].SpawnDelay := 3000 + random(3 - CreateDifficulty) * 1000;
    End;
    j := (NewWaveHitPoints - aWaveHitPoints) Div powersum(OppListData[i].LifePoints); // Anzahl der Noch Maximal hinzufügbaren Gegner
    j := 1 + random(j); // Mindestens einen hinzunehmen
    Waves[n].Opponents[k].Count := Waves[n].Opponents[k].Count + j;
    Waves[n].Opponents[k].UnitsPerSpawn := max(1, random(Waves[n].Opponents[k].Count Div (1 + random(10))));
    Waves[n].Opponents[k].Spawndelta := 1000 + Waves[n].Opponents[k].UnitsPerSpawn * 10 * (3 - CreateDifficulty);
    aWaveHitPoints := aWaveHitPoints + j * Powersum(OppListData[i].LifePoints); // Mit Rechnen wie viele Hitpoints wir schon eingefügt haben
  End;
  // Alles temporäre frei geben
  For j := 0 To high(BuildingListData) Do Begin
    BuildingListData[j].Free;
  End;
  For j := 0 To high(OppListData) Do Begin
    OppListData[j].Free;
  End;
End;

Function TMap.GetRating: Single;
Begin
  If fRatingCount = 0 Then Begin
    result := 0;
  End
  Else Begin
    result := fRatingSum / fRatingCount;
  End;
End;

Procedure TMap.MoveAllHeroes;
Var
  delta, i, j: integer; // Der Zeit
  n: int64;
  SQR_Min_Dist, ll, wx, wy: Single;
  l, dx, dy: Single;
Begin
  If FPausing Then exit;
  // Die Zeit schreitet voran, in Delta steht wieviel seit dem Letzten mal vergangen ist.
  n := GetTick;
  delta := n - fLastHeroMoveTime;
  delta := delta * Speedup; // Berücksichtigen eines evtl existierenden Speedups
  fLastHeroMoveTime := n;

  For i := high(fheroes) Downto 0 Do Begin
    If (fheroes[i].Level >= 0) And (fheroes[i].targetpos.x <> -1) And (fheroes[i].targetpos.y <> -1) Then Begin
      // Fliegende Gegner sind deutlich einfacher ;)
      // 1. Suchen des "kürzesten" Wegpunktes im WegpunktFeld
      wx := fheroes[i].targetpos.x * MapBlockSize;
      wy := fheroes[i].targetpos.y * MapBlockSize;
      // Offset für die Level Textur mit Rein Rechnen
      wx := wx - fheroes[i].Levels[fheroes[i].Level].w / 2;
      wy := wy - 1 + fheroes[i].Levels[fheroes[i].Level].h / 2;

      ll := sqr(wx - fheroes[i].Position.x) + sqr(wy - fheroes[i].Position.y);
      // 2. Berechnen Delta
      dx := wx - fheroes[i].Position.x;
      dy := wy - fheroes[i].Position.y;
      // 3. Berechnen der Länge in Pixeln
      l := sqrt(sqr(dx) + sqr(dy));
      If l = 0 Then l := 1;
      // 4. Normieren auf 1 Pixel
      dx := dx / l;
      dy := dy / l;
      // 5. Berechnen der Schaffbaren Wegstrecke, bei der Geschwindigkeit die unser Opponent hat
      ll := fheroes[i].levels[fheroes[i].level].Speed * MapBlockSize; // MapBlockSize Pro Sekunde
      ll := ll * delta / 1000; // Pixel in der Vergangenen Zeit.
      // 6. Wenn wir übers Ziel hinausschießen würden (weil der Benutzer den Opponent zu schnell gemacht hat
      If ll > l Then ll := l;
      // 7. Den Wegfortschritt Berechnen
      dx := dx * ll;
      dy := dy * ll;
      // 8. Tatsächlich fliegen
      fheroes[i].Position.x := fheroes[i].Position.x + dx;
      fheroes[i].Position.y := fheroes[i].Position.y + dy;
      fheroes[i].Direction := round(radtodeg(arctan2(-dy, dx)));
      // Haben wir den Wegpunkt ereicht ?
      If (l < 0.1) Then Begin // Wir haben den Wegpunkt Erreicht, nächsten anvisieren
        fheroes[i].targetpos := v2(-1, -1);
      End;
    End
  End;
  // Die Opponents Mögen sich nicht, Einbauen einer gewissen Divergenz
  SQR_Min_Dist := sqr(MinDistanceBetweenHeroes);
  For i := 0 To high(fHeroes) - 1 Do Begin
    For j := i + 1 To high(fHeroes) Do Begin
      l := sqr(fHeroes[i].Position.x - fHeroes[j].Position.x) + sqr(fHeroes[i].Position.y - fHeroes[j].Position.y);
      // Zwei Gegner sind sich zu nahe gekommen und befinden sich in der Selben Ebene (Flug oder Boden)
      If (l <= SQR_Min_Dist) Then Begin
        l := sqrt(l); // Die eigentliche Strecke Ausrechnen
        If l = 0 Then l := 1;
        // Von i nach j
        dx := fHeroes[i].Position.x - fHeroes[j].Position.x;
        dy := fHeroes[i].Position.y - fHeroes[j].Position.y;
        If (dx = 0) And (dy = 0) Then dx := 0.1; // stehen die beiden Gegner exakt aufeinander, dann nehmen wir hier "Künstlich" eine Distanz an.
        dx := dx / l;
        dy := dy / l;
        ll := max(fHeroes[i].Levels[fHeroes[i].Level].Speed, fHeroes[j].levels[fHeroes[j].level].Speed) * MapBlockSize;
        ll := ll * delta / 1000;
        // Beide Auseinanderdrücken
        // Wenn 2 Gegner direkt hintereinander sind und der Vordere Stoppt, und sie in die Gleiche Richtung Wollen, dann
        // Blockiert der Vordere den Hinteren komplett. Damit dies nicht geschieht
        // Werten die beiden Gegner zusätzlich Rechtwinklig zur Bewegungsrichtung mit halber Geschwindigkeit verschoben (2. * ll *0.5 Term)
        fHeroes[i].Position.x := fHeroes[i].Position.x + dx * ll + dy * ll * 0.5;
        fHeroes[i].Position.y := fHeroes[i].Position.y + dy * ll - dx * ll * 0.5;

        // Prüfen ob wir den Gegner nicht ausversehen in eine nicht begehbare Kachel geschoben haben
        fHeroes[j].Position.x := fHeroes[j].Position.x - dx * ll - dy * ll;
        fHeroes[j].Position.y := fHeroes[j].Position.y - dy * ll + dx * ll;
      End;
    End;
  End;
End;

{$ENDIF}

Procedure TMap.LoadGameingData(Const Stream: TStream);
Var
  i, j: integer;
  b: Boolean;
  s: String;
  f: Single;
Begin
  ResetAllBuyedObjects();
  b := false;
  f := 0;
  // Karten Dimensionen
  i := 0;
  stream.Read(i, SizeOf(i));
  If i <> high(fTerrain) + 1 Then exit;
  i := 0;
  stream.Read(i, SizeOf(i));
  If i <> high(fTerrain[0]) + 1 Then exit;
  // Blocked Informationen
  For i := 0 To high(fTerrain) Do
    For j := 0 To high(fTerrain[i]) Do Begin
      stream.read(b, SizeOf(b));
      fTerrain[i, j].blocked := b;
    End;
  i := 0;
  stream.Read(i, SizeOf(i));
  setlength(Fbuildings, i);
  For i := 0 To high(Fbuildings) Do Begin
    Fbuildings[i] := TBuilding.create();
    s := stream.ReadAnsiString;
    Fbuildings[i].LoadFromFile(MapFolder + MapName + PathDelim + s);
    stream.Read(f, sizeof(f));
    Fbuildings[i].Position.x := f;
    stream.Read(f, sizeof(f));
    Fbuildings[i].Position.y := f;
    stream.Read(j, SizeOf(j));
    Fbuildings[i].Stage := j;
    stream.Read(j, SizeOf(j));
    Fbuildings[i].Owner := j;
    stream.Read(Fbuildings[i].strategy, SizeOf(Fbuildings[i].strategy));
    stream.read(b, SizeOf(b));
    Fbuildings[i].PreverAir := b;
  End;
  i := 0;
  stream.Read(i, SizeOf(i));
  setlength(fHeroes, i);
  For i := 0 To high(fHeroes) Do Begin
    fHeroes[i] := THero.create();
    s := stream.ReadAnsiString;
    fHeroes[i].LoadFromFile(MapFolder + MapName + PathDelim + s);
    stream.Read(f, sizeof(f));
    fHeroes[i].Position.x := f;
    stream.Read(f, sizeof(f));
    fHeroes[i].Position.y := f;
    stream.Read(j, SizeOf(j));
    fHeroes[i].Level := j;
    stream.Read(j, SizeOf(j));
    fHeroes[i].CollectedDamages := j;
    stream.Read(j, SizeOf(j));
    fHeroes[i].Direction := j;
    stream.Read(j, SizeOf(j));
    fHeroes[i].Owner := j;
    stream.Read(fHeroes[i].strategy, SizeOf(fHeroes[i].strategy));
    stream.read(b, SizeOf(b));
    fHeroes[i].PreverAir := b;
  End;
  SortBuildingsByYCoordinate; // Notwendig, damit es beim Rendern "schöner" aussieht
  SortPlacementByYCoordinate; // Notwendig, damit es beim Rendern "schöner" aussieht
End;

Procedure TMap.Pause(value: Boolean);
Var
{$IFDEF Server}
  i: int64;
{$ENDIF}
  j: integer;
Begin
{$IFDEF Server}
  If FPausing Then Begin
    If Not value Then Begin
      i := GetTick() - fPauseTime;
      fLastOppMoveTime := fLastOppMoveTime + i; // Wir Verschieben die StartZeit um die Zeit der Pause in die Zukunft
      fLastHeroMoveTime := fLastHeroMoveTime + i; // Wir Verschieben die StartZeit um die Zeit der Pause in die Zukunft
      fLastBulletTime := fLastBulletTime + i; // Wir Verschieben die StartZeit um die Zeit der Pause in die Zukunft
      fHandleBuildingTime := fHandleBuildingTime + i;
    End;
  End
  Else Begin
    If value Then Begin
      fPauseTime := GetTick();
    End;
  End;
  FPausing := value;
{$ENDIF}
  // Propagieren in alle Gebäude
  For j := 0 To high(Fbuildings) Do Begin
    Fbuildings[j].pause(value);
  End;
  For j := 0 To high(fHeroes) Do Begin
    fHeroes[j].Pause(value);
  End;
End;

Function TMap.RemoveBuilding(x, y, Owner: integer): Boolean;
Var
  xx, yy, i, j: integer;
{$IFDEF Server}
  sc: Boolean;
{$ENDIF}
Begin
  result := false;
  For i := 0 To high(Fbuildings) Do
    If (round(Fbuildings[i].Position.x) = x) And
      (round(Fbuildings[i].Position.y) = y) And
      ((Owner = -1) Or (Fbuildings[i].Owner = Owner)) Then Begin
      result := true;
{$IFDEF Server}
      sc := false;
{$ENDIF}
      // Zurücknehmen der Blocked
      For x := 0 To round(Fbuildings[i].Width) - 1 Do
        For y := 0 To round(Fbuildings[i].Height) - 1 Do Begin
          xx := min(round(Fbuildings[i].Position.x) + x, High(fTerrain));
          yy := max(0, round(Fbuildings[i].Position.y) - y);
          fTerrain[xx, yy].blocked := false;
{$IFDEF Server}
          If (fTerrain[xx, yy].data And Walkable) <> 0 Then sc := true;
{$ENDIF}
        End;
{$IFDEF Server}
      // Wenn irgend ein Pfad wieder Freigegeben wurde, müssen die Wegpfade auch alle wieder neu berechnet werden
      If sc Then Begin
        CalcOpponentPaths();
      End;
      // Löschen aller Bullets die evtl. noch von diesem Gebäude aus unterwegs sind.
      For j := high(FBullets) Downto 0 Do Begin
        If FBullets[j].Owner = Fbuildings[i] Then Begin
          FBullets[j].Free;
          For x := j To high(FBullets) - 1 Do Begin
            FBullets[x] := FBullets[x + 1];
          End;
          setlength(FBullets, high(FBullets));
        End;
      End;
{$ENDIF}
      // Löschen aus der FBuildings
      Fbuildings[i].free;
      For j := i To high(Fbuildings) - 1 Do Begin
        Fbuildings[j] := Fbuildings[j + 1];
      End;
      setlength(Fbuildings, high(Fbuildings));
      break;
    End;
End;

Procedure TMap.deletePlacement(x, y: integer);
Var
  i: Integer;
  j: Integer;
Begin
  For i := 0 To high(fPlacements) Do Begin
    If (round(fPlacements[i].Position.x) = x) And
      (round(fPlacements[i].Position.y) = y) Then Begin
      fPlacements[i].free;
      For j := i To high(fPlacements) - 1 Do Begin
        fPlacements[j] := fPlacements[j + 1];
      End;
      setlength(fPlacements, high(fPlacements));
      break;
    End;
  End;
End;

Procedure TMap.addPlacement(x, y: integer; ObjName: String; Stage: integer);
Var
  pp, p: tctd_mapopbject;
  i: Integer;
  j: Integer;
Begin
  log(format('TMap.addPlacement %d/%d, %s', [x, y, ObjName]), llTrace);
  If (x > Width) Or (y > height) Then exit;
  ObjName := MapFolder + MapName + PathDelim + ObjName;
  Case (FilenameToType(ObjName)) Of
    moBuilding: Begin
        p := TBuilding.create();
      End;
    moOpponent: Begin
        p := TOpponent.create();
      End;
    moUnknown: Begin
        log('unable to identifier : ' + ObjName, llError);
        LogLeave;
        exit;
      End;
  End;
  p.LoadFromFile(ObjName);
  If p Is TBuilding Then Begin
    TBuilding(p).stage := min(Stage, high(TBuilding(p).Stages)); // Vorsichtshalber
    TBuilding(p).Owner := -1;
  End;
  // Suchen ob es Objekte gibt, die kollidieren, wenn Ja, Löschen
  For i := 0 To Ceil(p.Width) - 1 Do
    For j := 0 To Ceil(p.Height) - 1 Do Begin
      pp := PlacementAtCoord(x + i, y - j);
      If assigned(pp) Then Begin
        deletePlacement(round(pp.position.x), round(pp.position.y));
      End;
    End;
  p.Position := point(x, y);
  If p Is TOpponent Then Begin
    p.Width := TOpponent(p).SizeX;
    p.Height := TOpponent(p).Sizey;
  End;
  // Sortieren nach y- Koordinate
  setlength(fPlacements, high(fPlacements) + 2);
  fPlacements[high(fPlacements)] := p;
  SortPlacementByYCoordinate;
End;

Procedure TMap.addBuyable(Item: String; Kind: TBuyAbleKind; Wave, Count: integer
  );
Var
  i: Integer;
Begin
  // TODO: Prüfen ob wir hier auch noch den Kind brauchen ...
  // Verhindern das das "Selbe" Item 2 mal hinzugefügt wird
  For i := 0 To high(fBuyAbles) Do Begin
    If fBuyAbles[i].Item = Item Then Begin
      exit;
    End;
  End;
  setlength(fBuyAbles, high(fBuyAbles) + 2);
  fBuyAbles[high(fBuyAbles)].Item := Item;
  fBuyAbles[high(fBuyAbles)].WaveNum := Wave;
  fBuyAbles[high(fBuyAbles)].Count := Count;
  fBuyAbles[high(fBuyAbles)].Kind := Kind;
  FixBuyableSorting();
End;

Procedure TMap.delBuyable(Item: String; Wave, Count: integer);
Var
  i, j: Integer;
Begin
  // TODO: Prüfen ob wir hier auch noch den Kind brauchen ...
  For i := 0 To high(fBuyAbles) Do Begin
    If (fBuyAbles[i].Item = Item) And
      (fBuyAbles[i].WaveNum = Wave) And
      (fBuyAbles[i].Count = Count) Then Begin
      For j := i To high(fBuyAbles) - 1 Do Begin
        fBuyAbles[j] := fBuyAbles[j + 1];
      End;
      setlength(fBuyAbles, high(fBuyAbles));
      break;
    End;
  End;
End;

Procedure TMap.updateBuyable(Item: String; Wave, Count: integer);
Var
  i: Integer;
Begin
  // TODO: Prüfen ob wir hier auch noch den Kind brauchen ...
  For i := 0 To high(fBuyAbles) Do Begin
    If (fBuyAbles[i].Item = Item) Then Begin
      fBuyAbles[i].WaveNum := Wave;
      fBuyAbles[i].Count := Count;
      FixBuyableSorting();
      break;
    End;
  End;
End;

Function TMap.delOpponent(Opp: String): Boolean;
Var
  ap: String;
  osl, sl: TStringlist;
  op: TOpponent;
  i, j: Integer;
Begin
  result := true;
  ap := MapFolder + MapName + PathDelim;
  // Löschen aller Dateien, ggf auch aus den Waves
  // 1. Laden des Opponents
  op := TOpponent.create();
  op.LoadFromFile(ap + opp);
  // 2. Dateiliste erstellen
  osl := op.ListOfImages();
  op.free;
  // 3. Karte -> Liste Aller Dateien die es Gibt ->
  sl := GetListOfAllUsedFiles(Opp);
  // Alle osl die nicht in sl sind können nun gelöscht werden ;)
  For i := 0 To osl.Count - 1 Do Begin
    If pos(ap + osl[i], sl.Text) <> 0 Then Continue; // Die Datei wird offensichtlich von irgendwas benötigt.
    If FileExistsUTF8(ap + osl[i]) And (Not DeleteFileUTF8(ap + osl[i])) Then result := false;
  End;
  // Durch Alle Placements durch
  For i := high(fPlacements) Downto 0 Do Begin
    If fPlacements[i].Filename = ap + Opp Then Begin
      fPlacements[i].Free;
      For j := i To high(fPlacements) - 1 Do Begin
        fPlacements[j] := fPlacements[j + 1];
      End;
      setlength(fPlacements, high(fPlacements));
    End;
  End;
  If FileExistsUTF8(ap + Opp) And (Not DeleteFileUTF8(ap + Opp)) Then result := false;
  opp := ExtractFileName(opp);
  // Durch alle Waves gehen, wenn dort der Opp drin ist den "Löschen"
  For i := 0 To high(Waves) Do Begin
    For j := high(waves[i].Opponents) Downto 0 Do Begin
      If Waves[i].Opponents[j].opponent = opp Then Begin
        // hier könnte man den Opponent auch "Raus" Löschen, doch so kriegt der user
        // beim Nächsten Check Map ne Fehlermeldung und merkt, dass er den Opp vielleicht lieber nicht gelöscht hätte.
        Waves[i].Opponents[j].opponent := '';
      End;
    End;
  End;
  osl.free;
  sl.free;
End;

Function TMap.delBuilding(Geb: String): Boolean;
Var
  ap: String;
  osl, sl: TStringlist;
  b: TBuilding;
  i, j: Integer;
Begin
  result := true;
  ap := MapFolder + MapName + PathDelim;
  // Löschen aller Dateien, ggf. auch aus den Waves
  // 1. Laden des Gebäudes
  b := TBuilding.create();
  b.LoadFromFile(ap + Geb);
  // 2. Dateiliste erstellen
  osl := b.ListOfImages();
  b.free;
  // 3. Karte -> Liste Aller Dateien die es Gibt ->
  sl := GetListOfAllUsedFiles(Geb);
  // Alle osl die nicht in sl sind können nun gelöscht werden ;)
  For i := 0 To osl.Count - 1 Do Begin
    If pos(ap + osl[i], sl.Text) <> 0 Then Continue; // Die Datei wird offensichtlich von irgendwas benötigt.
    If FileExistsUTF8(ap + osl[i]) And (Not DeleteFileUTF8(ap + osl[i])) Then result := false;
  End;
  // Durch Alle Placements durch
  For i := high(fPlacements) Downto 0 Do Begin
    If fPlacements[i].Filename = ap + geb Then Begin
      fPlacements[i].Free;
      For j := i To high(fPlacements) - 1 Do Begin
        fPlacements[j] := fPlacements[j + 1];
      End;
      setlength(fPlacements, high(fPlacements));
    End;
  End;
  // Durch alle BuyAbles durch
  For i := high(fBuyAbles) Downto 0 Do Begin
    If fBuyAbles[i].Item = geb Then Begin
      For j := i To high(fBuyAbles) - 1 Do Begin
        fBuyAbles[j] := fBuyAbles[j + 1];
      End;
      setlength(fBuyAbles, high(fBuyAbles));
    End;
  End;
  If FileExistsUTF8(ap + Geb) And (Not DeleteFileUTF8(ap + Geb)) Then result := false;
  osl.free;
  sl.free;
End;

Function TMap.delHero(Hero: String): Boolean;
Var
  ap: String;
  osl, sl: TStringlist;
  h: THero;
  i, j: Integer;
Begin
  result := true;
  ap := MapFolder + MapName + PathDelim;
  // Löschen aller Dateien, ggf. auch aus den Waves
  // 1. Laden des Gebäudes
  h := Thero.create();
  h.LoadFromFile(ap + Hero);
  // 2. Dateiliste erstellen
  osl := h.ListOfImages();
  h.free;
  // 3. Karte -> Liste Aller Dateien die es Gibt ->
  sl := GetListOfAllUsedFiles(Hero);
  // Alle osl die nicht in sl sind können nun gelöscht werden ;)
  For i := 0 To osl.Count - 1 Do Begin
    If pos(ap + osl[i], sl.Text) <> 0 Then Continue; // Die Datei wird offensichtlich von irgendwas benötigt.
    If FileExistsUTF8(ap + osl[i]) And (Not DeleteFileUTF8(ap + osl[i])) Then result := false;
  End;
  // Durch Alle Placements durch
  For i := high(fPlacements) Downto 0 Do Begin
    If fPlacements[i].Filename = ap + Hero Then Begin
      fPlacements[i].Free;
      For j := i To high(fPlacements) - 1 Do Begin
        fPlacements[j] := fPlacements[j + 1];
      End;
      setlength(fPlacements, high(fPlacements));
    End;
  End;
  // Durch alle BuyAbles durch
  For i := high(fBuyAbles) Downto 0 Do Begin
    If fBuyAbles[i].Item = Hero Then Begin
      For j := i To high(fBuyAbles) - 1 Do Begin
        fBuyAbles[j] := fBuyAbles[j + 1];
      End;
      setlength(fBuyAbles, high(fBuyAbles));
    End;
  End;
  If FileExistsUTF8(ap + Hero) And (Not DeleteFileUTF8(ap + Hero)) Then result := false;
  osl.free;
  sl.free;
End;

Procedure TMap.DelOppInWave(WaveNum, OppNum: integer);
Var
  i: Integer;
Begin
  For i := OppNum To high(Waves[WaveNum].Opponents) - 1 Do Begin
    Waves[WaveNum].Opponents[i] := Waves[WaveNum].Opponents[i + 1];
  End;
  setlength(Waves[WaveNum].Opponents, max(0, high(Waves[WaveNum].Opponents)));
End;

Function TMap.AddBuilding(x, y: integer; Const building: TBuilding): Boolean;
Var
  i, j: Integer;
{$IFDEF Server}
  sc: Boolean; // Merker ob irgendwelche Felder die begehbarkeit verlieren
{$ENDIF}
  nam, s: String;
Begin
  // Prüfen ob überhaupt noch Gebäude der Art gebaut werden dürfen
  nam := ExtractFileName(building.Filename);
{$IFDEF Client}
  result := true;
{$ENDIF}
{$IFDEF Server}
  j := -1;
  For i := 0 To high(fBuyAbles) Do Begin
    If fBuyAbles[i].Item = nam Then Begin
      j := fBuyAbles[i].Count;
      break;
    End;
  End;
  (*
   * Hier wurde versucht ein Gebäude zu Adden, was gar nicht Kaufbar ist.
   *)
  If j = -1 Then Raise exception.create('Oh oh, BUG in TMap.AddBuilding !!');
  // prüfen auf maximal erlaubte Anzahl
  If j > 0 Then Begin
    For i := 0 To high(Fbuildings) Do Begin
      If (Fbuildings[i].Filename = building.Filename) And (Fbuildings[i].Owner = building.Owner) Then dec(j);
    End;
    If j <= 0 Then Begin
      result := false;
      exit;
    End;
  End;
  result := CoordIsBuildable(x, y, building);
  If result Then Begin // Prinzipiell ist das Objekt an der Stelle Baubar
    // Probeweise einfügen in die Karte
    sc := true; // Merken ob überhaupt irgend ein Begehbares Feld Gelöscht wird
{$ENDIF}
    setlength(Fbuildings, high(Fbuildings) + 2);
    Fbuildings[high(Fbuildings)] := building;
    // Löschen und Bakup der Begehbarkeit
    For i := 0 To round(building.Width) - 1 Do
      For j := 0 To round(building.Height) - 1 Do Begin
{$IFDEF Server}
        If (fTerrain[x + i, y - j].data And Walkable) <> 0 Then sc := false; // Jop, mindestens ein Feld wird nachher nicht mehr begehbar sein
{$ENDIF}
        fTerrain[x + i, y - j].blocked := true;
      End;
{$IFDEF Server}
    // Neue Wegpfade Berechnen
    If Not sc Then Begin // Ist True, wenn kein Fehld seine Begehbarkeit ändert, dann brauchen wir auch keine neuen Wege berechnen
      result := CalcOpponentPaths();
    End;
    If result Then Begin
{$ENDIF}
      // Das Gebäude kann Plaziert werden, ab jetzt kümmert sich die Karte darum
      // Weitere Objekte können hier natürlich nicht mehr gebaut werden, also Sperren wir das Bebaubarflag
      Fbuildings[high(Fbuildings)].Position.x := x;
      Fbuildings[high(Fbuildings)].Position.y := y;
      // Das Gebäude ist natürlich Sofort Schusbereit
//      Fbuildings[high(Fbuildings)].LastShootTime := GetTick() - Fbuildings[high(Fbuildings)].Stages[Fbuildings[high(Fbuildings)].Stage].reloadtime;
      Fbuildings[high(Fbuildings)].LastShootTime := GetTick() - Fbuildings[high(Fbuildings)].Stages[0].reloadtime;
      // Suchen des BulletImages für das Spätere Rendering
      For i := 0 To high(building.Stages) Do Begin
        s := nam + '_' + inttostr(i);
        For j := 0 To high(FBulletIndexes) Do Begin
          If s = FBulletIndexes[j].Name Then Begin
            building.Stages[i].fbulletimage := j;
            break;
          End;
        End;
      End;
      building.Stage := -1;
      building.IncStage; // des Gebäude nachträglich Initialisieren ;)
      SortBuildingsByYCoordinate;
      // Todo : Alle Bullets die kein Target haben (also Belagerungsschaden machen) und unterhalb des Gebäudes liegen müssen nun gelöscht werden
{$IFDEF Server}
    End
    Else Begin
      // das Gebäude würde einen Wegpfad ruinieren, also wieder Rückgängig machen.
      setlength(Fbuildings, high(Fbuildings));
      // Wieder Herstellen der Begehbarkeit
      For i := 0 To round(building.Width) - 1 Do
        For j := 0 To round(building.Height) - 1 Do Begin
          fTerrain[x + i, y - j].blocked := false;
        End;
      CalcOpponentPaths();
    End;
  End;
{$ENDIF}
End;

Function TMap.AddHero(x, y: integer; Const hero: THero): Boolean;
Var
  i, j: Integer;
  //{$IFDEF Server}
  //  sc: Boolean; // Merker ob irgendwelche Felder die begehbarkeit verlieren
  //{$ENDIF}
  nam, s: String;
Begin
  // Prüfen ob überhaupt noch Gebäude der Art gebaut werden dürfen
  nam := ExtractFileName(hero.Filename);
{$IFDEF Client}
  result := true;
{$ENDIF}
{$IFDEF Server}
  j := -1;
  For i := 0 To high(fBuyAbles) Do Begin
    If fBuyAbles[i].Item = nam Then Begin
      j := fBuyAbles[i].Count;
      break;
    End;
  End;
  (*
   * Hier wurde versucht ein Held zu Adden, was gar nicht Kaufbar ist.
   *)
  If j = -1 Then Raise exception.create('Oh oh, BUG in TMap.AddHero !!');
  // prüfen auf maximal erlaubte Anzahl
  If j > 0 Then Begin
    For i := 0 To high(fHeroes) Do Begin
      If (fHeroes[i].Filename = hero.Filename) And (fHeroes[i].Owner = hero.Owner) Then dec(j);
    End;
    If j <= 0 Then Begin
      result := false;
      exit;
    End;
  End;
  result := true; // CoordIsBuildable(x, y, building); -- Heroes haben keinen Buildable Check

  // einfügen in die Karte
{$ENDIF}
  setlength(fHeroes, high(fHeroes) + 2);
  fHeroes[high(fHeroes)] := hero;
  fHeroes[high(fHeroes)].MapHeroIndex := high(fHeroes);
  // Das Gebäude kann Plaziert werden, ab jetzt kümmert sich die Karte darum
  // Weitere Objekte können hier natürlich nicht mehr gebaut werden, also Sperren wir das Bebaubarflag
  fHeroes[high(fHeroes)].Position.x := x;
  fHeroes[high(fHeroes)].Position.y := y;
  // Der Held ist natürlich Sofort Schusbereit
  fHeroes[high(fHeroes)].LastShootTime := GetTick() - fHeroes[high(fHeroes)].Levels[0].reloadtime;
  // Suchen des BulletImages für das Spätere Rendering
  For i := 0 To high(Hero.Levels) Do Begin
    s := nam + '_' + inttostr(i);
    For j := 0 To high(FBulletIndexes) Do Begin
      If s = FBulletIndexes[j].Name Then Begin
        Hero.Levels[i].fbulletimage := j;
        break;
      End;
    End;
  End;
{$IFDEF Server}
  Hero.level := -1;
  // Initialisieren der Build Sequenz
  Hero.IncLevel;
{$ENDIF}
End;

Procedure TMap.CloneWave(SourceWaveNum: integer);
Var
  i, DestWaveNum: Integer;
Begin
  setlength(Waves, high(Waves) + 2);
  DestWaveNum := high(Waves);
  Waves[DestWaveNum].ChashOnStart := Waves[SourceWaveNum].ChashOnStart;
  Waves[DestWaveNum].WaveHint := Waves[SourceWaveNum].WaveHint;
  setlength(Waves[DestWaveNum].Opponents, length(Waves[SourceWaveNum].Opponents));
  For i := 0 To high(Waves[SourceWaveNum].Opponents) Do Begin
    Waves[DestWaveNum].Opponents[i] := Waves[SourceWaveNum].Opponents[i];
  End;
End;

Procedure TMap.CreateMovableObjectList(Wave: integer);
Var
  i, j: Integer;
  b: TBuilding;
  h: THero;
{$IFDEF Client}
  op: TOpponent;
{$ENDIF}
{$IFDEF Server}
  k: integer;
  nam,
{$ENDIF}
  p: String;
Begin
  log('TMap.CreateMovableObjectList : ' + inttostr(wave), lltrace);
{$IFDEF Client}
  If assigned(FIndexMapper) Then Begin
    LogLeave;
    exit; // Wenn die Liste aus irgend einem Grund schon erstellt wurde.
  End;
  // löschen evtl noch vorhandener Moveable Renderings und anderer Alter Daten
  setlength(fRenderOpponents, 0);
  setlength(fRenderBullets, 0);
  // Neu erstellen derIndex Listen für die Nächste Runde
  setlength(FIndexMapper, length(Waves[Wave].Opponents));
  p := MapFolder + MapName + PathDelim;
  For i := 0 To high(Waves[wave].Opponents) Do Begin
    op := TOpponent.create();
    op.LoadFromFile(p + Waves[Wave].Opponents[i].opponent);
    If lowercase(ExtractFileExt(op.Image)) = '.ani' Then Begin
      FIndexMapper[i].Image.image := 0; // Die haben Ihre Animationen, die wiederrum vom Server gesteuert werden.
    End
    Else Begin
      FIndexMapper[i].Image := OpenGL_GraphikEngine.LoadAlphaGraphikItem(p + op.Image, smClamp);
    End;
    FIndexMapper[i].SizeX := op.SizeX;
    FIndexMapper[i].Sizey := op.Sizey;
    FIndexMapper[i].TotalLifePoints := op.TotalLivepoints;
    FIndexMapper[i].CanFly := op.Canfly;
    FIndexMapper[i].Hint := DeSerialize(op.Description);
    FIndexMapper[i].Obj := op;
  End;
{$ENDIF}
  For i := 0 To high(fBuyAbles) Do Begin
    Case fBuyAbles[i].Kind Of
      bkBuilding: Begin
          b := TBuilding.create();
          b.LoadFromFile(MapFolder + MapName + PathDelim + fBuyAbles[i].Item);
          For j := 0 To high(b.Stages) Do Begin
            setlength(FBulletIndexes, high(FBulletIndexes) + 2);
            FBulletIndexes[high(FBulletIndexes)].name := fBuyAbles[i].Item + '_' + inttostr(j);
            FBulletIndexes[high(FBulletIndexes)].fimage := b.Stages[j].fbulletimage;
{$IFDEF Client}
            If assigned(b.Stages[j].BulletAnimation) Then Begin
              FBulletIndexes[high(FBulletIndexes)].Animation := TOpenGL_Animation.create;
              FBulletIndexes[high(FBulletIndexes)].Animation.CloneFrom(b.Stages[j].BulletAnimation);
            End
            Else Begin
              FBulletIndexes[high(FBulletIndexes)].Animation := Nil;
            End;
{$ENDIF}
            FBulletIndexes[high(FBulletIndexes)].Width := b.Stages[j].bulletw;
            FBulletIndexes[high(FBulletIndexes)].Height := b.Stages[j].bulleth;
            FBulletIndexes[high(FBulletIndexes)].Speed := max(0, b.Stages[j].bulletspeed);
          End;
          b.free;
        End;
      bkHero: Begin
          h := THero.create();
          h.LoadFromFile(MapFolder + MapName + PathDelim + fBuyAbles[i].Item);
          For j := 0 To high(h.Levels) Do Begin
            setlength(FBulletIndexes, high(FBulletIndexes) + 2);
            FBulletIndexes[high(FBulletIndexes)].name := fBuyAbles[i].Item + '_' + inttostr(j);
            FBulletIndexes[high(FBulletIndexes)].fimage := h.Levels[j].fbulletimage;
{$IFDEF Client}
            If assigned(h.Levels[j].BulletAnimation) Then Begin
              FBulletIndexes[high(FBulletIndexes)].Animation := TOpenGL_Animation.create;
              FBulletIndexes[high(FBulletIndexes)].Animation.CloneFrom(h.Levels[j].BulletAnimation);
            End
            Else Begin
              FBulletIndexes[high(FBulletIndexes)].Animation := Nil;
            End;
{$ENDIF}
            FBulletIndexes[high(FBulletIndexes)].Width := h.Levels[j].bulletw;
            FBulletIndexes[high(FBulletIndexes)].Height := h.Levels[j].bulleth;
            FBulletIndexes[high(FBulletIndexes)].Speed := max(0, h.Levels[j].bulletspeed);
          End;
          h.free;
        End;
    Else Begin
        Raise exception.create('TMap.CreateMovableObjectList: missing implementation');
      End;
    End;
  End;
{$IFDEF Server}
  // Wenn die Karte via LoadGame geladen wurde, dann gibt es schon Gebäude, diese müssen genau Gleich wie bei Addbuilding mit den richtigen
  // Texturinformationen versorgt werden
  For k := 0 To high(Fbuildings) Do Begin
    nam := ExtractFileName(Fbuildings[k].Filename);
    For i := 0 To high(Fbuildings[k].Stages) Do Begin
      p := nam + '_' + inttostr(i);
      For j := 0 To high(FBulletIndexes) Do Begin
        If p = FBulletIndexes[j].Name Then Begin
          Fbuildings[k].Stages[i].fbulletimage := j;
          break;
        End;
      End;
    End;
  End;
{$ENDIF}
  LogLeave;
End;

Procedure TMap.ExChangeWaves(w1, w2: integer);
Var
  tmp: TWave;
Begin
  tmp := Waves[w1];
  Waves[w1] := Waves[w2];
  Waves[w2] := tmp;
End;

Function TMap.GetObjUnderCursor(x, y: integer): tctd_mapopbject;
Var
  xx: Integer;
  yy: Integer;
{$IFDEF Client}
  lev,
{$ENDIF}
  i: Integer;
Begin
  // umrechnen Pixel in Grid Coodrinaten
  xx := x Div MapBlockSize;
  yy := y Div MapBlockSize;
  // Suchen aller Placements
  result := PlacementAtCoord(xx, yy);
  If Not assigned(result) Then Begin
    // Suchen aller Gebäude
    For i := 0 To high(Fbuildings) Do Begin
      If (Fbuildings[i].Position.x <= xx) And
        (Fbuildings[i].Position.x + Fbuildings[i].Width > xx) And
        (Fbuildings[i].Position.y - Fbuildings[i].Height < yy) And
        (Fbuildings[i].Position.y >= yy) Then Begin
        result := Fbuildings[i];
        exit;
      End;
    End;
{$IFDEF Client}
    If Not assigned(FIndexMapper) And assigned(fRenderOpponents) Then Begin
      log('Tmap.Render : uninitialzed FIndexMapper, deleting all opponents.', llCritical);
      setlength(fRenderOpponents, 0);
    End;
    // Suchen aller Opponents
    For i := 0 To high(fRenderOpponents) Do Begin
      // Der Kollisionsrahmen ist ein bischen Größer wie das eigentliche Objekt, aber vorerst soll uns das genügen.
      If ((fRenderOpponents[i].Position.x - FIndexMapper[fRenderOpponents[i].Index].SizeX / 2) * MapBlockSize <= x) And
        ((fRenderOpponents[i].Position.x + FIndexMapper[fRenderOpponents[i].Index].SizeX / 1) * MapBlockSize >= x) And
        ((fRenderOpponents[i].Position.y - FIndexMapper[fRenderOpponents[i].Index].Sizey / 2) * MapBlockSize <= y) And
        ((fRenderOpponents[i].Position.y + FIndexMapper[fRenderOpponents[i].Index].Sizey / 1) * MapBlockSize >= y) Then Begin
        TOpponent(FIndexMapper[fRenderOpponents[i].Index].Obj).LifePoints := fRenderOpponents[i].LifePoints;
        TOpponent(FIndexMapper[fRenderOpponents[i].Index].Obj).Identifier := fRenderOpponents[i].Identifier;
        result := FIndexMapper[fRenderOpponents[i].Index].Obj;
        exit;
      End;
    End;
    For i := 0 To high(fHeroes) Do Begin
      lev := max(0, fHeroes[i].level); // So können auch Helden angewählt werden die gerade gebaut werden
      If ((fHeroes[i].Position.x + 0) * MapBlockSize <= x) And
        ((fHeroes[i].Position.x + fHeroes[i].Levels[lev].w) * MapBlockSize >= x) And
        ((fHeroes[i].Position.y + 1) * MapBlockSize >= y) And
        ((fHeroes[i].Position.y + 1 - fHeroes[i].Levels[lev].h) * MapBlockSize <= y) Then Begin
        result := fHeroes[i];
        exit;
      End;
    End;
{$ENDIF}
  End;
End;

{$IFDEF Client}

Function TMap.getAirLevels: String;
Var
  i, j: integer;
  op: TOpponent;
Begin
  result := '';
  For i := 0 To high(Waves) Do Begin
    For j := 0 To high(waves[i].Opponents) Do Begin
      op := TOpponent.create();
      op.LoadFromFile(MapFolder + MapName + PathDelim + waves[i].Opponents[j].opponent);
      If op.Canfly Then Begin
        If result = '' Then Begin
          result := inttostr(i + 1);
        End
        Else Begin
          result := result + ', ' + inttostr(i + 1);
        End;
        op.free;
        break;
      End;
      op.free;
    End;
  End;
End;

Function TMap.getBonusLevels: String;
Var
  i, j: integer;
  op: TOpponent;
Begin
  result := '';
  For i := 0 To high(Waves) Do Begin
    For j := 0 To high(waves[i].Opponents) Do Begin
      op := TOpponent.create();
      op.LoadFromFile(MapFolder + MapName + PathDelim + waves[i].Opponents[j].opponent);
      If op.Bonus Then Begin
        If result = '' Then Begin
          result := inttostr(i + 1);
        End
        Else Begin
          result := result + ', ' + inttostr(i + 1);
        End;
        op.free;
        break;
      End;
      op.free;
    End;
  End;
End;

Function TMap.getBossLevels: String;
Var
  i, j: integer;
  op: TOpponent;
Begin
  result := '';
  For i := 0 To high(Waves) Do Begin
    For j := 0 To high(waves[i].Opponents) Do Begin
      op := TOpponent.create();
      op.LoadFromFile(MapFolder + MapName + PathDelim + waves[i].Opponents[j].opponent);
      If op.Boss Then Begin
        If result = '' Then Begin
          result := inttostr(i + 1);
        End
        Else Begin
          result := result + ', ' + inttostr(i + 1);
        End;
        op.free;
        break;
      End;
      op.free;
    End;
  End;
End;

{$ENDIF}
End.


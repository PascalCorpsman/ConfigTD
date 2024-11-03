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
Unit uctd_opp;

{$MODE objfpc}{$H+}

{$I apptype.inc}

(* -- Lazarus Bug in Auswertung von apptype.inc -- Dieser Block darf "aktiviert" nie ins GIT
{$UNDEF Client}
{$DEFINE Server}
//*)

Interface

Uses
  Classes, SysUtils, uctd_mapobject, uctd_common, uvectormath
{$IFDEF client}
  , uopengl_animation
{$ENDIF}
  ;

Type

  TSlowObject = Record
    obj: tctd_mapopbject;
    AddTime: int64;
    DynTime: integer; // Die Zeit wie Lange der Dynamische Teil wirkt
    DynValue: Single;
    StatValue: Single;
  End;

  { TOpponent }

  TOpponent = Class(tctd_mapopbject)
  private
    fTotalLivepoints: integer; // Summe über alle Lebenspunkte ohne Skallierung
    fStaticSlowingObjects: Array Of TSlowObject; // Eigentlich nur TBuilding
  public
{$IFDEF client}
    Animation: TOpenGL_Animation; // Nur Read Only Bitte !!
{$ENDIF}
{$IFDEF Server}
    HasAnimation: Boolean; // Der Server muss nicht wissen welche Animation, nur Dass es eine ist ;)
    AnimationOffset: uInt16;
    DamageByPlayers: Array Of Integer; // Der Schaden getrennt nach Spieler -> Zur Bestimmung, wer den Benefit beim Kill bekommt !
{$ENDIF}
    RenderIndex: uInt16; // Index für FIndexMapper
    Description: String;
    LifePoints: TPower;
    LifeFactors: Array[0..2] Of Single;
    Speed: single;
    SizeX: single;
    SizeY: single;
    ImageRotation: Boolean; // Wenn True, dann wird die Graphik entsprechend der Winkelgrade gedreht ausgegeben
    Canfly: Boolean;
    Boss: Boolean;
    Bonus: Boolean;
    Identifier: uint16;
    Direction: integer; // Die Richtung in die der Gegner geht (Angabe in Winkelgrad, wird von Map.MoveAllOpponents gesetzt)
    // Geld, welches der Spieler bekommt, wenn er eine Einheit platt macht,
    // wird durch die Karte gesetzt. Und braucht nicht gespeichert zu werden.
    Refund: integer;
    ShowLifePoints: Boolean; // Wenn True, dann wird ein "Leben" Balken über dem Spieler gezeigt.
    Property TotalLivepoints: integer read fTotalLivepoints;
    Constructor create(); override;
    Destructor destroy; override;

    Function LoadFromStream(Const Stream: TStream): Boolean; override;
    Function SaveToStream(Const Stream: TStream): Boolean; override;

    Function ListOfImages(): TStringList; override;

    Function GetInGameSpeed: Single; // Ermittelt Speed unter berücksichtigung evtl. Existierender "Slow" faktoren

    Function SlowDown(Const value: tctd_BulletObject): Boolean; // Unsinnig, das hier was zurück gegeben wird.
    Function isSlowing(Const value: tctd_mapopbject; Out DynamicSlowing: Boolean): integer;

{$IFDEF Server}
    Procedure GetMovingState(Const Stream: TSTream);
    Procedure InitDamageByPlayers(Count: integer);
{$ENDIF}

{$IFDEF client}
    Procedure AssignDataFromObj(Const obj: TOpponent);
    Function GetBuyCost: integer; override; // Wird für gegener nicht benötigt

    Function GetHint(): THint; override;
    Procedure Render(Grayed: Boolean); override; // Ist nur wenn Placements gerendert werden, evtl, kann man das ja noch Umbauen, dass es nicht Redundant drin ist ?
    Function Check(): String; // Prüft den Gegner auf gültigkeit, '' wenn Fehlerfrei, sonst ist das der Fehlercode
{$ENDIF}

  End;

  // Todo : Eigentlich ist das eine Datenstructur der Karte und nicht der Bullets, gibts da nen Trick wie man wieder in die map bekommt?
  TOpponentInfo = Record
    Obj: TOpponent;
    // Beim Diagonalen Gehen, kann es vorkommen, das die Figur auf einen nicht
    // begehbaren Block läuft (beim Umlaufen von Ecken), das geschieht aber nur im Diagonalen Modus.
    // Wenn dies Erkannt wird, verhindern wir das Diagonale gehen bei der nächsten
    // Bewegung.
    // Die Blockierung bleibt so lange an, bis wir uns eine Kachelbreite weiter
    // Bewegt haben
    NextTimeNoDiagWalk: Boolean;
    NextTimeNoDiagWalkPos: tvector2;
    NextWayPoint: integer; // Index des als nächstes an zu laufenden Wegpunktes
    DistanzeToGoal: Single; // Distanz zum Ziel für Gebäudestrategie first / last wird in TMap.MoveAllOpponents berechnet
  End;

  TOpponentInfoArray = Array Of TOpponentInfo;

Implementation

Uses LCLIntf, IniFiles, LazFileUtils, math
{$IFDEF client}
  , dglOpenGL
  , uopengl_graphikengine
  , ugraphics
  , Graphics
{$ENDIF}
  , uctd_bullet;

{ TOpponent }

Function TOpponent.isSlowing(Const value: tctd_mapopbject; Out
  DynamicSlowing: Boolean): integer;
Var
  i: integer;
Begin
  result := -1;
  DynamicSlowing := false;
  For i := 0 To high(fStaticSlowingObjects) Do
    If fStaticSlowingObjects[i].obj = value Then Begin
      result := i;
      If GetTick() - fStaticSlowingObjects[i].AddTime <= fStaticSlowingObjects[i].DynTime Then Begin
        DynamicSlowing := true;
      End;
      exit;
    End;
End;

{$IFDEF Server}

Procedure TOpponent.GetMovingState(Const Stream: TSTream);
Var
  m: TRenderOpponent;
Begin
  m.Index := RenderIndex;
  m.Identifier := Identifier;
  m.Position := Position;
  If ImageRotation Then Begin
    m.Angle := Direction;
  End
  Else Begin
    m.Angle := 0;
  End;
  m.AnimationOffset := AnimationOffset;
  m.LifePoints := Lifepoints;
  stream.Write(m, sizeof(m));
End;
{$ENDIF}

Constructor TOpponent.create;
Begin
  Inherited create;
{$IFDEF Server}
  DamageByPlayers := Nil;
  HasAnimation := false;
{$ENDIF}
{$IFDEF client}
  Animation := Nil;
{$ENDIF}
  Owner := -1; // 0 .. Playercount -1 ist gültig, so erzeugen wir ne A.V.
  Direction := 0;
  name := '';
  Description := '';
  LifePoints[0] := 0;
  LifePoints[1] := 0;
  LifePoints[2] := 0;
  LifePoints[3] := 0;
  Speed := 2;
  SizeX := 1.6;
  SizeY := 1.6;
  Image := '';
  Canfly := false;
  boss := false;
  Bonus := false;
  LifeFactors[0] := 0.5;
  LifeFactors[1] := 1.0;
  LifeFactors[2] := 1.5;
  ShowLifePoints := false;
  ImageRotation := true;
  fStaticSlowingObjects := Nil;
  // Todo : Initialisierung vervollständigen (alle Variablen sauber initialisieren)
End;

Destructor TOpponent.destroy;
Begin
  setlength(fStaticSlowingObjects, 0);
{$IFDEF Server}
  setlength(DamageByPlayers, 0);
{$ENDIF}
{$IFDEF client}
  If assigned(Animation) Then Animation.Free;
{$ENDIF}

  Inherited destroy;
End;

{$IFDEF Server}

Procedure TOpponent.InitDamageByPlayers(Count: integer);
Var
  i: Integer;
Begin
  setlength(DamageByPlayers, Count);
  For i := 0 To Count - 1 Do Begin
    DamageByPlayers[i] := 0;
  End;
End;
{$ENDIF}

{$IFDEF client}

Procedure TOpponent.AssignDataFromObj(Const obj: TOpponent);
Begin
  (*
   * Übernimmt "diverse" Daten aus Obj
   *)
  Identifier := obj.Identifier;
  fTotalLivepoints := obj.fTotalLivepoints;
End;

Function TOpponent.GetBuyCost: integer;
Begin
  result := 0;
End;

Function TOpponent.GetHint: THint;
Begin
  result.Kind := hkOpponent;
  result.Name := name;
  result.Power := LifePoints;
  result.TotalLivePoints := fTotalLivepoints;
  result.Description := DeSerialize(Description);
  // Zusatzinfos wie Bonus oder Boss auch anzeigen
  If boss And Bonus Then Begin
    result.Description := result.Description + LineEnding + '[Boss, Bonus]';
  End
  Else Begin
    If Boss Then Begin
      result.Description := result.Description + LineEnding + '[Boss]';
    End;
    If Bonus Then Begin
      result.Description := result.Description + LineEnding + '[Bonus]';
    End;
  End;
End;

Procedure TOpponent.Render(Grayed: Boolean);
Begin
  glPushMatrix;
  glTranslatef((SizeX * MapBlockSize) / 2, -(Sizey * MapBlockSize) / 2 + MapBlockSize, 0);
  glColor4f(1, 1, 1, 1);
  If assigned(Animation) Then Begin
    RenderAnim(point(0, 0), round(SizeX * MapBlockSize), round(SizeY * MapBlockSize), Animation, Direction);
  End
  Else Begin
    RenderObjItem(point(0, 0), round(SizeX * MapBlockSize), round(SizeY * MapBlockSize), Fimage, Direction);
  End;
  glPopMatrix;
End;

Function TOpponent.Check: String;
Var
  path: String;
Begin
  result := '';
  // Todo: Hier fehlt noch viel
  path := ExtractFilePath(Filename);
  If path = '' Then exit('Invalid filename');
  path := path + PathDelim;
  If Name = '' Then exit('Missing Name');
  If image = '' Then exit('Missing Image.');
  If Not FileExists(path + image) Then exit('Invalide Image');
End;

{$ENDIF}

Function TOpponent.LoadFromStream(Const Stream: TStream): Boolean;
Var
  ini: Tinifile;
  s: String;
{$IFDEF Client}
  b: Tbitmap;
{$ENDIF}
Begin
  result := Inherited;
  ini := TIniFile.Create(Stream);
  ini.CacheUpdates := true;
  s := ini.ReadString('opponent', 'size', '');
  If s = '' Then Begin
    ini.free;
    exit;
  End;
  DefaultFormatSettings.DecimalSeparator := '.'; // Anscheinend werden zu nicht vorhersehbaren Zeitpunkten die Dezimalseparatoren auf die des Systems angepasst unter de systemen kommt es dann zum Absturz
  sizex := strtofloat(copy(s, 1, pos('x', s) - 1));
  sizey := strtofloat(copy(s, pos('x', s) + 1, length(s)));
  // Breite und Höhe werden benötigt um bei Placements das "Löschen" zu ermöglichen, alles andere läuft bei den Opponents über Size*
  Width := ceil(SizeX);
  Height := ceil(sizey);
  name := ini.ReadString('opponent', 'name', '');
  Description := ini.ReadString('opponent', 'description', '');
  LifePoints[0] := ini.ReadInteger('opponent', 'lifepoints0', 0);
  LifePoints[1] := ini.ReadInteger('opponent', 'lifepoints1', 0);
  LifePoints[2] := ini.ReadInteger('opponent', 'lifepoints2', 0);
  LifePoints[3] := ini.ReadInteger('opponent', 'lifepoints3', 0);
  LifeFactors[0] := ini.Readfloat('opponent', 'lifefactor0', 0.5);
  LifeFactors[1] := ini.Readfloat('opponent', 'lifefactor1', 1);
  LifeFactors[2] := ini.Readfloat('opponent', 'lifefactor2', 1.5);
  fTotalLivepoints := LifePoints[0] + LifePoints[1] + LifePoints[2] + LifePoints[3];
  speed := ini.ReadFloat('opponent', 'speed', 0);
  image := ini.ReadString('opponent', 'image', '');
  Canfly := ini.ReadBool('opponent', 'canfly', false);
  Boss := ini.ReadBool('opponent', 'boss', false);
  Bonus := ini.ReadBool('opponent', 'bonus', false);
  ImageRotation := ini.ReadBool('opponent', 'imagerotation', true);
{$IFDEF Server}
  HasAnimation := lowercase(ExtractFileExt(Image)) = '.ani';
{$ENDIF}
{$IFDEF Client}
  If assigned(Animation) Then Animation.free;
  Animation := Nil;
  If FileExistsutf8(MapFolder + MapName + PathDelim + Image) Then Begin
    If lowercase(ExtractFileExt(Image)) = '.ani' Then Begin
      Animation := TOpenGL_Animation.Create;
      Animation.LoadFromFile(MapFolder + MapName + PathDelim + Image);
      b := Animation.GetFirstBitmap();
      Animation.AnimationOffset := random(Animation.Sprite[0].FrameCount * Animation.Sprite[0].TimePerFrame); // Dafür sorgen, dass alle Animationen "unterschiedlich" sind.
      Fimage := OpenGL_GraphikEngine.LoadAlphaColorGraphikitem(b, MapFolder + MapName + PathDelim + Image + 'Preview', ugraphics.ColorToRGB(clfuchsia), smClamp);
      b.free;
    End
    Else Begin
      Fimage := OpenGL_GraphikEngine.LoadAlphaGraphikitem(MapFolder + MapName + PathDelim + Image, smClamp);
    End;
  End;
{$ENDIF}
  ini.free;
  result := true;
End;

Function TOpponent.SaveToStream(Const Stream: TStream): Boolean;
Var
  ini: TIniFile;
Begin
  result := Inherited;
  ini := TIniFile.Create(Stream);
  ini.CacheUpdates := true;
  ini.WriteString('opponent', 'name', name);
  ini.WriteString('opponent', 'description', description);
  ini.WriteInteger('opponent', 'lifepoints0', LifePoints[0]);
  ini.WriteInteger('opponent', 'lifepoints1', LifePoints[1]);
  ini.WriteInteger('opponent', 'lifepoints2', LifePoints[2]);
  ini.WriteInteger('opponent', 'lifepoints3', LifePoints[3]);
  ini.Writefloat('opponent', 'lifefactor0', LifeFactors[0]);
  ini.Writefloat('opponent', 'lifefactor1', LifeFactors[1]);
  ini.Writefloat('opponent', 'lifefactor2', LifeFactors[2]);
  ini.WriteFloat('opponent', 'speed', speed);
  ini.WriteString('opponent', 'size', format('%fx%f', [SizeX, SizeY]));
  ini.WriteString('opponent', 'image', image);
  ini.WriteBool('opponent', 'canfly', canfly);
  ini.WriteBool('opponent', 'boss', Boss);
  ini.WriteBool('opponent', 'bonus', bonus);
  ini.WriteBool('opponent', 'imagerotation', ImageRotation);
  ini.UpdateFile;
  ini.free;
  result := true;
End;

Function TOpponent.ListOfImages: TStringList;
Begin
  result := TStringList.Create;
  result.Add(image);
End;

Function TOpponent.GetInGameSpeed: Single;
Var
  i: integer;
  f: Single;
  t: int64;
Begin
  // Einbaun Berücksichtigung diverser Slow Faktoren
  t := GetTick();
  f := 1.0;
  For i := 0 To high(fStaticSlowingObjects) Do Begin
    f := f * fStaticSlowingObjects[i].StatValue;
    If (t - fStaticSlowingObjects[i].AddTime) <= fStaticSlowingObjects[i].DynTime Then Begin
      f := f * fStaticSlowingObjects[i].DynValue;
    End;
  End;
  result := Speed * f;
End;

Function TOpponent.SlowDown(Const value: tctd_BulletObject): Boolean;
Var
  b: TBulletObject;
  i: integer;
  blub: Boolean;
Begin
  result := false;
  If Not assigned(value) Then exit;
  b := TBulletObject(value);
  // ggf. Den Bullet Adden
  i := isSlowing(b.Owner, blub);
  If i = -1 Then Begin
    setlength(fStaticSlowingObjects, high(fStaticSlowingObjects) + 2);
    fStaticSlowingObjects[high(fStaticSlowingObjects)].obj := b.Owner;
    i := high(fStaticSlowingObjects);
  End;
  fStaticSlowingObjects[i].DynTime := b.Slowdown.SlowDownTime;
  fStaticSlowingObjects[i].DynValue := b.Slowdown.SlowDownDynamic;
  fStaticSlowingObjects[i].StatValue := b.Slowdown.SlowDownStatic;
  fStaticSlowingObjects[i].AddTime := GetTick();
  result := True;
End;

End.


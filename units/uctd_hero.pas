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
Unit uctd_hero;

{$MODE ObjFPC}{$H+}

{$I apptype.inc}

(* -- Lazarus Bug in Auswertung von apptype.inc -- Dieser Block darf "aktiviert" nie ins GIT
{$UNDEF Client}
{$DEFINE Server}
//*)

Interface

Uses
  Classes, SysUtils, uctd_mapobject, uctd_common
{$IFDEF Client}
  , uopengl_graphikengine
  , uopengl_animation
  , uopengl_spriteengine
{$ENDIF}
{$IFDEF Server}
  , uvectormath
{$ENDIF}
  ;

Type

  THeroLevel = Record
    Description: String;
    Image: String;
    ImageRotation: Boolean; // Wenn True, dann wird die Graphik entsprechend der Winkelgrade gedreht ausgegeben
    StopAnimationWhenStillStanding: Boolean;
    HitpointsForNextLevel: Integer;
    Speed: Single;
    w: Single; // In MAP_BLOCK_WIDTH
    h: Single; // In MAP_BLOCK_WIDTH
    Range: Single;
    reloadtime: integer;
    bulletspeed: single;
    bulletpower: TPower;
    BulletImage: String;
    bulletw: Single;
    bulleth: Single;
{$IFDEF Client}
    Fimage: TGraphikItem; // Image als OpenGL Version
    Animation: TOpenGL_Animation; // Wenn Asigned, dann wird stattdessen eine Animation verwendet
    BulletAnimation: TOpenGL_Animation; // Wenn Asigned, dann wird stattdessen eine Animation verwendet
{$ENDIF}
    fbulletimage: integer; // bulletimage als OpenGL Version, wird auch vom Server verwendet
  End;

  { THero }

  THero = Class(tctd_mapopbject)
  private
    FPausing: Boolean;
    fPauseTime: int64;
    fLastUpdateTime: int64; // Wird genutzt um fUpdating zu aktualisieren
    //    fHasUpdated: boolean; // Merkt sich ob das Gebäude seit dem Letzten ForceIncStage geuppt wurde, wenn ja, dann mus der Server dieses Gebäude beim nächsten Level Restart an alle Clients Aktualisieren
{$IFDEF Client}
    Procedure LoadOpenGLGraphics;
{$ENDIF}
    Procedure Clear;
  public
{$IFDEF Client}
    ShowLifePoints: Boolean;
    Moving: Boolean;
{$ENDIF}
{$IFDEF Server}
    TargetPos: TVector2;
{$ENDIF}
    Level: integer; // -1 = Der Gegner wird gerade initial erzeugt, danach das aktuelle Level
    AnimationOffset: uint16;
    Direction: integer; // Die Richtung in die der Gegner geht (Angabe in Winkelgrad, wird von Map.HandleAllHeroes gesetzt)
    CollectedDamages: integer; // Die Schadenspunkte die in der Aktuellen Stufe gesammelt wurden
    MapHeroIndex: uInt16; // Index in TMap.FHeroes ! ( ACHTUNG, wenn der geändert wird, dann muss er auch in "TServer.HandleSetHeroTargets" geändert werden !! )
    fUpdating: TUpdating; // Todo : Private machen
    LastShootTime: int64; // zwischenspeichervariable für das Spiel

    strategy: TBuildingStrategy;
    PreverAir: boolean; // Wenn True, dann werden zuerst Lufteinheiten angegriffen

    Levels: Array Of THeroLevel;
    Cost: integer;
    BuildTime: integer;

    Constructor create(); override;
    Destructor destroy; override;

    Function LoadFromStream(Const Stream: TStream): Boolean; override;
    Function SaveToStream(Const Stream: TStream): Boolean; override;

    Function ListOfImages(): TStringList; override; // Soll die Liste aller eingebundenen Bilder zurückgeben

{$IFDEF Server}
    Function IncLevel: Boolean; // Startet ein IncStage mit Berücksichtigung der Bauzeit
    Procedure GetMovingState(Const Stream: TSTream);
    Procedure AddDamageDealt(aDamage: integer);
{$ENDIF}

{$IFDEF client}
    Function GetBuyCost: integer; override;
    Function GetHint(): THint; override; // Die Hint Informationen für Im Spiel
    Function GetNextLevelHint(): Thint;
    Procedure Render(Grayed: Boolean); override; // Grayed ist optional und aktuell nur für gebäude implementiert
    Procedure RenderRange;
    Function Check(): String; // Prüft den Gegner auf gültigkeit, '' wenn Fehlerfrei, sonst ist das der Fehlercode
    Procedure ApplyRenderData(Const Data: TRenderHero);
{$ENDIF}
    Function ForceBuilded(): Boolean;
    Procedure SetStage(value: integer); override; // Wird nur für TBuilding gebraucht, macht aber das Laden in Map einfacher

    Procedure Pause(value: Boolean);
    Procedure Start();
    Procedure Update();
  End;

  THeroArray = Array Of THero;

Implementation

Uses IniFiles, LazFileUtils
{$IFDEF Server}
  , math
{$ENDIF}
{$IFDEF Client}
  , dglOpenGL
  , Graphics
  , ugraphics
{$ENDIF}
  ;

{ THero }

Constructor THero.create;
Begin
  Inherited create;
{$IFDEF Client}
  ShowLifePoints := false;
  Moving := false;
{$ENDIF}
  Levels := Nil;
  Level := -1;
  strategy := bsFirst;
  PreverAir := true;
  fLastUpdateTime := GetTick();
{$IFDEF Server}
  TargetPos := v2(-1, -1);
{$ENDIF}
End;

Destructor THero.destroy;
Begin
  Clear;
  Inherited destroy;
End;

Procedure THero.Clear;
{$IFDEF Client}
Var
  i: Integer;
{$ENDIF}
Begin
{$IFDEF Client}
  For i := 0 To high(Levels) Do Begin
    If assigned(Levels[i].Animation) Then Levels[i].Animation.free;
    If assigned(Levels[i].BulletAnimation) Then Levels[i].BulletAnimation.free;
  End;
{$ENDIF}
  setlength(Levels, 0);
End;

Function THero.LoadFromStream(Const Stream: TStream): Boolean;
Var
  ini: TIniFile;
  s: String;
  i: Integer;
  j: Integer;
Begin
  Result := Inherited LoadFromStream(Stream);
  Clear();
  Level := -1;
  ini := TIniFile.Create(Stream);
  ini.CacheUpdates := true;
  DefaultFormatSettings.DecimalSeparator := '.'; // Anscheinend werden zu nicht vorhersehbaren Zeitpunkten die Dezimalseparatoren auf die des Systems angepasst unter de systemen kommt es dann zum Absturz
  s := ini.ReadString('Hero', 'size', '');
  Width := strtointdef(copy(s, 1, pos('x', s) - 1), 1);
  Height := strtointdef(copy(s, pos('x', s) + 1, length(s)), 1);
  name := ini.ReadString('Hero', 'name', '');
  image := ini.ReadString('Hero', 'image', '');
  cost := ini.readinteger('Hero', 'cost', 1);
  BuildTime := ini.readinteger('Hero', 'buildtime', 0);
  setlength(Levels, ini.ReadInteger('Hero', 'Levelcount', 1));
  For i := 0 To high(Levels) Do Begin
    Levels[i].HitpointsForNextLevel := ini.readinteger('Level' + inttostr(i), 'Hitpoints_for_next_level', 100);
    Levels[i].Speed := ini.readfloat('Level' + inttostr(i), 'speed', 1);
    s := ini.ReadString('Level' + inttostr(i), 'size', '');
    Levels[i].w := strtofloatdef(copy(s, 1, pos('x', s) - 1), 1);
    Levels[i].h := strtofloatdef(copy(s, pos('x', s) + 1, length(s)), 1);
    Levels[i].image := ini.ReadString('Level' + inttostr(i), 'image', '');
    Levels[i].Description := ini.ReadString('Level' + inttostr(i), 'description', '');
    Levels[i].reloadtime := ini.readinteger('Level' + inttostr(i), 'reloadtime', 2500);
    Levels[i].range := ini.readfloat('Level' + inttostr(i), 'range', 50);
    Levels[i].bulletspeed := ini.readfloat('Level' + inttostr(i), 'bulletspeed', 5);
    Levels[i].bulletimage := ini.ReadString('Level' + inttostr(i), 'bulletimage', '');
    s := ini.ReadString('Level' + inttostr(i), 'bulletsize', '0x0');
    Levels[i].bulletw := strtofloat(copy(s, 1, pos('x', s) - 1));
    Levels[i].bulleth := strtofloat(copy(s, pos('x', s) + 1, length(s)));
    For j := 0 To 3 Do Begin
      Levels[i].bulletpower[j] := ini.readinteger('Level' + inttostr(i), 'bulletpower' + inttostr(j), 0);
    End;
    Levels[i].ImageRotation := ini.ReadBool('Level' + inttostr(i), 'imagerotation', true);
    Levels[i].StopAnimationWhenStillStanding := ini.ReadBool('Level' + inttostr(i), 'StopAnimationWhenStillStanding', true);
{$IFDEF Client}
    Levels[i].Animation := Nil;
    Levels[i].BulletAnimation := Nil;
{$ENDIF}
  End;
  ini.free;
{$IFDEF Client}
  LoadOpenGLGraphics();
{$ENDIF}
  result := true;
End;

Function THero.SaveToStream(Const Stream: TStream): Boolean;
Var
  ini: TIniFile;
  i: Integer;
  j: Integer;
Begin
  Result := Inherited SaveToStream(Stream);
  ini := TIniFile.Create(Stream);
  ini.CacheUpdates := true;
  ini.WriteString('hero', 'name', name);
  ini.WriteString('hero', 'image', image);
  ini.WriteString('hero', 'size', format('%dx%d', [round(Width), round(Height)]));
  ini.WriteInteger('hero', 'Levelcount', high(Levels) + 1);
  ini.WriteInteger('hero', 'cost', Cost);
  ini.WriteInteger('hero', 'buildtime', buildtime);
  For i := 0 To high(Levels) Do Begin
    ini.WriteInteger('Level' + inttostr(i), 'Hitpoints_for_next_level', Levels[i].HitpointsForNextLevel);
    ini.Writefloat('Level' + inttostr(i), 'speed', Levels[i].Speed);
    ini.WriteString('Level' + inttostr(i), 'size', format('%.2fx%.2f', [Levels[i].w, Levels[i].h]));
    ini.WriteString('Level' + inttostr(i), 'image', Levels[i].image);
    ini.WriteString('Level' + inttostr(i), 'description', Levels[i].Description);
    ini.WriteInteger('Level' + inttostr(i), 'reloadtime', Levels[i].reloadtime);
    ini.Writefloat('Level' + inttostr(i), 'range', Levels[i].range);
    ini.Writefloat('Level' + inttostr(i), 'bulletspeed', Levels[i].bulletspeed);
    ini.WriteString('Level' + inttostr(i), 'bulletimage', Levels[i].bulletimage);
    ini.WriteString('Level' + inttostr(i), 'bulletsize', format('%.2fx%.2f', [Levels[i].bulletw, Levels[i].bulleth]));
    For j := 0 To 3 Do Begin
      ini.WriteInteger('Level' + inttostr(i), 'bulletpower' + inttostr(j), Levels[i].bulletpower[j]);
    End;
    ini.WriteBool('Level' + inttostr(i), 'imagerotation', Levels[i].ImageRotation);
    ini.WriteBool('Level' + inttostr(i), 'StopAnimationWhenStillStanding', Levels[i].StopAnimationWhenStillStanding);
  End;
  ini.UpdateFile;
  ini.free;
  result := true;
End;

{$IFDEF Server}

Procedure THero.GetMovingState(Const Stream: TSTream);
Var
  rh: TRenderHero;
  d, n: int64;
Begin
  rh.Position := Position;
  rh.AnimationOffset := AnimationOffset;
  rh.collectedDamages := CollectedDamages;
  If Level >= 0 Then Begin
    If levels[Level].ImageRotation Then Begin
      rh.Angle := Direction;
    End
    Else Begin
      rh.Angle := 0;
    End;
  End
  Else Begin
    rh.Angle := 0;
    (*
     * Während des initialen "Aufbauens" wird die Hitpoint Genutzt um den Aufbau an zu zeigen
     *)
    n := GetTick();
    If Speedup <> 1 Then Begin
      d := n - fLastUpdateTime;
      d := d * (int64(Speedup) - 1);
    End;
    d := n - fUpdating.StartTime;
    rh.CollectedDamages := round(d * Levels[0].HitpointsForNextLevel / BuildTime);
  End;
  rh.Level := Level;
  rh.Moving := Not Equal(TargetPos, v2(-1, -1));
  stream.write(rh, sizeof(TRenderHero));
End;

Procedure THero.AddDamageDealt(aDamage: integer);
Begin
  CollectedDamages := CollectedDamages + aDamage;
  If Level = high(Levels) Then Begin
    CollectedDamages := min(CollectedDamages, levels[Level].HitpointsForNextLevel);
  End
  Else Begin
    If CollectedDamages >= levels[Level].HitpointsForNextLevel Then Begin
      IncLevel();
    End;
  End;
End;

Function THero.IncLevel: Boolean;
Var
  n: QWord;
Begin
  result := false;
  If High(Levels) <= Level Then exit;
  If level = -1 Then Begin // Der erste Bau
    n := GetTick();
    fUpdating.StartTime := n;
    fUpdating.State := usInProgress;
    fUpdating.FinState := Level + 1;
    fLastUpdateTime := n;
    result := true;
  End
  Else Begin
    // Direktes Updating
    fUpdating.State := usIdleInactive;
    Level := Level + 1;
    LastShootTime := GetTick() - Levels[Level].reloadtime;
    CollectedDamages := 0;
  End;
End;

{$ENDIF}

Function THero.ForceBuilded: Boolean;
Begin
  If Level = -1 Then Begin // Der Hero "Baut" noch -> Nu is er fertig !
    fUpdating.State := usIdleInactive;
    result := true;
    level := 0;
{$IFDEF Server}
    CollectedDamages := 0;
{$ENDIF}
  End
  Else Begin
    result := false;
  End;
  //  result := fHasUpdated;
  //  If fUpdating.State <> usIdleInactive Then Begin
  //    Level := fUpdating.FinState;
  //    fUpdating.State := usIdleInactive;
  LastShootTime := GetTick() - Levels[Level].reloadtime;
  //  End;
  //  If ResetHasUpdated Then Begin
  //    fHasUpdated := false;
  //  End;
End;

{$IFDEF client}

Procedure THero.LoadOpenGLGraphics;
Var
  ap: String;
  i: Integer;
  b: Tbitmap;
  a: TOpenGL_Animation;
Begin
  ap := MapFolder + MapName + PathDelim;
  If FileExistsUTF8(ap + image) And (image <> '') Then Begin
    If lowercase(ExtractFileExt(Image)) = '.ani' Then Begin
      // Bei einer Animation, wird nur die erste Textur Verwendet und der Rest weg geworfen..
      a := TOpenGL_Animation.Create;
      a.LoadFromFile(ap + image, false);
      b := a.GetFirstBitmap();
      Fimage := OpenGL_GraphikEngine.LoadAlphaColorGraphik(b, ap + image + '_Image', ugraphics.ColorToRGB(clFuchsia), smStretchHard);
      b.free;
      a.free;
    End
    Else Begin
      Fimage := OpenGL_GraphikEngine.LoadAlphaGraphik(ap + image, smStretchHard);
    End;
  End
  Else Begin
    Fimage := 0;
  End;
{$IFDEF Server}
  AnimationOffset := random(65536);
{$ENDIF}
  For i := 0 To high(Levels) Do Begin
    If FileExistsUTF8(ap + Levels[i].image) And (Levels[i].image <> '') Then Begin
      If lowercase(ExtractFileExt(Levels[i].image)) = '.ani' Then Begin
        Levels[i].Animation := TOpenGL_Animation.Create;
        Levels[i].Animation.LoadFromFile(ap + Levels[i].image, true);
        Levels[i].Animation.AnimationOffset := random(65536);
        Levels[i].Fimage.Image := 0;
      End
      Else Begin
        Levels[i].Fimage := OpenGL_GraphikEngine.LoadAlphaGraphikItem(ap + Levels[i].image, smClamp);
      End;
    End
    Else Begin
      Levels[i].Fimage.Image := 0;
    End;
    If FileExistsUTF8(ap + Levels[i].bulletimage) And (trim(Levels[i].bulletimage) <> '') Then Begin
      If lowercase(ExtractFileExt(Levels[i].bulletimage)) = '.ani' Then Begin
        Levels[i].BulletAnimation := TOpenGL_Animation.Create;
        Levels[i].BulletAnimation.LoadFromFile(ap + Levels[i].bulletimage, true);
        Levels[i].BulletAnimation.AnimationOffset := random(65536);
        Levels[i].Fbulletimage := 0;
      End
      Else Begin
        Levels[i].Fbulletimage := OpenGL_GraphikEngine.LoadAlphaGraphik(ap + Levels[i].bulletimage, smStretchHard);
      End;
    End
    Else Begin
      Levels[i].Fbulletimage := 0;
    End;
  End;
End;

Function THero.Check: String;
Var
  path: String;
  i: Integer;
Begin
  result := '';
  path := ExtractFilePath(Filename);
  If path = '' Then exit('Invalid filename');
  path := path + PathDelim;
  If Name = '' Then exit('Missing name');
  If image = '' Then exit('Missing image.');
  If Not FileExists(path + image) Then exit('Invalide Image');
  // TODO: Size Implementieren
  If cost <= 0 Then exit('Invalid cost value');
  If Buildtime <= 0 Then exit('Invalid buildtime value');
  If high(Levels) < 0 Then exit('Invalid Level count');
  If high(levels) > 65535 Then exit('only max 65535 levels allowed for heroes.');
  For i := 0 To high(Levels) Do Begin
    If Levels[i].Image = '' Then exit('Level ' + inttostr(i + 1) + ' invalid image');
    If Not FileExists(path + Levels[i].image) Then exit('Invalide image in level' + inttostr(i + 1));
    If Levels[i].BulletImage = '' Then exit('Level ' + inttostr(i + 1) + ' invalid bulletimage');
    If Not FileExists(path + Levels[i].bulletimage) Then exit('Invalide bullet image in level ' + inttostr(i + 1));
    If Levels[i].Speed <= 0 Then exit('Level ' + inttostr(i + 1) + ' invalid speed');
    If Levels[i].HitpointsForNextLevel <= 0 Then exit('Level ' + inttostr(i + 1) + ' invalid hitpoints for next level');
    // TODO: Size implementieren
    // TODO: BulletSize implementieren
    If Levels[i].Range <= 0 Then exit('Level ' + inttostr(i + 1) + ' invalid range');
    If Levels[i].reloadtime <= 0 Then exit('Level ' + inttostr(i + 1) + ' invalid reloadtime');
    If Levels[i].bulletspeed <= 0 Then exit('Level ' + inttostr(i + 1) + ' invalid bulletspeed');
    If (Levels[i].bulletpower[0] < 0) Or
      (Levels[i].bulletpower[1] < 0) Or
      (Levels[i].bulletpower[2] < 0) Or
      (Levels[i].bulletpower[3] < 0) Then exit('Level ' + inttostr(i + 1) + ' invalid damage value');
    If (Levels[i].bulletpower[0] = 0) And
      (Levels[i].bulletpower[1] = 0) And
      (Levels[i].bulletpower[2] = 0) And
      (Levels[i].bulletpower[3] = 0) Then exit('Level ' + inttostr(i + 1) + ' level makes no damage at all');
  End;
End;

Procedure THero.ApplyRenderData(Const Data: TRenderHero);
Begin
  Position := data.Position;
  AnimationOffset := data.AnimationOffset;
  Direction := data.Angle;
  CollectedDamages := data.CollectedDamages; // Den Schaden, den der Hero in diesem Level bereits "gesammelt" hat
  If data.level = 65535 Then Begin
    level := -1;
  End
  Else Begin
    Level := data.Level; // Das Aktuelle Level
  End;
  Moving := data.Moving; // Wenn True, dann bewegt sich der Held gerade
End;

Function THero.GetBuyCost: integer;
Begin
  result := Cost;
End;

Function THero.GetHint: THint;
Begin
  result.Kind := hkHero;
  result.StageCount := length(Levels);
  result.Stage := Level + 1;
  result.Name := name;
  If (Level >= 0) And (Level <= high(Levels)) Then Begin
    result.Power := Levels[Level].bulletpower;
    result.Description :=
      'Hits: ' + inttostr(CollectedDamages) + ' / ' + inttostr(Levels[Level].HitpointsForNextLevel) + LineEnding +
      DeSerialize(Levels[Level].Description) +
      'Shoots every: ' + inttostr(Levels[Level].reloadtime) + 'ms';
  End
  Else Begin
    result.Power[0] := 0;
    result.Power[1] := 0;
    result.Power[2] := 0;
    result.Power[3] := 0;
    result.Description := '';
  End;
End;

Function THero.GetNextLevelHint(): Thint;
Begin
  result.Kind := hkHero;
  result.StageCount := length(Levels);
  result.Stage := Level + 2;
  result.Name := name;
  If (Level >= 0) And (Level < high(Levels)) Then Begin
    result.Power := Levels[Level + 1].bulletpower;
    result.Description :=
      DeSerialize(Levels[Level + 1].Description) +
      'Shoots every: ' + inttostr(Levels[Level + 1].reloadtime) + 'ms';
  End
  Else Begin
    result.Power[0] := 0;
    result.Power[1] := 0;
    result.Power[2] := 0;
    result.Power[3] := 0;
    result.Description := '';
  End;
End;

Procedure THero.Render(Grayed: Boolean);
Var
  aLevel: Integer;
  Procedure RenderBar(PerCent: Single);
  Var
    t, l, w, h: Single;
  Begin
    glPushMatrix;
    glTranslatef((Levels[aLevel].w * MapBlockSize) / 2, -(Levels[aLevel].h * MapBlockSize) / 2 + MapBlockSize, 0);
    // TODO: Ohne tiefentest sieht es zwar gut aus, aber leider im Menü dann doof
    // glDisable(GL_DEPTH_TEST);
    t := 0; //-height * MapBlockSize - LifebarOffset;
    l := -Levels[aLevel].w * MapBlockSize / 2;
    h := LifebarHeight;
    w := Levels[aLevel].w * MapBlockSize;
    glBindTexture(GL_TEXTURE_2D, 0);
    glbegin(GL_QUADS);
    glColor3f(0, 0, 0);
    glVertex2f(l - 1, t - 1);
    glVertex2f(l + 1 + w, t - 1);
    glVertex2f(l + 1 + w, t + h + 1);
    glVertex2f(l - 1, t + h + 1);
    glend;
    glColor(Good_Col);
    w := Levels[aLevel].w * MapBlockSize * PerCent;
    glTranslatef(0, 0, 6 * ctd_EPSILON);
    glbegin(GL_QUADS);
    glVertex2f(l, t);
    glVertex2f(l + w, t);
    glVertex2f(l + w, t + h);
    glVertex2f(l, t + h);
    glend;
    // glenable(GL_DEPTH_TEST);
    glPopMatrix;
  End;

Var
  Sprite_Engine_Enabled: Boolean;
  lps: Single;
Begin
  // Wenn der Held leicht Ausgegraut gerendert werden soll ;)
  If Grayed Then Begin
    glColor4f(0.5, 0.5, 0.5, 1);
  End
  Else Begin
    glColor4f(1, 1, 1, 1);
  End;
  Sprite_Engine_Enabled := OpenGL_SpriteEngine.Enabled;
  alevel := Level;
  lps := CollectedDamages / Levels[0].HitpointsForNextLevel;
  glPushMatrix;
  If level = -1 Then Begin
    aLevel := 0;
  End;
  If assigned(Levels[aLevel].Animation) Then Begin
    (*
     * Nur Ausschaltende sind erlaubt !!
     *)
    If Level = -1 Then OpenGL_SpriteEngine.Enabled := false;
    If (Not Moving) And (Levels[aLevel].StopAnimationWhenStillStanding) Then OpenGL_SpriteEngine.Enabled := false;
    RenderMoveableAnim(AnimationOffset, Levels[aLevel].Animation, Direction, Levels[aLevel].w, Levels[aLevel].h, lps, ShowLifePoints);
  End
  Else Begin
    RenderMoveable(Levels[aLevel].Fimage.Image, direction, Levels[aLevel].w, Levels[aLevel].h, lps, ShowLifePoints);
  End;
  If level = -1 Then Begin
    RenderBar(lps);
  End;
  fLastUpdateTime := GetTick();
  OpenGL_SpriteEngine.Enabled := Sprite_Engine_Enabled;
  glPopMatrix;
  (*  // Debug -- Zum Austesten der Boundingbox gegen Bauskoordinaten
    glPushMatrix();
    glDisable(GL_DEPTH_TEST);
    glBindTexture(GL_TEXTURE_2D, 0);
    //  glTranslatef(Position.x, Position.y, 0);
    glColor3f(1, 0, 0);
    glBegin(GL_LINE_STRIP);
    glVertex2f(0, 0);
    glVertex2f(0, -MapBlockSize);
    glend();
    glColor3f(0, 1, 0);
    glBegin(GL_LINE_STRIP);

    Links unten = (0, 1)
    Rechts Oben = (w, 1-h)
    glVertex2f(0, MapBlockSize);
    glVertex2f(0 + Levels[aLevel].w * MapBlockSize, MapBlockSize - Levels[aLevel].h * MapBlockSize);
    glend();

    glenable(GL_DEPTH_TEST);
    glPopMatrix;
     // *)
End;

Procedure THero.RenderRange;
Var
  i: integer;
  r: Single;
Begin
  // Nen Range gibts nicht immer
  If Level < 0 Then exit;
  If Levels[Level].range > 0 Then Begin
    glLineStipple(1, $00FF);
    glEnable(GL_LINE_STIPPLE);
    glColor(Building_Range_Col);
    glBindTexture(GL_TEXTURE_2D, 0);
    glPushMatrix;
    glTranslatef((Levels[Level].w * MapBlockSize) / 2, -(Levels[Level].h * MapBlockSize) / 2 + MapBlockSize, 0);
    r := MapBlockSize * Levels[Level].range;
    glbegin(GL_LINE_LOOP);
    // TODO: Das könnte in Abhängigkeit von r auch "mehr" sein..
    For i := 0 To 19 Do Begin
      glVertex2f(cos(pi * i / 10) * r, sin(pi * i / 10) * r);
    End;
    glend;
    glPopMatrix;
    glDisable(GL_LINE_STIPPLE);
  End;
End;

{$ENDIF}

Procedure THero.SetStage(value: integer);
Begin
  Inherited SetStage(value);
End;

Procedure THero.Pause(value: Boolean);
Var
  i: int64;
Begin
  If FPausing Then Begin
    If Not value Then Begin
      i := GetTick() - fPauseTime;
      LastShootTime := LastShootTime + i; // Wir Verschieben die StartZeit um die Zeit der Pause in die Zukunft
      fUpdating.StartTime := fUpdating.StartTime + i; // falls wir gerade Updaten..
      fLastUpdateTime := fLastUpdateTime + i;
    End;
  End
  Else Begin
    If value Then Begin
      fPauseTime := GetTick();
    End;
  End;
  FPausing := value;
End;

Procedure THero.Start;
Var
  n: int64;
Begin
  FPausing := false;
  n := GetTick();
  fLastUpdateTime := n;
  If (Level >= 0) And (Level <= high(Levels)) Then Begin
    LastShootTime := n - Levels[Level].reloadtime;
  End;
End;

//So wie es aussieht wird der Hero trotz Pause Fertig gestellt !

Procedure THero.Update;
Var
  d, n: int64;
Begin
  If Level >= 0 Then Begin
    // Das Gebäude ist "Fertig" ;)
    fLastUpdateTime := GetTick();
  End
  Else Begin
    // Der Progressbar
    If Not FPausing Then Begin
      (*
       * !! ACHTUNG !! , das hier ist Doppelt hier und in Procedure THero.Render;
       *)
      n := GetTick();
      If Speedup <> 1 Then Begin
        d := n - fLastUpdateTime;
        d := d * (int64(Speedup) - 1);
        fUpdating.StartTime := fUpdating.StartTime - d;
      End;
      fLastUpdateTime := n;
      d := n - fUpdating.StartTime;
      If d >= BuildTime Then Begin
        ForceBuilded();
      End;
    End;
  End;
End;

Function THero.ListOfImages: TStringList;
Var
  i: Integer;
Begin
  result := TStringList.Create;
  result.Add(Image);
  For i := 0 To high(Levels) Do Begin
    result.add(levels[i].Image);
    result.add(levels[i].BulletImage);
  End;
End;

End.


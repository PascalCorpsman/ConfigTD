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
Unit uctd_building;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils, uctd_mapobject, uctd_common
{$IFDEF Client}
  , uopengl_graphikengine
  , uopengl_animation
{$ENDIF}
  ;

Type

  TStage = Record
    w: integer; // In MAP_BLOCK_WIDTH
    h: integer; // In MAP_BLOCK_WIDTH
{$IFDEF Client}
    Fimage: TGraphikItem; // Image als OpenGL Version
    Animation: TOpenGL_Animation; // Wenn Asigned, dann wird stattdessen eine Animation verwendet
    BulletAnimation: TOpenGL_Animation; // Wenn Asigned, dann wird stattdessen eine Animation verwendet
{$ENDIF}
    fbulletimage: integer; // bulletimage als OpenGL Version, wird auch vom Server verwendet
    BulletRenderIndex: integer;
    BuildTime: integer; // in ms für die jeweilige Stage
    bulletw: Single;
    bulleth: Single;
    bulletLeafPointx: Single;
    bulletLeafPointy: Single;
    image: String;
    Description: String;
    Cost: integer;
    reloadtime: integer;
    range: Single;
    earn: integer;
    bulletspeed: single;
    bulletimage: String;
    bulletsplashradius: single;
    bulletpower: TPower;
    SlowDown: TSlowDown;
  End;

  { TBuilding }

  TBuilding = Class(tctd_mapopbject)
  private
    FPausing: Boolean;
    DeltaSinceLastUpdate: integer; // Zeit in ms, seit dem letzten Update, Wird genutzt um fUpdating zu aktualisieren
    fHasUpdated: boolean; // Merkt sich ob das Gebäude seit dem Letzten ForceIncStage geuppt wurde, wenn ja, dann mus der Server dieses Gebäude beim nächsten Level Restart an alle Clients Aktualisieren

{$IFDEF Client}
    Procedure LoadOpenGLGraphics;
{$ENDIF}
    Procedure Clear;
  public
    fUpdating: TUpdating; // Todo : Private machen ( das ist Public, wegem Anwählen aller gleichen Gebs in der Karte)
    DeltaSinceLastShoot: integer; // Zeit in ms, seit dem Letzten Schuß
    Category: String;
    Stages: Array Of TStage;
    Stage: integer;
    Refund: Array[0..2] Of Integer; // Wiederverkaufswert 0 = Einfach, 1 = Mittel, 2 = Schwer (Angabe in Prozent des bisher investierten Betrags Kaufkosten der einzelnen Stages)


    strategy: TBuildingStrategy;
    PreverAir: boolean; // Wenn True, dann werden zuerst Lufteinheiten angegriffen
    Constructor create(); override;
    Destructor destroy; override;

    Function LoadFromStream(Const Stream: TStream): Boolean; override;
    Function SaveToStream(Const Stream: TStream): Boolean; override;

    Function ListOfImages(): TStringList; override;
{$IFDEF Client}
    Function GetBuyCost: integer; override;
    Function GetHint(): THint; override;
    Function GetNextStageHint(): THint; // Gleich wie TBuilding.GetHint: THint; nur eben für die Stage nach der Aktuellen, wenn es keine mehr gibt alles 0
    Procedure Render(Grayed: Boolean); override;
    Procedure RenderRange;
    Function Check(): String; // Prüft das Gebäude auf gültigkeit, '' wenn Fehlerfrei, sonst ist das der Fehlercode
    Function CalcSellRefund(Difficulty: integer): integer;
    Function CalculateCost(): integer;
{$ENDIF}
    Function ForceIncStage(ResetHasUpdated: Boolean): Boolean; // Erhöht den Status unabhängig von der Bauzeit, True, wenn Geuppt wurde
    Function IncStage: Boolean; // Startet ein IncStage mit Berücksichtigung der Bauzeit
    Procedure SetStage(value: integer); override;

    Procedure Pause(value: Boolean);
    Procedure Start();
    Procedure Update(delta: integer);
  End;

  TBuildingArray = Array Of TBuilding;

Implementation

Uses
  IniFiles, LazFileUtils, LCLIntf
  , math
{$IFDEF Client}
  , dglOpenGL
  , Graphics
  , ugraphics
{$ENDIF}
  ;

{ TBuilding }

Constructor TBuilding.create;
Begin
  Inherited create;
  fHasUpdated := false;
  // Alles zurück auf "nichts"
  Stages := Nil;
  Stage := -1;
  strategy := bsFirst;
  PreverAir := true;
  DeltaSinceLastUpdate := 0;
  DeltaSinceLastShoot := 0;
End;

Destructor TBuilding.destroy;
Begin
  Clear;
  Inherited destroy;
End;

Function TBuilding.LoadFromStream(Const Stream: TStream): Boolean;
Var
  ini: TIniFile;
  s: String;
  i: Integer;
  j: Integer;
Begin
  Result := Inherited LoadFromStream(Stream);
  Clear();
  Stage := 0;
  ini := TIniFile.Create(Stream);
  ini.CacheUpdates := true;
  DefaultFormatSettings.DecimalSeparator := '.'; // Anscheinend werden zu nicht vorhersehbaren Zeitpunkten die Dezimalseparatoren auf die des Systems angepasst unter de systemen kommt es dann zum Absturz
  s := ini.ReadString('building', 'size', '');
  Width := strtointdef(copy(s, 1, pos('x', s) - 1), 1);
  Height := strtointdef(copy(s, pos('x', s) + 1, length(s)), 1);
  name := ini.ReadString('building', 'name', '');
  image := ini.ReadString('building', 'image', '');
  Category := ini.ReadString('building', 'category', '');
  refund[0] := ini.ReadInteger('building', 'refund0', 100);
  refund[1] := ini.ReadInteger('building', 'refund1', 80);
  refund[2] := ini.ReadInteger('building', 'refund2', 50);
  setlength(Stages, ini.ReadInteger('building', 'stagecount', 1));
  For i := 0 To high(Stages) Do Begin
    s := ini.ReadString('stage' + inttostr(i), 'size', '');
    stages[i].w := strtointdef(copy(s, 1, pos('x', s) - 1), 1);
    stages[i].h := strtointdef(copy(s, pos('x', s) + 1, length(s)), 1);
    stages[i].image := ini.ReadString('stage' + inttostr(i), 'image', '');
    stages[i].Description := ini.ReadString('stage' + inttostr(i), 'description', '');
    stages[i].cost := ini.readinteger('stage' + inttostr(i), 'cost', 100);
    stages[i].reloadtime := ini.readinteger('stage' + inttostr(i), 'reloadtime', 2500);
    stages[i].range := ini.readfloat('stage' + inttostr(i), 'range', 50);
    stages[i].earn := ini.readinteger('stage' + inttostr(i), 'earn', 0);
    stages[i].bulletspeed := ini.readfloat('stage' + inttostr(i), 'bulletspeed', 5);
    stages[i].bulletimage := ini.ReadString('stage' + inttostr(i), 'bulletimage', '');
    stages[i].bulletsplashradius := ini.readfloat('stage' + inttostr(i), 'bulletsplashradius', 0);
    s := ini.ReadString('stage' + inttostr(i), 'bulletsize', '0x0');
    stages[i].bulletw := strtofloat(copy(s, 1, pos('x', s) - 1));
    stages[i].bulleth := strtofloat(copy(s, pos('x', s) + 1, length(s)));
    s := ini.ReadString('stage' + inttostr(i), 'bulletexitpoint', '0x0');
    stages[i].bulletLeafPointx := strtofloat(copy(s, 1, pos('x', s) - 1));
    stages[i].bulletLeafPointy := strtofloat(copy(s, pos('x', s) + 1, length(s)));
    For j := 0 To 3 Do Begin
      stages[i].bulletpower[j] := ini.readinteger('stage' + inttostr(i), 'bulletpower' + inttostr(j), 0);
    End;
    stages[i].SlowDown.slowdownstatic := ini.ReadFloat('stage' + inttostr(i), 'slowdownstatic', 1);
    stages[i].SlowDown.slowdowndynamic := ini.ReadFloat('stage' + inttostr(i), 'slowdowndynamic', 1);
    stages[i].SlowDown.slowdowntime := ini.readinteger('stage' + inttostr(i), 'slowdowntime', 0);
    stages[i].BuildTime := ini.readinteger('stage' + inttostr(i), 'buildtime', 0);
{$IFDEF Client}
    stages[i].Animation := Nil;
    stages[i].BulletAnimation := Nil;
{$ENDIF}
  End;
  ini.free;
{$IFDEF Client}
  LoadOpenGLGraphics();
{$ENDIF}
  result := true;
End;

Function TBuilding.SaveToStream(Const Stream: TStream): Boolean;
Var
  ini: TIniFile;
  i: Integer;
  j: Integer;
Begin
  Result := Inherited SaveToStream(Stream);
  ini := TIniFile.Create(Stream);
  ini.CacheUpdates := true;
  ini.WriteString('building', 'name', name);
  ini.WriteString('building', 'size', format('%dx%d', [round(Width), round(Height)]));
  ini.WriteInteger('building', 'stagecount', high(Stages) + 1);
  ini.WriteString('building', 'image', image);
  ini.WriteString('building', 'category', Category);
  ini.WriteInteger('building', 'refund0', refund[0]);
  ini.WriteInteger('building', 'refund1', refund[1]);
  ini.WriteInteger('building', 'refund2', refund[2]);
  For i := 0 To high(Stages) Do Begin
    ini.WriteString('stage' + inttostr(i), 'size', format('%dx%d', [Stages[i].w, Stages[i].h]));
    ini.WriteString('stage' + inttostr(i), 'image', Stages[i].image);
    ini.WriteString('stage' + inttostr(i), 'description', Stages[i].Description);
    ini.WriteInteger('stage' + inttostr(i), 'cost', Stages[i].Cost);
    ini.WriteInteger('stage' + inttostr(i), 'reloadtime', Stages[i].reloadtime);
    ini.Writefloat('stage' + inttostr(i), 'range', Stages[i].range);
    ini.WriteInteger('stage' + inttostr(i), 'earn', Stages[i].earn);
    ini.Writefloat('stage' + inttostr(i), 'bulletspeed', Stages[i].bulletspeed);
    ini.WriteString('stage' + inttostr(i), 'bulletimage', Stages[i].bulletimage);
    ini.Writefloat('stage' + inttostr(i), 'bulletsplashradius', Stages[i].bulletsplashradius);
    ini.WriteString('stage' + inttostr(i), 'bulletsize', format('%.2fx%.2f', [Stages[i].bulletw, Stages[i].bulleth]));
    ini.WriteString('stage' + inttostr(i), 'bulletexitpoint', format('%.2fx%.2f', [Stages[i].bulletLeafPointx, Stages[i].bulletLeafPointy]));
    For j := 0 To 3 Do Begin
      ini.WriteInteger('stage' + inttostr(i), 'bulletpower' + inttostr(j), Stages[i].bulletpower[j]);
    End;
    ini.WriteFloat('stage' + inttostr(i), 'slowdownstatic', Stages[i].SlowDown.slowdownstatic);
    ini.WriteFloat('stage' + inttostr(i), 'slowdowndynamic', Stages[i].SlowDown.slowdowndynamic);
    ini.WriteInteger('stage' + inttostr(i), 'slowdowntime', Stages[i].SlowDown.slowdowntime);
    ini.WriteInteger('stage' + inttostr(i), 'buildtime', Stages[i].BuildTime);
  End;
  ini.UpdateFile;
  ini.free;
  result := true;
End;

Function TBuilding.ListOfImages: TStringList;
Var
  i: Integer;
Begin
  result := TStringList.Create;
  result.add(image);
  For i := 0 To high(Stages) Do Begin
    result.add(Stages[i].image);
    If (stages[i].range <> 0) Then
      result.add(Stages[i].bulletimage);
  End;
End;

Function TBuilding.IncStage: Boolean;
Begin
  result := false;
  If High(Stages) <= Stage Then exit;
  fUpdating.State := usInProgress;
  fUpdating.FinState := Stage + 1;
  fHasUpdated := true;
  DeltaSinceLastUpdate := 0;
  result := true;
  // Direktes Updating
  If Stages[fUpdating.FinState].BuildTime <= 0 Then Begin
    fUpdating.State := usIdleInactive;
    Stage := Stage + 1;
    DeltaSinceLastShoot := Stages[Stage].reloadtime;
  End;
End;

Procedure TBuilding.Pause(value: Boolean);
Begin
  FPausing := value;
End;

Procedure TBuilding.Start;
Begin
  FPausing := false;
  DeltaSinceLastUpdate := 0;
  DeltaSinceLastShoot := 0;
  If (Stage >= 0) And (stage <= high(Stages)) Then Begin
    DeltaSinceLastShoot := Stages[Stage].reloadtime;
  End;
End;

Function TBuilding.ForceIncStage(ResetHasUpdated: Boolean): Boolean;
Begin
  result := fHasUpdated;
  If fUpdating.State <> usIdleInactive Then Begin
    Stage := fUpdating.FinState;
    fUpdating.State := usIdleInactive;
    DeltaSinceLastUpdate := 0;
    DeltaSinceLastShoot := Stages[Stage].reloadtime;
  End;
  If ResetHasUpdated Then Begin
    fHasUpdated := false;
  End;
End;

Procedure TBuilding.Update(delta: integer);
Begin
  If fUpdating.State = usIdleInactive Then Begin
    // Das Gebäude ist "Fertig" ;)
    DeltaSinceLastUpdate := 0;
    DeltaSinceLastShoot := min(DeltaSinceLastShoot + delta, Stages[Stage].reloadtime);
  End
  Else Begin
    // Der Progressbar
    If Not FPausing Then Begin
      DeltaSinceLastUpdate := DeltaSinceLastUpdate + delta;
      If DeltaSinceLastUpdate >= stages[fUpdating.FinState].BuildTime Then Begin
        ForceIncStage(false);
      End;
    End;
  End;
End;

Procedure TBuilding.Clear;
{$IFDEF Client}
Var
  i: Integer;
{$ENDIF}
Begin
{$IFDEF Client}
  For i := 0 To high(Stages) Do Begin
    If assigned(Stages[i].Animation) Then Stages[i].Animation.free;
    If assigned(Stages[i].BulletAnimation) Then Stages[i].BulletAnimation.free;
  End;
{$ENDIF}
  setlength(stages, 0);
End;

{$IFDEF Client}

Function TBuilding.GetBuyCost: integer;
Begin
  result := Stages[0].Cost;
End;

Function TBuilding.GetHint: THint;
Begin
  result.Kind := hkbuilding;
  result.StageCount := length(Stages);
  result.Stage := stage + 1;
  result.Name := name;
  If (stage >= 0) And (stage <= high(Stages)) Then Begin
    result.Power := Stages[Stage].bulletpower;
    result.Description := DeSerialize(Stages[Stage].Description) +
      'Shoots every: ' + inttostr(Stages[Stage].reloadtime) + 'ms';
  End
  Else Begin
    result.Power[0] := 0;
    result.Power[1] := 0;
    result.Power[2] := 0;
    result.Power[3] := 0;
    result.Description := '';
  End;
End;

Function TBuilding.GetNextStageHint: THint;
Begin
  result.Kind := hkbuilding;
  result.StageCount := length(Stages);
  result.Stage := stage + 2;
  result.Name := name;
  If (stage >= -1) And (stage < high(Stages)) Then Begin
    result.Power := Stages[Stage + 1].bulletpower;
    result.Description := DeSerialize(Stages[Stage + 1].Description) +
      'Shoots every: ' + inttostr(Stages[Stage + 1].reloadtime) + 'ms';
  End
  Else Begin
    result.Power[0] := 0;
    result.Power[1] := 0;
    result.Power[2] := 0;
    result.Power[3] := 0;
    result.Description := '';
  End;
End;

Procedure TBuilding.LoadOpenGLGraphics;
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
      Fimage := OpenGL_GraphikEngine.LoadAlphaColorGraphikitem(b, ap + image + '_Image', ugraphics.ColorToRGB(clFuchsia), smStretchHard);
      b.free;
      a.free;
    End
    Else Begin
      Fimage := OpenGL_GraphikEngine.LoadAlphaGraphikitem(ap + image, smStretchHard);
    End;
  End
  Else Begin
    Fimage.Image := 0;
  End;
  For i := 0 To high(Stages) Do Begin
    If FileExistsUTF8(ap + stages[i].image) And (stages[i].image <> '') Then Begin
      If lowercase(ExtractFileExt(stages[i].image)) = '.ani' Then Begin
        stages[i].Animation := TOpenGL_Animation.Create;
        stages[i].Animation.LoadFromFile(ap + stages[i].image, true);
        stages[i].Animation.AnimationOffset := random(65536);
        Stages[i].Fimage.Image := 0;
      End
      Else Begin
        Stages[i].Fimage := OpenGL_GraphikEngine.LoadAlphaGraphikItem(ap + stages[i].image, smStretchHard);
      End;
    End
    Else Begin
      Stages[i].Fimage.Image := 0;
    End;
    If FileExistsUTF8(ap + stages[i].bulletimage) And (trim(stages[i].bulletimage) <> '') Then Begin
      If lowercase(ExtractFileExt(stages[i].bulletimage)) = '.ani' Then Begin
        stages[i].BulletAnimation := TOpenGL_Animation.Create;
        stages[i].BulletAnimation.LoadFromFile(ap + stages[i].bulletimage, true);
        stages[i].BulletAnimation.AnimationOffset := random(65536);
        Stages[i].Fbulletimage := 0;
      End
      Else Begin
        Stages[i].Fbulletimage := OpenGL_GraphikEngine.LoadAlphaGraphik(ap + stages[i].bulletimage, smStretchHard);
      End;
    End
    Else Begin
      Stages[i].Fbulletimage := 0;
    End;
  End;
End;

Procedure TBuilding.Render(Grayed: Boolean);

  Procedure RenderBar(PerCent: Single);
  Var
    t, l, w, h: Single;
  Begin
    glPushMatrix;
    // TODO: Ohne tiefentest sieht es zwar gut aus, aber leider im Menü dann doof
    // glDisable(GL_DEPTH_TEST);
    t := 0; //-height * MapBlockSize - LifebarOffset;
    l := -width * MapBlockSize / 2;
    h := LifebarHeight;
    w := width * MapBlockSize;
    glBindTexture(GL_TEXTURE_2D, 0);
    glbegin(GL_QUADS);
    glColor3f(0, 0, 0);
    glVertex2f(l - 1, t - 1);
    glVertex2f(l + 1 + w, t - 1);
    glVertex2f(l + 1 + w, t + h + 1);
    glVertex2f(l - 1, t + h + 1);
    glend;
    Case round(PerCent * 100) Of
      71..100: glColor(Good_Col);
      31..70: glColor(Middle_Col);
      0..30: glColor(Bad_col);
    End;
    w := width * MapBlockSize * PerCent;
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

Begin
  // Wenn das Gebäude leicht Ausgegraut gerendert werden soll ;)
  If Grayed Then Begin
    glColor4f(0.5, 0.5, 0.5, 1);
  End
  Else Begin
    glColor4f(1, 1, 1, 1);
  End;
  glPushMatrix;
  If fUpdating.State = usIdleInactive Then Begin
    // nicht Updaten
    glTranslatef((Stages[Stage].w * MapBlockSize) / 2, -(Stages[Stage].h * MapBlockSize) / 2 + MapBlockSize, 0);
    If assigned(stages[Stage].Animation) Then Begin
      RenderAnim(point(0, 0), stages[Stage].w * MapBlockSize, stages[Stage].h * MapBlockSize, stages[Stage].Animation);
    End
    Else Begin
      RenderObjItem(point(0, 0), stages[Stage].w * MapBlockSize, stages[Stage].h * MapBlockSize, stages[Stage].Fimage);
    End;
    DeltaSinceLastUpdate := 0;
  End
  Else Begin
    // Anfahren der Richtigen Position (Mittelpunkt, vom Kollisionauf der Karte)
    glTranslatef((width * MapBlockSize) / 2, -(height * MapBlockSize) / 2 + MapBlockSize, 0);
    // Der Progressbar
    If FPausing Then Begin
      RenderBar(0);
    End
    Else Begin
      RenderBar(DeltaSinceLastUpdate / stages[fUpdating.FinState].BuildTime);
    End;
    // Die Baustelle zeichnen (ist hinterher, dass wir kein Offset in der Höhe brauchen)
    glcolor3f(1, 1, 1);
    RenderObjItem(point(0, 0), round(width * MapBlockSize), round(height * MapBlockSize), BuildBuildingTex);
  End;
  glPopMatrix;
End;

Procedure TBuilding.RenderRange;
Var
  i: integer;
  r: Single;
Begin
  // Nen Range gibts nicht immer
  If stage < 0 Then exit;
  If Stages[Stage].range > 0 Then Begin
    glLineStipple(1, $00FF);
    glEnable(GL_LINE_STIPPLE);
    glColor(Building_Range_Col);
    glBindTexture(GL_TEXTURE_2D, 0);
    glPushMatrix;
    glTranslatef((Stages[Stage].bulletLeafPointx * MapBlockSize), -(Stages[Stage].bulletLeafPointy * MapBlockSize) + MapBlockSize, 0);
    r := MapBlockSize * Stages[Stage].range;
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

Function TBuilding.Check: String;
Var
  path: String;
  i: Integer;
Begin
  result := '';
  path := ExtractFilePath(Filename);
  If path = '' Then exit('Invalid filename');
  path := path + PathDelim;
  If Name = '' Then exit('Missing Name');
  If image = '' Then exit('Missing Image.');
  If Not FileExists(path + image) Then exit('Invalide Image');
  // TODO: Hier fehlt noch viel..
  For i := 0 To high(Stages) Do Begin
    If Stages[i].image = '' Then exit('Missing stage image in stage ' + inttostr(i + 1));
    If Not FileExists(path + Stages[i].image) Then exit('Invalide image in stage' + inttostr(i + 1));
    If Stages[i].range > 0 Then Begin
      If Stages[i].bulletimage = '' Then exit('Missing stage bullet image in Stage ' + inttostr(i + 1));
      If Not FileExists(path + Stages[i].bulletimage) Then exit('Invalide bullet image in stage ' + inttostr(i + 1));
    End;
    If Stages[i].range < 0 Then Begin
      exit('Invalid tower range in stage ' + inttostr(i + 1) + ' has to be >= 0.');
    End;
  End;
End;

Function TBuilding.CalcSellRefund(Difficulty: integer): integer;
Var
  i: Integer;
Begin
  result := 0;
  // Wenn das Gebäude im Bau ist, dann zeigen wir den Sell Refunde der Stage 1 an ;)
  For i := 0 To max(0, Stage) Do Begin
    result := result + Stages[i].Cost * Refund[Difficulty];
  End;
  result := result Div 100;
End;

Function TBuilding.CalculateCost: integer;
Var
  i: Integer;
Begin
  result := 0;
  // Wenn das Gebäude im Bau ist, dann zeigen wir den Sell Refunde der Stage 1 an ;)
  For i := 0 To max(0, Stage) Do Begin
    result := result + Stages[i].Cost;
  End;
  result := result;
End;

{$ENDIF}

Procedure TBuilding.SetStage(value: integer);
Begin
  Stage := value;
End;

End.


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
Unit ubuilding_stage_frame;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, ExtDlgs,
  uctd_building, uctd_map;

Type

  { TBuilding_stage }

  TBuilding_stage = Class(TFrame)
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit20: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label27: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit20Change(Sender: TObject);
    Procedure Edit8KeyPress(Sender: TObject; Var Key: char);
    Procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Image2Paint(Sender: TObject);
  private
    { private declarations }
    Procedure LoadGebStageImage(Filename: String);
    Procedure LoadGebStageBulletImage(Filename: String);
  public
    { public declarations }
    BulletImg: String;
    Img: String;
    ReadPreviousStage: TNotifyEvent;
    FolderName: String;
    Procedure ResetStage;
    Procedure ApplyStage(Const Stage: TStage; Const Map: TMap);
    Function ReadStage(): TStage;
  End;

Implementation

{$R *.lfm}

Uses uctd_common, Graphics, lazutf8, LazFileUtils, uvectormath, uopengl_animation;

{ TBuilding_stage }

Procedure TBuilding_stage.Button3Click(Sender: TObject);
Begin
  // Laden des Bildes für die Anzeige auf der Karte
  InitOpenDialog(OpenPictureDialog1, FolderName);
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, FolderName) Then Begin
      img := OpenPictureDialog1.FileName;
      LoadGebStageImage(img);
      edit5.text := format('%dx%d', [round(image2.Picture.Bitmap.Width / TextrureImportMapBlockSize), round(image2.Picture.Bitmap.Height / TextrureImportMapBlockSize)]);
      edit20.text := format('%.2fx%.2f', [image2.Picture.Bitmap.Width / (2 * TextrureImportMapBlockSize), (0.75 * image2.Picture.Bitmap.Height) / TextrureImportMapBlockSize]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure TBuilding_stage.Button1Click(Sender: TObject);
Begin
  ReadPreviousStage(self);
End;

Procedure TBuilding_stage.Button4Click(Sender: TObject);
Begin
  // Laden des Bildes für die Anzeige auf der Karte
  InitOpenDialog(OpenPictureDialog1, FolderName);
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, FolderName) Then Begin
      bulletimg := OpenPictureDialog1.FileName;
      LoadGebStageBulletImage(BulletImg);
      edit6.text := format('%.2fx%.2f', [image3.Picture.Bitmap.Width / TextrureImportMapBlockSize, image3.Picture.Bitmap.Height / TextrureImportMapBlockSize]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure TBuilding_stage.Edit20Change(Sender: TObject);
Begin
  Image2.Invalidate;
End;

Procedure TBuilding_stage.Edit8KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = ',' Then key := '.';
End;

Procedure TBuilding_stage.Image2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  dx, dy, fx, fy: Single;
Begin
  y := Image2.Height - y;
  fx := x / TextrureImportMapBlockSize;
  fy := y / TextrureImportMapBlockSize;
  Try
    dx := strtoint(copy(Edit5.Text, 1, pos('x', edit5.text) - 1));
  Except
    dx := image2.Width / TextrureImportMapBlockSize;
  End;
  Try
    dy := strtoint(copy(Edit5.Text, pos('x', edit5.text) + 1, length(edit5.text)));
  Except
    dy := image2.Height / TextrureImportMapBlockSize;
  End;
  fx := ConvertDimension(0, Image2.Width, x, 0, dx);
  fy := ConvertDimension(0, Image2.Height, y, 0, dy);
  Edit20.text := format('%.2fx%.2f', [fx, fy]); // BulletLeafpoint
  image2.Invalidate;
End;

Procedure TBuilding_stage.Image2Paint(Sender: TObject);
Var
  sd, s: String;
  dx, dy, x, y: single;
Begin
  s := edit20.Text;
  sd := Edit5.Text;
  Try
    x := strtofloat(copy(s, 1, pos('x', s) - 1));
    y := strtofloat(copy(s, pos('x', s) + 1, length(s)));
    dx := strtoint(copy(sd, 1, pos('x', sd) - 1));
    dy := strtoint(copy(sd, pos('x', sd) + 1, length(sd)));
    x := ConvertDimension(0, dx, x, 0, Image2.Width);
    y := ConvertDimension(0, dy, y, 0, Image2.Height);

    y := Image2.Height - y;
  Except
    exit;
  End;
  image2.canvas.pen.Color := clred;
  image2.canvas.Brush.Color := clwhite;
  image2.canvas.Ellipse(round(x) - 3, round(y) - 3, round(x) + 3, round(y) + 3);
End;

Procedure TBuilding_stage.LoadGebStageImage(Filename: String);
Var
  a: TOpenGL_Animation;
  b: Tbitmap;
Begin
  If lowercase(ExtractFileExt(Filename)) = '.ani' Then Begin
    a := TOpenGL_Animation.Create;
    a.LoadFromFile(Filename, false);
    b := a.GetFirstBitmap();
    image2.Picture.Assign(b);
    b.free;
    a.free;
  End
  Else Begin
    image2.Picture.LoadFromFile(Filename);
  End;
  image2.Picture.Bitmap.TransparentColor := clFuchsia;
  image2.Transparent := true;
End;

Procedure TBuilding_stage.LoadGebStageBulletImage(Filename: String);
Var
  a: TOpenGL_Animation;
  b: Tbitmap;
Begin
  If lowercase(ExtractFileExt(Filename)) = '.ani' Then Begin
    a := TOpenGL_Animation.Create;
    a.LoadFromFile(Filename, false);
    b := a.GetFirstBitmap();
    image3.Picture.Assign(b);
    b.free;
    a.free;
  End
  Else Begin
    image3.Picture.LoadFromFile(Filename);
  End;
  image3.Picture.Bitmap.TransparentColor := clFuchsia;
  image3.Transparent := true;
End;

Procedure TBuilding_stage.ResetStage;
Begin
  memo1.clear; // Description
  Edit1.text := '0'; // StageBuild time
  Edit5.text := '2x2'; // Size
  Edit7.text := '100'; // Cost
  Image2.Picture.Clear; // Image der Stage
  Edit8.text := '5.0'; // Range
  Edit9.text := '2500'; // Reload time
  Edit10.text := '0'; // Earn
  Edit11.text := '5.0'; // Bulletspeed
  Edit12.text := '0.0'; // Splash radius
  Edit13.text := '0'; // Damage stitch
  Edit14.text := '0'; // Damage Poision
  Edit15.text := '0'; // Damage Magic
  Edit16.text := '0'; // Damage Air
  Image3.Picture.Clear; // Bullet Image der Stage
  Edit6.text := '0.0x0.0'; // Bullet Size
  Edit19.text := '1.0'; // Static
  Edit18.text := '1.0'; // Dynamic
  Edit17.text := '0'; // Time
  Edit20.text := '1.0x1.75'; // Time
  BulletImg := '';
  Img := '';
End;

Procedure TBuilding_stage.ApplyStage(Const Stage: TStage; Const Map: TMap);
Var
  ap, s: String;
Begin
  ap := IncludeTrailingPathDelimiter(FolderName);
  memo1.text := DeSerialize(Stage.Description); // Description
  Edit5.text := format('%dx%d', [Stage.w, Stage.h]); // Size
  Edit20.text := format('%.2fx%.2f', [Stage.bulletLeafPointx, Stage.bulletLeafPointy]); // BulletLeafpoint
  Edit7.text := inttostr(Stage.Cost); // Cost
  img := ap + Stage.image;
  If FileExistsUTF8(img) And (trim(Stage.image) <> '') Then Begin
    LoadGebStageImage(img); // Image der Stage
  End
  Else Begin
    image2.Picture.Clear; // Image der Stage
    img := '';
  End;
  Edit8.text := format('%.2f', [Stage.range]); // Range
  Edit9.text := inttostr(Stage.reloadtime); // Reload time
  Edit10.text := inttostr(Stage.earn); // Earn
  Edit11.text := format('%.2f', [Stage.bulletspeed]); // Bulletspeed
  Edit12.text := format('%.2f', [Stage.bulletsplashradius]); // Splash radius
  Edit13.text := inttostr(Stage.bulletpower[0]); // Damage stitch
  Edit14.text := inttostr(Stage.bulletpower[1]); // Damage Poision
  Edit15.text := inttostr(Stage.bulletpower[2]); // Damage Magic
  Edit16.text := inttostr(Stage.bulletpower[3]); // Damage Air
  bulletimg := ap + Stage.bulletimage;
  If FileExistsUTF8(bulletimg) And (trim(Stage.bulletimage) <> '') Then Begin
    LoadGebStageBulletImage(BulletImg);
  End
  Else Begin
    bulletimg := '';
  End;
  Edit6.text := format('%.2fx%.2f', [Stage.bulletw, Stage.bulleth]); // Bullet Size
  Edit19.text := format('%.2f', [Stage.SlowDown.slowdownstatic]); // Static
  Edit18.text := format('%.2f', [Stage.SlowDown.slowdowndynamic]); // Dynamic
  Edit17.text := inttostr(Stage.SlowDown.slowdowntime); // Time
  edit1.text := inttostr(stage.BuildTime); // BuldTime

  ap := '';
  If assigned(map) Then Begin
    ap := MapFolder + MapName + PathDelim;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass1Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image4.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image4.Picture.Clear;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass2Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image5.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image5.Picture.Clear;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass3Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image6.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image6.Picture.Clear;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass4Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image7.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image7.Picture.Clear;
  End;
End;

Function TBuilding_stage.ReadStage: TStage;
Var
  s: String;
Begin
  result.Description := Serialize(memo1.text); // Description
  s := Edit5.text; // Size
  result.w := strtoint(copy(s, 1, pos('x', s) - 1));
  result.h := strtoint(copy(s, pos('x', s) + 1, length(s)));
  s := Edit20.text; // BulletLeafpoint
  result.bulletLeafPointx := strtofloat(copy(s, 1, pos('x', s) - 1));
  result.bulletLeafPointy := strtofloat(copy(s, pos('x', s) + 1, length(s)));
  result.cost := strtoint(Edit7.text); // Cost
  result.image := ExtractFilename(img);
  result.range := strtofloat(Edit8.text); // Range
  result.reloadtime := strtoint(Edit9.text); // Reload time
  result.earn := strtoint(Edit10.text); // Earn
  result.bulletspeed := strtofloat(Edit11.text); // Bulletspeed
  result.bulletsplashradius := strtofloat(Edit12.text); // Splash radius
  result.bulletpower[0] := strtoint(Edit13.text); // Damage stitch
  result.bulletpower[1] := strtoint(Edit14.text); // Damage Poision
  result.bulletpower[2] := strtoint(Edit15.text); // Damage Magic
  result.bulletpower[3] := strtoint(Edit16.text); // Damage Air
  result.bulletimage := ExtractFileName(BulletImg);
  s := Edit6.text; // Bullet Size
  result.bulletw := strtofloat(copy(s, 1, pos('x', s) - 1));
  result.bulleth := strtofloat(copy(s, pos('x', s) + 1, length(s)));
  result.SlowDown.slowdownstatic := strtofloat(Edit19.text); // Static
  result.SlowDown.slowdowndynamic := strtofloat(Edit18.text); // Dynamic
  result.SlowDown.slowdowntime := strtoint(Edit17.text); // Time
  result.BuildTime := strtoint(edit1.text);
  result.Animation := Nil;
  result.BulletAnimation := Nil;
End;

End.


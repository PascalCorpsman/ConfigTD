Unit uhero_level_frame;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ExtDlgs, uctd_hero,
  uctd_map;

Type

  { THero_Level }

  THero_Level = Class(TFrame)
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox4: TCheckBox;
    Edit10: TEdit;
    Edit12: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
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
  private
    Procedure LoadHeroLevelImage(Filename: String);
    Procedure LoadHeroLevelBulletImage(Filename: String);

  public
    BulletImg: String;
    Img: String;
    ReadPreviousLevel: TNotifyEvent;
    FolderName: String;
    Procedure ResetLevel;
    Function ReadLevel(): THeroLevel;
    Procedure ApplyLevel(Const Level: THeroLevel; Const Map: TMap);
  End;

Implementation

{$R *.lfm}

Uses uctd_common, LazFileUtils, uopengl_animation, Graphics;

{ THero_Level }

Procedure THero_Level.Button3Click(Sender: TObject);
Begin
  // Laden des Bildes für die Anzeige auf der Karte
  InitOpenDialog(OpenPictureDialog1, FolderName);
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, FolderName) Then Begin
      img := OpenPictureDialog1.FileName;
      LoadHeroLevelImage(img);
      edit5.text := format('%.2fx%.2f', [image2.Picture.Bitmap.Width / TextrureImportMapBlockSize, image2.Picture.Bitmap.Height / TextrureImportMapBlockSize]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure THero_Level.Button1Click(Sender: TObject);
Begin
  ReadPreviousLevel(self);
End;

Procedure THero_Level.Button4Click(Sender: TObject);
Begin
  // Laden des Bildes für die Anzeige auf der Karte
  InitOpenDialog(OpenPictureDialog1, FolderName);
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, FolderName) Then Begin
      bulletimg := OpenPictureDialog1.FileName;
      LoadHeroLevelBulletImage(BulletImg);
      edit6.text := format('%.2fx%.2f', [image3.Picture.Bitmap.Width / TextrureImportMapBlockSize, image3.Picture.Bitmap.Height / TextrureImportMapBlockSize]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure THero_Level.LoadHeroLevelImage(Filename: String);
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

Procedure THero_Level.LoadHeroLevelBulletImage(Filename: String);
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

Procedure THero_Level.ResetLevel;
Begin
  memo1.clear;
  edit8.text := '1.0';
  edit7.text := '100';
  edit5.text := '0.0x0.0';
  edit9.text := '2.0';
  edit10.text := '1000';
  edit12.text := '5.0';
  edit14.text := '0';
  edit15.text := '0';
  edit16.text := '0';
  edit17.text := '0';
  edit6.text := '0.0x0.0';
  CheckBox4.Checked := false;
  CheckBox1.Checked := true;
End;

Function THero_Level.ReadLevel: THeroLevel;
Var
  s: String;
Begin
  result.Description := Serialize(Memo1.Text);
  result.image := ExtractFilename(img);
  result.HitpointsForNextLevel := strtoint(Edit7.text);
  result.Speed := strtofloat(Edit8.text);
  s := Edit5.text; // Size
  result.w := strtofloat(copy(s, 1, pos('x', s) - 1));
  result.h := strtofloat(copy(s, pos('x', s) + 1, length(s)));

  result.range := strtofloat(Edit9.text); // Range
  result.reloadtime := strtoint(Edit10.text); // Reload time

  result.bulletspeed := strtofloat(Edit12.text); // Bulletspeed
  result.bulletimage := ExtractFileName(BulletImg);
  s := Edit6.text; // Bullet Size
  result.bulletw := strtofloat(copy(s, 1, pos('x', s) - 1));
  result.bulleth := strtofloat(copy(s, pos('x', s) + 1, length(s)));
  result.Animation := Nil;
  result.BulletAnimation := Nil;
  result.bulletpower[0] := strtoint(Edit14.text); // Damage stitch
  result.bulletpower[1] := strtoint(Edit15.text); // Damage Poision
  result.bulletpower[2] := strtoint(Edit16.text); // Damage Magic
  result.bulletpower[3] := strtoint(Edit17.text); // Damage Air
  result.ImageRotation := CheckBox4.Checked;
  result.StopAnimationWhenStillStanding := CheckBox1.Checked;
End;

Procedure THero_Level.ApplyLevel(Const Level: THeroLevel; Const Map: TMap);
Var
  ap, s: String;
Begin
  ap := IncludeTrailingPathDelimiter(FolderName);
  memo1.text := DeSerialize(level.Description); // Description
  Edit5.text := format('%.2fx%.2f', [Level.w, Level.h]); // Bullet Size
  Edit8.text := format('%.2f', [Level.Speed]); // Range
  Edit7.text := inttostr(Level.HitpointsForNextLevel);

  img := ap + Level.image;
  If FileExistsUTF8(img) And (trim(Level.image) <> '') Then Begin
    LoadHeroLevelImage(img); // Image der Stage
  End
  Else Begin
    image2.Picture.Clear; // Image der Stage
    img := '';
  End;

  bulletimg := ap + Level.bulletimage;
  If FileExistsUTF8(bulletimg) And (trim(Level.bulletimage) <> '') Then Begin
    LoadHeroLevelBulletImage(BulletImg);
  End
  Else Begin
    bulletimg := '';
  End;

  Edit9.text := format('%.2f', [Level.range]); // Range
  Edit10.text := inttostr(Level.reloadtime); // Reload time
  Edit12.text := format('%.2f', [Level.bulletspeed]); // Bulletspeed

  Edit14.text := inttostr(Level.bulletpower[0]); // Damage stitch
  Edit15.text := inttostr(Level.bulletpower[1]); // Damage Poision
  Edit16.text := inttostr(Level.bulletpower[2]); // Damage Magic
  Edit17.text := inttostr(Level.bulletpower[3]); // Damage Air
  Edit6.text := format('%.2fx%.2f', [Level.bulletw, Level.bulleth]); // Bullet Size
  CheckBox4.Checked := Level.ImageRotation;
  CheckBox1.Checked := level.StopAnimationWhenStillStanding;
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

End.


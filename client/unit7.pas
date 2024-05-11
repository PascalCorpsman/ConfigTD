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
Unit unit7;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ExtDlgs, uctd_opp, uctd_map;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Label1: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit3KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Procedure LoadOppImage(Filename: String);
  public
    { public declarations }
    img: String;
    transfer: Boolean;
    Procedure ClearAll;
    Procedure opponenttolcl(Const Map: TMap);
    Procedure lcltoopponent;
  End;

Var
  Form7: TForm7;
  Opponent: TOpponent;

Implementation

Uses uctd_common, LazFileUtils, unit14, uopengl_animation;

{$R *.lfm}

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  Opponent := TOpponent.create();
  caption := 'Opponent editor';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm7.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm7.Button4Click(Sender: TObject);
Begin
  ClearAll;
End;

Procedure TForm7.Edit3KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = ',' Then key := '.';
End;

Procedure TForm7.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  form14.ReloadIndex;
  form14.Show;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Begin
  // Load Opponent Image
  InitOpenDialog(OpenPictureDialog1, IncludeTrailingPathDelimiter(ExtractFilePath(Opponent.Filename)));
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, ExtractFilePath(Opponent.Filename)) Then Begin
      img := OpenPictureDialog1.FileName;
      LoadOppImage(img);
      // Automatisch eine Größe Vorschlagen
      edit4.text := format('%.2fx%.2f', [image1.Picture.Bitmap.Width / TextrureImportMapBlockSize, image1.Picture.Bitmap.Height / TextrureImportMapBlockSize]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure TForm7.Button2Click(Sender: TObject);
Var
  s: String;
Begin
  // Speichern
  lcltoopponent;
  s := Opponent.Check();
  If s <> '' Then Begin
    showmessage(s + LineEnding + LineEnding +
      'Invalid opponents are not allowed to be saved.');
    exit;
  End;
  Opponent.SaveToFile(Opponent.Filename);
  TItemObject.Reload(Opponent.Filename);
  If transfer Then Begin
    If Not Form14.TransferShareServer(Opponent) Then Begin
      logshow('Could not transfer opponent to server, please retry in a few seconds.', llError);
    End;
  End;
  close;
End;

Procedure TForm7.FormDestroy(Sender: TObject);
Begin
  Opponent.Free;
  Opponent := Nil;
End;

Procedure TForm7.LoadOppImage(Filename: String);
Var
  a: TOpenGL_Animation;
  b: Tbitmap;
Begin
  If lowercase(ExtractFileExt(Filename)) = '.ani' Then Begin
    a := TOpenGL_Animation.Create;
    a.LoadFromFile(img, false);
    b := a.GetFirstBitmap();
    image1.Picture.Assign(b);
    b.free;
    a.free;
  End
  Else Begin
    image1.Picture.LoadFromFile(Filename);
  End;
  image1.Picture.Bitmap.TransparentColor := clFuchsia;
  image1.Transparent := true;
End;

Procedure TForm7.ClearAll;
Begin
  Edit1.text := '';
  memo1.Text := '';
  Edit3.text := '2.0';
  Edit4.text := '0.0x0.0';
  image1.Picture.Clear;
  img := '';
  Edit5.text := '0';
  Edit6.text := '0';
  Edit7.text := '0';
  Edit8.text := '0';

  Edit2.text := '0.5';
  Edit9.text := '1.0';
  Edit10.text := '1.5';

  CheckBox1.Checked := false;
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := true;
End;

Procedure TForm7.opponenttolcl(Const Map: TMap);
Var
  ap, s: String;
Begin
  s := IncludeTrailingPathDelimiter(ExtractFilePath(Opponent.Filename)) + opponent.Image;
  If FileExistsUTF8(s) And (trim(opponent.Image) <> '') Then Begin
    img := s;
    LoadOppImage(img);
  End
  Else Begin
    image1.Picture.Clear;
    img := '';
  End;
  ap := '';
  If assigned(map) Then Begin
    ap := MapFolder + MapName + PathDelim;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass1Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image2.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image2.Picture.Clear;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass2Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image3.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image3.Picture.Clear;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass3Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image4.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image4.Picture.Clear;
  End;
  s := '';
  If assigned(map) Then s := Map.DamageClass4Tex;
  If (s <> '') And (FileExistsUTF8(ap + s)) Then Begin
    Image5.Picture.LoadFromFile(ap + s);
  End
  Else Begin
    Image5.Picture.Clear;
  End;
  Edit1.Text := opponent.name;
  memo1.Text := DeSerialize(opponent.Description);
  edit3.text := format('%.2f', [opponent.Speed]);
  edit4.text := format('%fx%f', [opponent.SizeX, opponent.SizeY]);
  edit5.text := inttostr(opponent.LifePoints[0]);
  edit6.text := inttostr(opponent.LifePoints[1]);
  edit7.text := inttostr(opponent.LifePoints[2]);
  edit8.text := inttostr(opponent.LifePoints[3]);
  edit2.text := format('%.2f', [opponent.LifeFactors[0]]);
  edit9.text := format('%.2f', [opponent.LifeFactors[1]]);
  edit10.text := format('%.2f', [opponent.LifeFactors[2]]);
  CheckBox1.Checked := opponent.Canfly;
  CheckBox2.Checked := opponent.Boss;
  CheckBox3.Checked := opponent.Bonus;
  CheckBox4.Checked := opponent.ImageRotation;
End;

Procedure TForm7.lcltoopponent;
Var
  s: String;
Begin
  DefaultFormatSettings.DecimalSeparator := '.';
  opponent.name := Edit1.Text;
  opponent.Description := Serialize(memo1.Text);
  opponent.Speed := strtofloat(edit3.text);
  s := edit4.text;
  opponent.SizeX := strtofloat(copy(s, 1, pos('x', s) - 1));
  opponent.sizey := strtofloat(copy(s, pos('x', s) + 1, length(s)));
  opponent.LifePoints[0] := strtoint(edit5.text);
  opponent.LifePoints[1] := strtoint(edit6.text);
  opponent.LifePoints[2] := strtoint(edit7.text);
  opponent.LifePoints[3] := strtoint(edit8.text);
  opponent.LifeFactors[0] := strtofloat(edit2.text);
  opponent.LifeFactors[1] := strtofloat(edit9.text);
  opponent.LifeFactors[2] := strtofloat(edit10.text);
  opponent.Canfly := CheckBox1.Checked;
  opponent.Boss := CheckBox2.Checked;
  opponent.Bonus := CheckBox3.Checked;
  opponent.Image := ExtractFileName(img);
  opponent.ImageRotation := CheckBox4.Checked;
End;

End.


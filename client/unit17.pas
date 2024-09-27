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
Unit unit17;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ExtDlgs, uctd_maptext;

Type

  { TForm17 }

  TForm17 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    fMapTex: TMapTex;
  public
    { public declarations }
    Procedure LoadMap;
  End;

Var
  Form17: TForm17;

Implementation

{$R *.lfm}

Uses uctd_common, LazFileUtils;

{ TForm17 }

Procedure TForm17.FormCreate(Sender: TObject);
Begin
  caption := 'Automated backtex editor';
  fMapTex := TMapTex.Create;
  edit1.text := '10';
  edit2.text := '5';
End;

Procedure TForm17.FormDestroy(Sender: TObject);
Begin
  fMapTex.Free;
End;

Procedure TForm17.LoadMap;
Var
  img: TBitmap;
Begin
  fMapTex.LoadMap(mapname);
  img := fMapTex.MapPreview();
  image1.Picture.Assign(img);
  img.free;
End;

Procedure TForm17.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm17.Button2Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    image2.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    fMapTex.NichtsImage := OpenPictureDialog1.FileName;
  End;
End;

Procedure TForm17.Button3Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    image3.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    fMapTex.BegehbarImage := OpenPictureDialog1.FileName;
  End;
End;

Procedure TForm17.Button4Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    image4.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    fMapTex.BebaubarImage := OpenPictureDialog1.FileName;
  End;
End;

Procedure TForm17.Button5Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    image5.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    fMapTex.BeidesImage := OpenPictureDialog1.FileName;
  End;
End;

Procedure TForm17.Button6Click(Sender: TObject);
Var
  img: TBitmap;
Begin
  If Not FileExistsUTF8(fMapTex.NichtsImage) Then Begin
    logshow(format('Missing %s texture', [label2.caption]), llError);
    exit;
  End;
  If Not FileExistsUTF8(fMapTex.BebaubarImage) Then Begin
    logshow(format('Missing %s texture', [label4.caption]), llError);
    exit;
  End;
  If Not FileExistsUTF8(fMapTex.BegehbarImage) Then Begin
    logshow(format('Missing %s texture', [label3.caption]), llError);
    exit;
  End;
  If Not FileExistsUTF8(fMapTex.BeidesImage) Then Begin
    logshow(format('Missing %s texture', [label5.caption]), llError);
    exit;
  End;
  img := fMapTex.CreateTexture(strtoint(edit1.text), StrToInt(Edit2.text));
  image6.Picture.Assign(img);
  img.free;
End;

Procedure TForm17.Button7Click(Sender: TObject);
Var
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
Begin
  If SaveDialog1.Execute Then Begin
    If lowercase(ExtractFileExt(SaveDialog1.FileName)) = '.png' Then Begin
      png := TPortableNetworkGraphic.Create;
      png.Assign(Image6.Picture.Bitmap);
      png.SaveToFile(SaveDialog1.FileName);
      png.free;
    End
    Else Begin
      jp := TJPEGImage.Create;
      jp.Assign(Image6.Picture.Bitmap);
      jp.SaveToFile(SaveDialog1.FileName);
      jp.free;
    End;
  End;
End;

End.


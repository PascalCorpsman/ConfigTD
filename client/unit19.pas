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
Unit Unit19;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ExtDlgs, uctd_hero, uctd_map;

Type

  { TForm19 }

  TForm19 = Class(TForm)
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    Procedure LoadHeroImage(Filename: String);
  public
    img: String;
    transfer: Boolean;
    Procedure ClearAll;
    Procedure Herotolcl(Const Map: TMap);
    Procedure AddLevel;
    Procedure DelLastLevel;
    Procedure ResetPage(Const Page: TTabSheet);
    Procedure ReadPreviousLevelCallback(Sender: TObject);
    Procedure lcltohero;
  End;

Var
  Form19: TForm19;
  Hero: THero;

Implementation

{$R *.lfm}

Uses
  unit14, math, LazFileUtils,
  uhero_level_frame, uopengl_animation, uctd_common,
  uctd;

{ TForm19 }

Procedure TForm19.FormCreate(Sender: TObject);
Begin
  hero := THero.Create();
  caption := 'Hero editor';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  PageControl1.Pages[0].Free;
End;

Procedure TForm19.Button5Click(Sender: TObject);
Var
  j: Integer;
Begin
  // Übernahme der neuen Stage Zahl
  j := strtoint(edit4.text);
  While PageControl1.PageCount < j Do Begin
    AddLevel;
  End;
  While PageControl1.PageCount > j Do Begin
    DelLastLevel;
  End;
End;

Procedure TForm19.Button6Click(Sender: TObject);
Begin
  // Load Buymenu Image
  InitOpenDialog(OpenPictureDialog1, ExtractFilePath(Hero.Filename));
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, ExtractFilePath(Hero.Filename)) Then Begin
      img := OpenPictureDialog1.FileName;
      LoadHeroImage(img);
      // Automatisch eine Größe Vorschlagen
      edit2.text := format('%dx%d', [round(image1.Picture.Bitmap.Width / TextrureImportMapBlockSize), round(image1.Picture.Bitmap.Height / TextrureImportMapBlockSize)]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure TForm19.Button2Click(Sender: TObject);
Var
  s: String;
Begin
  // Speichern
  lcltohero;
  s := hero.Check();
  If s <> '' Then Begin
    showmessage(s + LineEnding + LineEnding +
      'Invalid heros are not allowed to be saved.');
    exit;
  End;
  hero.SaveToFile(hero.Filename);
  TItemObject.Reload(hero.Filename);
  If transfer Then Begin
    If Not Form14.TransferShareServer(Hero) Then Begin
      logshow('Could not transfer hero to server, please retry in a few seconds.', llError);
      exit;
    End;
  End;
  close;
End;

Procedure TForm19.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm19.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  form14.ReloadIndex;
  form14.Show;
End;

Procedure TForm19.FormDestroy(Sender: TObject);
Begin
  hero.Free;
  hero := Nil;
End;

Procedure TForm19.LoadHeroImage(Filename: String);
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

Procedure TForm19.ClearAll;
Begin
  img := '';
  Image1.Picture.Clear;
  Hero.Free;
  Hero := THero.create();
  edit2.text := '0x0';
  edit11.text := '0';
  edit12.text := '0';
  edit4.text := '1';
  edit3.text := '';
  While PageControl1.PageCount > 1 Do
    DelLastLevel;
  If (PageControl1.PageCount = 0) Then AddLevel;
  ResetPage(PageControl1.Pages[0]);
End;

Procedure TForm19.Herotolcl(Const Map: TMap);
Var
  ap: String;
  i: integer;
  f: THero_Level;
Begin
  ap := IncludeTrailingPathDelimiter(ExtractFilePath(Hero.Filename));
  edit3.Text := Hero.Name;
  edit11.text := inttostr(Hero.Cost);
  edit12.text := inttostr(Hero.BuildTime);
  edit4.text := inttostr(max(1, high(Hero.Levels) + 1));
  button5.Click; // Übernehmen der Levels
  img := ap + Hero.image;
  If FileExistsUTF8(img) And (Hero.image <> '') Then Begin
    LoadHeroImage(img);
  End
  Else Begin
    image1.Picture.Clear;
  End;
  edit2.text := format('%dx%d', [Round(Hero.Width), Round(Hero.Height)]);
  For i := 0 To high(Hero.Levels) Do Begin
    f := PageControl1.Pages[i].Components[0] As THero_Level;
    f.FolderName := ExtractFilePath(Hero.Filename);
    f.ApplyLevel(Hero.Levels[i], Map);
  End;
  PageControl1.ActivePageIndex := 0;
End;

Procedure TForm19.AddLevel;
Var
  p: TTabSheet;
  f: THero_Level;
Begin
  p := PageControl1.AddTabSheet;
  p.Name := 'Level' + inttostr(PageControl1.PageCount);
  p.tag := PageControl1.PageCount - 1;
  f := THero_Level.Create(p);
  f.Name := 'FrameStage' + inttostr(PageControl1.PageCount);
  f.Parent := p;
  f.Align := alClient;
  f.ResetLevel;
  f.FolderName := ExtractFilePath(hero.Filename);
  f.ReadPreviousLevel := @ReadPreviousLevelCallback;
  PageControl1.PageIndex := PageControl1.PageCount - 1;
End;

Procedure TForm19.DelLastLevel;
Begin
  PageControl1.Pages[PageControl1.PageCount - 1].free;
End;

Procedure TForm19.ResetPage(Const Page: TTabSheet);
Var
  f: THero_Level;
Begin
  f := page.Components[0] As THero_Level;
  f.ResetLevel;
End;

Procedure TForm19.ReadPreviousLevelCallback(Sender: TObject);
Var
  f: THero_Level;
  l: THeroLevel;
Begin
  f := THero_Level(sender);
  If f.Parent.Tag > 0 Then Begin
    l := (PageControl1.Pages[f.Parent.Tag - 1].Components[0] As THero_Level).ReadLevel;
    f.FolderName := ExtractFilePath(Hero.Filename);
    f.ApplyLevel(l, ctd.map);
  End;
End;

Procedure TForm19.lcltohero;
Var
  i: Integer;
  f: THero_Level;
Begin
  DefaultFormatSettings.DecimalSeparator := '.';
  Hero.name := Edit3.text;
  Hero.Width := strtoint(copy(edit2.text, 1, pos('x', edit2.text) - 1));
  Hero.Height := strtoint(copy(edit2.text, pos('x', edit2.text) + 1, length(edit2.text)));
  setlength(Hero.Levels, strtoint(edit4.text));
  Hero.BuildTime := strtoint(edit12.text);
  Hero.Cost := strtoint(edit11.text);
  Hero.image := ExtractFileName(img);
  For i := 0 To high(Hero.Levels) Do Begin
    f := PageControl1.Pages[i].Components[0] As THero_Level;
    Hero.Levels[i] := f.ReadLevel();
  End;
End;

End.


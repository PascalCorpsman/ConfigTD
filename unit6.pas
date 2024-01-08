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
Unit unit6;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ExtDlgs, LazFileUtils, uctd_building, uctd_map;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog2: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    TabSheet1: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Procedure LoadGebImage(Filename: String);
  public
    { public declarations }
    img: String;
    transfer: Boolean; // True, wenn bei OK das Gebäude an den Server Kopiert werden muss
    Procedure ClearAll;
    Procedure buildingtolcl(Const Map: TMap);
    Procedure AddStage;
    Procedure DelLastStage;
    Procedure ResetPage(Const Page: TTabSheet);
    Procedure ReadPreviousStageCallback(Sender: TObject);
    Procedure lcltoBuilding;
  End;

Var
  Form6: TForm6;
  Building: TBuilding;

Implementation

{$R *.lfm}

Uses
  uctd,
  ubuilding_stage_frame, uctd_common, math, unit14, uopengl_animation;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Building editor';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  Building := TBuilding.create;
  PageControl1.Pages[0].Free;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  j: Integer;
Begin
  // Übernahme der neuen Stage Zahl
  j := strtoint(edit3.text);
  While PageControl1.PageCount < j Do Begin
    AddStage;
  End;
  While PageControl1.PageCount > j Do Begin
    DelLastStage;
  End;
End;

Procedure TForm6.Button2Click(Sender: TObject);
Begin
  // Load Buymenu Image
  InitOpenDialog(OpenPictureDialog1, ExtractFilePath(Building.Filename));
  If OpenPictureDialog1.Execute Then Begin
    If LoadFileToMyPath(OpenPictureDialog1.FileName, ExtractFilePath(Building.Filename)) Then Begin
      img := OpenPictureDialog1.FileName;
      LoadGebImage(img);
      // Automatisch eine Größe Vorschlagen
      edit2.text := format('%dx%d', [round(image1.Picture.Bitmap.Width / TextrureImportMapBlockSize), round(image1.Picture.Bitmap.Height / TextrureImportMapBlockSize)]);
    End
    Else Begin
      LogShow('Could not load :' + OpenPictureDialog1.FileName, llerror);
    End;
  End;
End;

Procedure TForm6.Button3Click(Sender: TObject);
Var
  s: String;
Begin
  form6.lcltoBuilding;
  s := Building.Check();
  If s <> '' Then Begin
    showmessage(s + LineEnding + LineEnding +
      'Invalid Buildings are not allowed to be saved.');
    exit;
  End;
  Building.savetoFile(Building.Filename);
  If transfer Then Begin
    If Not Form14.TransferShareServer(Building) Then Begin
      logshow('Could not transfer building to server, please retry in a few seconds.', llError);
      exit;
    End;
  End;
  close;
End;

Procedure TForm6.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm6.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  form14.Show;
End;

Procedure TForm6.FormDestroy(Sender: TObject);
Begin
  Building.Free;
  Building := Nil;
End;

Procedure TForm6.LoadGebImage(Filename: String);
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

Procedure TForm6.ClearAll;
Begin
  img := '';
  Image1.Picture.Clear;
  Building.Free;
  Building := TBuilding.create();
  edit1.text := '';
  edit2.text := '2x2';
  edit3.text := '1';
  edit4.text := 'Unknown';
  edit20.text := '100';
  edit21.text := '90';
  edit22.text := '80';
  While PageControl1.PageCount > 1 Do
    DelLastStage;
  //  If Not form6ShowOnce Then Begin // Wenn das Formular schon mal Sichtbar war
  If (PageControl1.PageCount = 0) Then AddStage;
  ResetPage(PageControl1.Pages[0]);
  //  End;
End;

Procedure TForm6.buildingtolcl(Const Map: TMap);
Var
  ap: String;
  i: Integer;
  f: TBuilding_stage;
Begin
  ap := IncludeTrailingPathDelimiter(ExtractFilePath(Building.Filename));
  edit1.text := Building.name;
  edit2.text := format('%dx%d', [Round(Building.Width), Round(Building.Height)]);
  edit3.text := inttostr(max(1, high(Building.Stages) + 1));
  edit20.text := inttostr(Building.Refund[0]);
  edit21.text := inttostr(Building.Refund[1]);
  edit22.text := inttostr(Building.Refund[2]);
  button1.Click; // Übernehmen der Stages
  edit4.text := Building.Category;
  img := ap + Building.image;
  If FileExistsUTF8(img) And (Building.image <> '') Then Begin
    LoadGebImage(img);
  End
  Else Begin
    image1.Picture.Clear;
  End;
  For i := 0 To high(Building.Stages) Do Begin
    f := PageControl1.Pages[i].Components[0] As TBuilding_stage;
    f.FolderName := ExtractFilePath(Building.Filename);
    f.ApplyStage(Building.Stages[i], Map);
  End;
  PageControl1.ActivePageIndex := 0;
End;

Procedure TForm6.AddStage;
Var
  p: TTabSheet;
  f: TBuilding_stage;
Begin
  p := PageControl1.AddTabSheet;
  p.Name := 'Stage' + inttostr(PageControl1.PageCount);
  p.tag := PageControl1.PageCount - 1;
  f := TBuilding_stage.Create(p);
  f.Name := 'FrameStage' + inttostr(PageControl1.PageCount);
  f.Parent := p;
  f.Align := alClient;
  f.ResetStage;
  f.FolderName := ExtractFilePath(Building.Filename);
  f.ReadPreviousStage := @ReadPreviousStageCallback;
  PageControl1.PageIndex := PageControl1.PageCount - 1;
End;

Procedure TForm6.DelLastStage;
Begin
  PageControl1.Pages[PageControl1.PageCount - 1].free;
End;

Procedure TForm6.ResetPage(Const Page: TTabSheet);
Var
  f: TBuilding_stage;
Begin
  f := page.Components[0] As TBuilding_stage;
  f.ResetStage;
End;

Procedure TForm6.ReadPreviousStageCallback(Sender: TObject);
Var
  f: TBuilding_stage;
  s: TStage;
Begin
  f := TBuilding_stage(sender);
  If f.Parent.Tag > 0 Then Begin
    s := (PageControl1.Pages[f.Parent.Tag - 1].Components[0] As TBuilding_stage).ReadStage;
    f.FolderName := ExtractFilePath(Building.Filename);
    f.ApplyStage(s, ctd.map);
  End;
End;

Procedure TForm6.lcltoBuilding;
Var
  i: Integer;
  f: TBuilding_stage;
Begin
  DefaultFormatSettings.DecimalSeparator := '.';
  Building.name := edit1.text;
  Building.Width := strtoint(copy(edit2.text, 1, pos('x', edit2.text) - 1));
  Building.Height := strtoint(copy(edit2.text, pos('x', edit2.text) + 1, length(edit2.text)));
  setlength(Building.Stages, strtoint(edit3.text));
  Building.Category := edit4.text;
  Building.image := ExtractFileName(img);
  Building.Refund[0] := strtoint(edit20.text);
  Building.Refund[1] := strtoint(edit21.text);
  Building.Refund[2] := strtoint(edit22.text);
  For i := 0 To high(Building.Stages) Do Begin
    f := PageControl1.Pages[i].Components[0] As TBuilding_stage;
    Building.Stages[i] := f.ReadStage();
  End;
End;

End.


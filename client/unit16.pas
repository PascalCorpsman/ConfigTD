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
Unit unit16;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

Type

  TGameList = Record
    name: String;
    bakup: Byte;
  End;

  { TForm16 }

  TForm16 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    Procedure Button1Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
    fList: Array Of TGameList;
  public
    { public declarations }
    Procedure LoadList(Const Stream: TStream);
  End;

Var
  Form16: TForm16;

Implementation

{$R *.lfm}

Uses LazFileUtils, uctd, uctd_common;

{ TForm16 }

Procedure TForm16.FormCreate(Sender: TObject);
Begin

End;

Procedure TForm16.ListBox1Click(Sender: TObject);
Begin
  If Button1.caption = 'Save' Then Begin
    edit1.text := ExtractFileNameWithoutExt(RemoveTimestampInfoFromFilename(ListBox1.Items[ListBox1.ItemIndex]));
  End;
End;

Procedure TForm16.ListBox1DblClick(Sender: TObject);
Begin
  If Button1.caption = 'Load' Then Begin
    Button1.Click;
  End;
End;

Procedure TForm16.MenuItem1Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  // Delete Savegame
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ExtractFileNameWithoutExt(RemoveTimestampInfoFromFilename(ListBox1.Items[ListBox1.ItemIndex]));
    ctd.DelSaveGame(s);
    For i := ListBox1.ItemIndex To high(fList) - 1 Do Begin
      fList[i] := fList[i + 1];
    End;
    setlength(fList, high(fList));
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  End;
End;

Procedure TForm16.MenuItem2Click(Sender: TObject);
Begin
  // Delete All Savegames
  ctd.DelAllSaveGames();
  ListBox1.Items.Clear;
  setlength(fList, 0);
End;

Procedure TForm16.LoadList(Const Stream: TStream);
Var
  j, i: integer;
Begin
  i := 0;
  Stream.Read(i, sizeof(i));
  setlength(fList, i);
  For j := 0 To i - 1 Do Begin
    fList[j].name := Stream.ReadAnsiString;
  End;
  For j := 0 To i - 1 Do Begin
    fList[j].bakup := 0;
    stream.Read(fList[j].bakup, SizeOf(fList[j].bakup));
  End;
End;

Procedure TForm16.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.Click;
End;

Procedure TForm16.Button1Click(Sender: TObject);
Begin
  If Edit1.Visible Then Begin
    Edit1.Text := trim(Edit1.Text);
    If trim(edit1.text) = '' Then Begin
      exit;
    End;
  End;
  ModalResult := mrOK;
End;

Procedure TForm16.CheckBox1Change(Sender: TObject);
Var
  i: Integer;
Begin
  // Aktualisieren der Listbox
  ListBox1.Clear;
  For i := 0 To high(fList) Do Begin
    If CheckBox1.Checked Then Begin
      ListBox1.Items.Add(fList[i].name);
    End
    Else Begin
      If fList[i].bakup = 0 Then Begin
        ListBox1.Items.Add(fList[i].name);
      End;
    End;
  End;
End;

End.


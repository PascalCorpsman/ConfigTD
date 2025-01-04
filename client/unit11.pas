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
Unit unit11;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm11 }

  TForm11 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    ListBox1: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    { private declarations }
    Procedure SelectDifficulty(index: integer);
  public
    { public declarations }
  End;

Var
  Form11: TForm11;
  form11Difficulty: integer = -1;

Implementation

Uses uctd, uctd_common, math;

{$R *.lfm}

Var
  Form11ShowOnce: Boolean = false;

  { TForm11 }

Procedure TForm11.FormCreate(Sender: TObject);
Begin
  setValue('AskDifficultyForm', 'Left', inttostr(left));
  setValue('AskDifficultyForm', 'Top', inttostr(top));
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  caption := 'Start new round..';
End;

Procedure TForm11.Button1Click(Sender: TObject);
Begin
  SelectDifficulty(0);
End;

Procedure TForm11.Button2Click(Sender: TObject);
Begin
  SelectDifficulty(1);
End;

Procedure TForm11.Button3Click(Sender: TObject);
Begin
  SelectDifficulty(2);
End;

Procedure TForm11.Button4Click(Sender: TObject);
Begin
  ModalResult := mrCancel;
End;

Procedure TForm11.Button5Click(Sender: TObject);
Begin
  // Player 1 Up
  If listbox1.ItemIndex = -1 Then Begin
    showmessage('Error, no player selected.');
    exit;
  End;
  ctd.ChangePlayerPos(listbox1.ItemIndex, true);
  listbox1.ItemIndex := max(0, listbox1.ItemIndex - 1);
End;

Procedure TForm11.Button6Click(Sender: TObject);
Begin
  // Player 1 Down
  If listbox1.ItemIndex = -1 Then Begin
    showmessage('Error, no player selected.');
    exit;
  End;
  ctd.ChangePlayerPos(listbox1.ItemIndex, false);
  listbox1.ItemIndex := min(listbox1.ItemIndex + 1, listbox1.Items.Count - 1);
End;

Procedure TForm11.FormShow(Sender: TObject);
Begin
  If Not Form11ShowOnce Then Begin
    Form11ShowOnce := true;
    left := strtoint(GetValue('AskDifficultyForm', 'Left', inttostr(left)));
    top := strtoint(GetValue('AskDifficultyForm', 'Top', inttostr(top)));
    FixFormPosition(form11);
  End;
End;

Procedure TForm11.SelectDifficulty(index: integer);
Begin
  If ctd.Map.Lives[index] <= 0 Then Begin
    logshow('This map has no definition for the selected difficulty. Please choose other difficulty', llWarning);
    exit;
  End;
  form11Difficulty := index;
  ModalResult := mrOK;
End;

End.


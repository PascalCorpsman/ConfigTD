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
Unit unit15;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm15 }

  TForm15 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Procedure Button2Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    comparer: String;
    Direction: integer; // 0 Local -> Map, 1 Map -> Local
  End;

Var
  Form15: TForm15;

Implementation

{$R *.lfm}

Uses unit14, uctd_common;

{ TForm15 }

Procedure TForm15.FormCreate(Sender: TObject);
Begin
  caption := 'Please choose';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm15.Button2Click(Sender: TObject);
Var
  b: Boolean;
  i: Integer;
Begin
  b := LowerCase(trim(Edit1.Text)) <> comparer;
  If Direction = 0 Then Begin
    For i := 0 To Form14.ListBox2.Items.Count - 1 Do Begin
      If LowerCase(trim(Edit1.Text)) = LowerCase(trim(Form14.ListBox2.Items[i])) Then Begin
        b := true;
        break;
      End;
    End;
  End;
  If Direction = 1 Then Begin
    For i := 0 To Form14.ListBox1.Items.Count - 1 Do Begin
      If LowerCase(trim(Edit1.Text)) = LowerCase(trim(Form14.ListBox1.Items[i])) Then Begin
        b := true;
        break;
      End;
    End;
  End;
  If b Then Begin
    ModalResult := mrOK;
  End
  Else Begin
    logshow(Edit1.Text + ' is already used.', llWarning);
  End;
End;

Procedure TForm15.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button2.Click;
End;

End.


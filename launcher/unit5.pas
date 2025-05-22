(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ctd_launcher                                          *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ulauncher;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
  private
    fFiles: TFiles;

  public
    Procedure ShowData(Const files: TFiles);
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Detail viewer';
End;

Procedure TForm5.ListBox1DblClick(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    showmessage(fFiles[ListBox1.ItemIndex].URL);
  End;
End;

Procedure TForm5.ShowData(Const files: TFiles);
Var
  i: Integer;
Begin
  fFiles := Files;
  ListBox1.Clear;
  For i := 0 To high(files) Do Begin
    ListBox1.Items.Add(
      format('%s (%s)',
      [
      ExtractFileName(files[i].Filename),
        FileSizeToString(files[i].Size)
        ])
        );
  End;
  ShowModal;
End;

End.


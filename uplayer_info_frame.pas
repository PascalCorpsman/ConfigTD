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
Unit uplayer_info_frame;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

Type

  TTransferCallback = Procedure(Sender: TObject; DestPlayer, Count: integer) Of Object;

  { TPlayerInfoFrame }

  TPlayerInfoFrame = Class(TFrame)
    Button1: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure ComboBox1KeyPress(Sender: TObject; Var Key: char);
  private
    { private declarations }
  public
    { public declarations }
    CashTransferCallback: TTransferCallback;

    Constructor Create(AOwner: TComponent); override;
  End;

Implementation

{$R *.lfm}

{ TPlayerInfoFrame }

Constructor TPlayerInfoFrame.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ComboBox1.text := '100';
End;

Procedure TPlayerInfoFrame.Button1Click(Sender: TObject);
Var
  i: integer;
Begin
  // Geld Senden..
  i := strtointdef(ComboBox1.text, -1);
  If i > 0 Then Begin
    CashTransferCallback(self, tag - 1, i);
  End;
End;

Procedure TPlayerInfoFrame.ComboBox1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1Click(self);
End;

End.


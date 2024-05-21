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
Unit unit12;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uplayer_info_frame;

Type

  { TForm12 }

  TForm12 = Class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    PlayerInfoFrame1: TPlayerInfoFrame;
    ScrollBar1: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Procedure CashTransferCallback(Sender: TObject; DestPlayer, Count: integer);
  public
    { public declarations }
    Procedure RecreateFrames;
    Procedure RefreshInfos;
  End;

Var
  Form12: TForm12;

Implementation

{$R *.lfm}

Uses uctd;

{ TForm12 }

Procedure TForm12.Button1Click(Sender: TObject);
Begin
  RefreshInfos;
End;

Procedure TForm12.FormCreate(Sender: TObject);
Begin
  panel1.Caption := '';
  caption := 'Player stats';
End;

Procedure TForm12.CashTransferCallback(Sender: TObject; DestPlayer,
  Count: integer);
Begin
  ctd.TransferCash(DestPlayer, Count);
End;

Procedure TForm12.RecreateFrames;
Var
  j, i: Integer;
  f: TPlayerInfoFrame;
Begin
  j := 0;
  For i := panel1.ComponentCount - 1 Downto 0 Do Begin
    // Wenn die Spielerzahl sich nicht ändert, aber die Position des Spielers, dann muss die "Gui" mit angepasst werden.
    If Panel1.Components[i] Is TPlayerInfoFrame Then Begin
      f := Panel1.Components[i] As TPlayerInfoFrame;
      f.Button1.Visible := Not (i = ctd.PlayerIndex);
      f.ComboBox1.Visible := Not (i = ctd.PlayerIndex);
      f.Label9.Visible := Not (i = ctd.PlayerIndex);
      inc(j);
    End;
  End;
  If ctd.PlayerCount = j Then exit;
  // Die Anzahl stimmt wohl nicht, also alles einfach neu initialisieren...
  For i := panel1.ComponentCount - 1 Downto 0 Do Begin
    If Panel1.Components[i] Is TPlayerInfoFrame Then
      Panel1.Components[i].Free;
  End;
  If ctd.PlayerCount = 0 Then exit; // Nil Pointer AV, unten verhinden, auch wenn das eigentlich unmöglich ist ..
  ScrollBar1.Position := 0;
  For i := 0 To ctd.PlayerCount - 1 Do Begin
    f := TPlayerInfoFrame.Create(Panel1, i = 0);
    f.Name := 'PlayerInfoFrame' + inttostr(i + 1);
    f.Parent := Panel1;
    f.left := 8;
    f.top := (f.Height + 10) * i + 8;
    f.tag := i + 1; // zur Ausgrenzung des 0 Tags
    f.CashTransferCallback := @CashTransferCallback;
    // Der Spieler kann sich selbst kein Geld senden
    If i = ctd.PlayerIndex Then Begin
      f.Button1.Visible := false;
      f.ComboBox1.Visible := false;
      f.Label9.Visible := false;
    End;
  End;
  ScrollBar1.Max := (ctd.PlayerCount - 1) * (f.Height + 10);
  ScrollBar1.Visible := ScrollBar1.Max <> ScrollBar1.Min;
End;

Procedure TForm12.RefreshInfos;
Var
  i: integer;
  f: TPlayerInfoFrame;
  cl: TPlayerInfos;
Begin
  For i := 0 To panel1.ComponentCount - 1 Do Begin
    If panel1.Components[i] Is TPlayerInfoFrame Then Begin
      f := TPlayerInfoFrame(panel1.Components[i]);
      cl := ctd.Player[panel1.Components[i].Tag - 1];
      f.Label2.caption := cl.Name;
      f.Label4.caption := inttostr(cl.Cash);
      f.Label6.caption := inttostr(cl.Lives);
      f.Label8.caption := inttostr(cl.Kills);
    End;
  End;
End;

End.


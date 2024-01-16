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
Unit uwave_oppenent_frame;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, uctd_map;

Type

  { TWaveOpponentFrame }

  TWaveOpponentFrame = Class(TFrame)
    Button1: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SpeedButton1: TSpeedButton;
    Procedure Button1Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure Edit3Change(Sender: TObject);
    Procedure Edit4Change(Sender: TObject);
    Procedure Edit5Change(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    WaveNum: integer;
    OpponnetNum: integer;
    RefreshOppentList: TNotifyEvent;
    OpenOpponentEditor: TNotifyEvent;
    Constructor Create(TheOwner: TComponent); override;
    Procedure Reset();
    Procedure LoadOppent(Const Opp: TWaveOpponent);
    Function GetOpponent(): TWaveOpponent;
  End;

Implementation

{$R *.lfm}

Uses uctd, uctd_messages, LCLType;

{ TWaveOpponentFrame }

Procedure TWaveOpponentFrame.Button1Click(Sender: TObject);
Begin
  If assigned(RefreshOppentList) Then Begin
    RefreshOppentList(Nil);
  End;
End;

Procedure TWaveOpponentFrame.ComboBox1Change(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  m.WriteAnsiString(ComboBox1.Text);
  ctd.UpdateMapProperty(mpWaveOpponent, m);
  ctd.Map.Waves[WaveNum].Opponents[OpponnetNum].opponent := ComboBox1.Text;
End;

Procedure TWaveOpponentFrame.ComboBox1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // Read Only von Hand
  If (key <> VK_down) And (key <> VK_Up) Then key := 0;
End;

Procedure TWaveOpponentFrame.Edit1Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  i := strtointdef(edit1.text, 1);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentCount, m);
  ctd.Map.Waves[WaveNum].Opponents[OpponnetNum].Count := i;
End;

Procedure TWaveOpponentFrame.Edit2Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  i := strtointdef(edit2.text, 0);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentCashPerUnit, m);
  ctd.Map.Waves[WaveNum].Opponents[OpponnetNum].refund := i;
End;

Procedure TWaveOpponentFrame.Edit3Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  i := strtointdef(edit3.text, 1);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentUnitsPerSpawn, m);
  ctd.Map.Waves[WaveNum].Opponents[OpponnetNum].UnitsPerSpawn := i;
End;

Procedure TWaveOpponentFrame.Edit4Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  i := strtointdef(edit4.text, 1);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentSpawnDelay, m);
  ctd.Map.Waves[WaveNum].Opponents[OpponnetNum].SpawnDelay := i;
End;

Procedure TWaveOpponentFrame.Edit5Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  i := strtointdef(edit5.text, 1);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentSpawnDelta, m);
  ctd.Map.Waves[WaveNum].Opponents[OpponnetNum].Spawndelta := i;
End;

Procedure TWaveOpponentFrame.SpeedButton1Click(Sender: TObject);
Begin
  If assigned(OpenOpponentEditor) Then Begin
    OpenOpponentEditor(Nil);
  End;
End;

Constructor TWaveOpponentFrame.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  RefreshOppentList := Nil;
  OpenOpponentEditor := Nil;
  SpeedButton1.Caption := 'Open' + LineEnding + 'Opponent' + LineEnding + 'Editor';
End;

Procedure TWaveOpponentFrame.Reset();
Begin
  ComboBox1.Text := '';
  If ComboBox1.items.count <> 0 Then ComboBox1.ItemIndex := 0;
  edit1.text := '10'; // Count
  edit2.text := '1'; // Cash per unit
  edit3.text := '1'; // Units per Spawn
  edit4.text := '2500'; // Spawndelay
  edit5.text := '1000'; // Spawndelta
End;

Procedure TWaveOpponentFrame.LoadOppent(Const Opp: TWaveOpponent);
Begin
  ComboBox1.ItemIndex := -1;
  ComboBox1.Text := Opp.opponent;
  (*
   * Wenn es den Gegner noch nicht in der Liste gibt (weil diese noch nicht initialisiert ist)
   * Dann legen wir einen Initialeintrag an, dieser wird später zwar wieder überschrieben aber das ist egal.
   *)
  If ComboBox1.ItemIndex = -1 Then Begin
    ComboBox1.Items.Add(Opp.opponent);
    ComboBox1.Text := Opp.opponent;
  End;
  Edit1.text := inttostr(Opp.Count); // Count
  edit2.text := inttostr(Opp.refund); // Cash per unit
  Edit3.text := inttostr(Opp.UnitsPerSpawn); // Units per Spawn
  Edit4.text := inttostr(Opp.SpawnDelay); // Spawndelay
  Edit5.text := inttostr(Opp.Spawndelta); // Spawndelta
End;

Function TWaveOpponentFrame.GetOpponent(): TWaveOpponent;
Begin
  result.opponent := ComboBox1.Text;
  result.Count := strtointdef(edit1.text, 0);
  result.refund := strtointdef(edit2.text, 0);
  result.UnitsPerSpawn := strtointdef(edit3.text, 0);
  result.SpawnDelay := strtointdef(edit4.text, 0);
  result.Spawndelta := strtointdef(edit5.text, 0);
End;

End.


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
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, uctd_map, Types;

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
    Procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure ComboBox1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure Edit3Change(Sender: TObject);
    Procedure Edit4Change(Sender: TObject);
    Procedure Edit5Change(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure OnEditEnter(Sender: TObject);
    Procedure OnEditExit(Sender: TObject);
  private
    { private declarations }
    Procedure AddOpponentItem(opponent: String);
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

    Procedure SetOppentTo(opponent: String);
  End;

Implementation

{$R *.lfm}

Uses uctd, uctd_messages, uctd_common, LCLType;

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
End;

Procedure TWaveOpponentFrame.ComboBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
Var
  cb: TComboBox;
  obj: TItemObject;
  l: Integer;
  t: String;
Begin
  cb := TComboBox(Control);
  obj := TItemObject(cb.items.Objects[index]);
  l := 0;
  t := cb.Items[index];
  If assigned(obj) Then Begin
    l := ARect.Bottom - ARect.Top;
    cb.Canvas.StretchDraw(rect(ARect.Left, arect.Top, ARect.Left + l, ARect.Bottom), obj.Image);
    // t := obj.DisplayName + ' ' + obj.Text; // -- So sieht man die Powers nach Schadensklasse sortiert
    t := obj.DisplayName + ' ' + obj.PowerSum; // -- So sieht man die Summe aller Powers, ist nicht so breit und der Name sollte eigentlich auch sagen was sache ist ;)
  End;
  cb.Canvas.TextRect(arect,
    l,
    (ARect.Bottom + ARect.Top - cb.canvas.TextHeight('8')) Div 2
    , '  ' + t);
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
  If edit1.text = '' Then exit;
  i := strtointdef(edit1.text, 1);
  edit1.Tag := i;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentCount, m);
End;

Procedure TWaveOpponentFrame.Edit2Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  If edit2.text = '' Then exit;
  i := strtointdef(edit2.text, 1);
  edit2.Tag := i;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentCashPerUnit, m);
End;

Procedure TWaveOpponentFrame.Edit3Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  If edit3.text = '' Then exit;
  i := strtointdef(edit3.text, 1);
  edit3.Tag := i;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentUnitsPerSpawn, m);
End;

Procedure TWaveOpponentFrame.Edit4Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  If edit4.text = '' Then exit;
  i := strtointdef(edit4.text, 1);
  edit4.Tag := i;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentSpawnDelay, m);
End;

Procedure TWaveOpponentFrame.Edit5Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  If edit5.text = '' Then exit;
  i := strtointdef(edit5.text, 1);
  edit5.Tag := i;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(OpponnetNum, sizeof(OpponnetNum));
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpWaveOpponentSpawnDelta, m);
End;

Procedure TWaveOpponentFrame.SpeedButton1Click(Sender: TObject);
Begin
  If assigned(OpenOpponentEditor) Then Begin
    OpenOpponentEditor(Nil);
  End;
End;

Procedure TWaveOpponentFrame.OnEditEnter(Sender: TObject);
Var
  e: TEdit absolute Sender;
Begin
  e.Tag := strtointdef(e.Text, 0);
End;

Procedure TWaveOpponentFrame.OnEditExit(Sender: TObject);
Var
  e: TEdit absolute Sender;
Begin
  e.Text := inttostr(e.Tag);
End;

Constructor TWaveOpponentFrame.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  RefreshOppentList := Nil;
  OpenOpponentEditor := Nil;
  SpeedButton1.Caption := 'Open' + LineEnding + 'Opponent' + LineEnding + 'Editor';
End;

Procedure TWaveOpponentFrame.Reset;
Begin
  ComboBox1.Text := '';
  edit1.text := '10'; // Count
  edit2.text := '1'; // Cash per unit
  edit3.text := '1'; // Units per Spawn
  edit4.text := '2500'; // Spawndelay
  edit5.text := '1000'; // Spawndelta
End;

Procedure TWaveOpponentFrame.LoadOppent(Const Opp: TWaveOpponent);
Begin
  SetOppentTo(Opp.opponent);
  Edit1.text := inttostr(Opp.Count); // Count
  edit2.text := inttostr(Opp.refund); // Cash per unit
  Edit3.text := inttostr(Opp.UnitsPerSpawn); // Units per Spawn
  Edit4.text := inttostr(Opp.SpawnDelay); // Spawndelay
  Edit5.text := inttostr(Opp.Spawndelta); // Spawndelta
End;

Function TWaveOpponentFrame.GetOpponent: TWaveOpponent;
Begin
  result.opponent := ComboBox1.Text;
  result.Count := strtointdef(edit1.text, 0);
  result.refund := strtointdef(edit2.text, 0);
  result.UnitsPerSpawn := strtointdef(edit3.text, 0);
  result.SpawnDelay := strtointdef(edit4.text, 0);
  result.Spawndelta := strtointdef(edit5.text, 0);
End;

Procedure TWaveOpponentFrame.AddOpponentItem(opponent: String);
Var
  obj: TItemObject;
Begin
  obj := TItemObject.Create;
  obj.LoadOppInfo(MapFolder + MapName + PathDelim + opponent);
  ComboBox1.Items.AddObject(opponent, obj);
  ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
End;

Procedure TWaveOpponentFrame.SetOppentTo(opponent: String);
Var
  i: Integer;
Begin
  For i := 0 To ComboBox1.Items.Count - 1 Do Begin
    If ComboBox1.Items[i] = opponent Then Begin
      ComboBox1.ItemIndex := i;
      exit;
    End;
  End;
  AddOpponentItem(opponent);
End;

End.


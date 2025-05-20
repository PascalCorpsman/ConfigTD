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
Unit uwave_frame;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ComCtrls, ExtCtrls, uwave_oppenent_frame, uctd_map;

Type

  TChangeWaveIndex = Procedure(WaveIndex: integer; Direction: Boolean) Of Object;

  { TWaveFrame }

  TWaveFrame = Class(TFrame)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit2: TEdit;
    Edit1: TEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure Reset();
    Function GetWave(): TWave; // Nur für die Change Wave Routine
  private
    { private declarations }
    FWaveNume: integer;
    Procedure SetWaveNum(AValue: integer);
    Procedure SendSetOppenentCountTo(OpponentCount: integer);

  public
    { public declarations }
    RefreshOppentList: TNotifyEvent;
    ChangeWaveIndex: TChangeWaveIndex;
    Property WaveNum: integer read FWaveNume write SetWaveNum;
    Constructor Create(AOwner: TComponent); override;
    Procedure ResetOpponent(Const t: TTabsheet);
    Procedure UpdateOpponents(Const CommaList: String);
    Procedure LoadWave(Const Wave: TWave);
    Procedure SetOppenentCountTo(OpponentCount, SenderUid: integer);

    Procedure FixOpponentNums(); // Repariert die Opponent Frame Nummern nachdem ein Beliebiger Opp gelöscht wurde
  End;

Implementation

{$R *.lfm}

Uses uctd, uctd_messages, unit1;

{ TWaveFrame }


Procedure TWaveFrame.SendSetOppenentCountTo(OpponentCount: integer);
Var
  m: TMemoryStream;
Begin
  If Not assigned(ctd) Then exit;
  If Not ctd.BlockMapUpdateSending Then Begin
    m := TMemoryStream.Create;
    m.Write(WaveNum, sizeof(WaveNum));
    m.Write(OpponentCount, sizeof(OpponentCount));
    ctd.UpdateMapProperty(mpWaveOpponents, m);
  End;
End;

Procedure TWaveFrame.SetOppenentCountTo(OpponentCount, SenderUid: integer);
Var
  ops: integer;
  t: TTabSheet;
  f: TWaveOpponentFrame;
  opstr: String;
Begin
  ops := OpponentCount;
  (*
   * Wir hohlen uns eine Opponent Liste falls es schon eine gibt.
   *)
  opstr := '';
  If PageControl1.PageCount > 0 Then Begin
    f := PageControl1.Pages[0].Components[0] As TWaveOpponentFrame;
    opstr := f.ComboBox1.Items.CommaText;
  End;
  While ops > PageControl1.PageCount Do Begin
    t := PageControl1.AddTabSheet;
    t.caption := 'Opponent' + inttostr(PageControl1.PageCount);
    t.name := 'optabs' + inttostr(PageControl1.PageCount);
    f := TWaveOpponentFrame.Create(t);
    f.OpenOpponentEditor := @Form1.MenuItem16Click;
    f.name := 'WaveOpponentFrame' + inttostr(PageControl1.PageCount);
    f.wavenum := WaveNum;
    f.OpponnetNum := PageControl1.PageCount - 1;
    f.Parent := t;
    f.RefreshOppentList := RefreshOppentList;
    If opstr <> '' Then f.ComboBox1.Items.CommaText := opstr; // Schon mal vor laden der Opponents
    ResetOpponent(t);
  End;
  While ops < PageControl1.PageCount Do
    PageControl1.Pages[PageControl1.PageCount - 1].free;
  If ctd.OwnServerUid = SenderUid Then Begin
    PageControl1.ActivePageIndex := PageControl1.PageCount - 1;
  End;
End;

Procedure TWaveFrame.FixOpponentNums;
Var
  i: integer;
Begin
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    TWaveOpponentFrame(PageControl1.Pages[i].Components[0]).OpponnetNum := i;
    TWaveOpponentFrame(PageControl1.Pages[i].Components[0]).Name := 'WaveOpponentFrame' + inttostr(i);
    PageControl1.Pages[i].Name := 'optabs' + inttostr(i);
    PageControl1.Pages[i].Caption := 'Opponent' + inttostr(i + 1);
  End;
End;

Procedure TWaveFrame.Button1Click(Sender: TObject);
Begin
  // Add one Oppenent
  SendSetOppenentCountTo(length(ctd.Map.Waves[WaveNum].Opponents) + 1);
  PageControl1.ActivePageIndex := PageControl1.PageCount - 1;
End;

Procedure TWaveFrame.Button2Click(Sender: TObject);
Begin
  If assigned(ChangeWaveIndex) Then
    ChangeWaveIndex(FWaveNume, false);
End;

Procedure TWaveFrame.Button3Click(Sender: TObject);
Begin
  If assigned(ChangeWaveIndex) Then
    ChangeWaveIndex(FWaveNume, true);
End;

Procedure TWaveFrame.Button4Click(Sender: TObject);
Var
  m: TMemoryStream;
  h: integer;
Begin
  // Del Opponent
  If PageControl1.PageCount > 1 Then Begin // Den Letzten gegner können wir nicht Löschen !
    m := TMemoryStream.Create;
    m.Write(WaveNum, sizeof(WaveNum));
    h := PageControl1.ActivePageIndex;
    m.Write(h, sizeof(h));
    ctd.UpdateMapProperty(mpDelOppInWave, m);
  End;
End;

Procedure TWaveFrame.Edit1Change(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.WriteAnsiString(edit1.text);
  ctd.UpdateMapProperty(mpWaveHint, m);
End;

Procedure TWaveFrame.Edit2Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: Integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  i := StrToIntDef(Edit2.Text, 0);
  m := TMemoryStream.Create;
  m.Write(WaveNum, sizeof(WaveNum));
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpCashOnWaveStart, m);
End;

Procedure TWaveFrame.PageControl1Change(Sender: TObject);
Begin
  // Deaktiviert, killt unter Windows alles
  // GGF machen wir das nur noch rein, wenn der Opponent Editor ausgeführt wurde mit änderungen
  //    dann aber bei allen !
  // If assigned(RefreshOppentList) Then RefreshOppentList(Nil);
End;

Procedure TWaveFrame.SetWaveNum(AValue: integer);
Begin
  FWaveNume := AValue;
  button2.Visible := AValue > 0;
  label1.Visible := button2.Visible Or button3.Visible;
End;

Procedure TWaveFrame.Reset;
Begin
  SetOppenentCountTo(1, ctd.OwnServerUid);
  Edit2.text := '0'; // Cash on Start
  Edit1.text := ''; // Wave hint
  // ResetOpponent(PageControl1.Pages[0]); -- Es geht auch ohne, wenn es auch Merkwrüdig ist ..
End;

Constructor TWaveFrame.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
End;

Procedure TWaveFrame.ResetOpponent(Const t: TTabsheet);
Var
  f: TWaveOpponentFrame;
Begin
  f := t.Components[0] As TWaveOpponentFrame;
  f.reset;
  //  PageControl1.Height := f.Height; -- Da müsste die "Höhe" der Tabs noch mit rein
  //  PageControl1.Width := f.Width;
End;

Procedure TWaveFrame.UpdateOpponents(Const CommaList: String);
Var
  j, i: integer;
  f: TWaveOpponentFrame;
  s: String;
  sl, sl2: TStringList;
  b: Boolean;
Begin
  // Alphabetisch sortieren der Opps
  sl := TStringList.Create;
  sl.CommaText := CommaList;
  sl2 := TStringList.Create;
  sl2.Sorted := True;
  For j := 0 To sl.Count - 1 Do Begin
    sl2.Add(sl[j]);
  End;
  sl.free;
  // Einfügen in die Componenten :)
  For j := 0 To PageControl1.PageCount - 1 Do Begin
    f := PageControl1.Pages[j].Components[0] As TWaveOpponentFrame;
    s := f.ComboBox1.Text;
    f.ComboBox1.Items.BeginUpdate;
    f.ComboBox1.items.Clear;
    For i := 0 To sl2.Count - 1 Do Begin
      f.AddOpponentItem(sl2[i]);
    End;
    f.ComboBox1.Items.EndUpdate;
    f.ComboBox1.ItemIndex := -1;
    // Wieder Zurücksetzen des alten Textes, falls es diesen noch gibt...
    For i := 0 To f.ComboBox1.Items.Count - 1 Do Begin
      If f.ComboBox1.Items[i] = s Then Begin
        f.ComboBox1.Text := s;
        break;
      End;
    End;
    // Init mit dem 1. Element, falls dieses vorher undefiniert war !
    If (s = '') And (f.ComboBox1.Items.Count > 0) Then Begin
      f.ComboBox1.Text := f.ComboBox1.Items[0];
      (*
       * Default initialisiert der Server das nicht -> hier müssen die Clients explizit informiert werden !
       *)
      b := ctd.BlockMapUpdateSending;
      ctd.BlockMapUpdateSending := false;
      f.ComboBox1Change(f.ComboBox1);
      ctd.BlockMapUpdateSending := b;
    End;
  End;
  sl2.free;
End;

Procedure TWaveFrame.LoadWave(Const Wave: TWave);
Var
  j: Integer;
  f: TWaveOpponentFrame;
Begin
  Edit2.text := inttostr(Wave.ChashOnStart); // Cash on Start
  SetOppenentCountTo(high(Wave.Opponents) + 1, ctd.OwnServerUid); // Opponents
  Edit1.text := Wave.WaveHint; // Wave Hint
  For j := 0 To high(Wave.Opponents) Do Begin
    f := PageControl1.Pages[j].Components[0] As TWaveOpponentFrame;
    f.LoadOppent(wave.Opponents[j]);
  End;
End;

Function TWaveFrame.GetWave: TWave;
Var
  i: Integer;
Begin
  result.ChashOnStart := StrToIntDef(Edit2.Text, 0);
  result.WaveHint := Edit1.Text;
  setlength(result.Opponents, PageControl1.PageCount);
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    result.Opponents[i] := (PageControl1.Pages[i].Components[0] As TWaveOpponentFrame).GetOpponent();
  End;
End;

End.


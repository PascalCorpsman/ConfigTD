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
Unit unit10;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, uctd;

Type

  { TForm10 }

  TForm10 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormHide(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1KeyPress(Sender: TObject; Var Key: char);
  private
    { private declarations }
    Updateing: integer;
    PreviewIndex: integer;


    Procedure CreatePreview;
    Procedure OnGetMapPrieviewInfoEvent(Sender: TObject; Const Data: TMapPrieviewInfo);
    Function ReadName(Const value: String): String;
    Function ReadPlayer(Const Value: String): integer;
    Function ReadRating(Const Value: String): Single;
    Function ReadType(Const Value: String): integer;
    Function ReadWaveCount(Const Value: String): integer;
  public
    { public declarations }
    Procedure ReloadLevels;
    Procedure BeginUpdate;
    Procedure EndUpdate;
  End;

Var
  Form10: TForm10;
  Form10MapList: TStringlist;
  form10Filename: String;

Implementation

{$R *.lfm}

Uses
  math, unit1, uctd_common;

Var
  form10ShowOnce: Boolean = false;

  { TForm10 }

Procedure TForm10.FormCreate(Sender: TObject);
Begin
  Updateing := 0;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  caption := 'Load a map...';
  Memo1.Clear;
End;

Procedure TForm10.FormHide(Sender: TObject);
Begin
  If assigned(ctd.Map) Then Begin
    form1.RestoreForm4;
  End;
End;

Procedure TForm10.CheckBox1Change(Sender: TObject);
Begin
  ReloadLevels();
End;

Procedure TForm10.Button1Click(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    form10Filename := ListBox1.Items[ListBox1.ItemIndex];
    setvalue('Global', 'minMapPlayer', form10.Edit1.Text);
    setvalue('Global', 'maxMapPlayer', form10.Edit2.Text);
    setvalue('Global', 'MapsSingle', inttostr(ord(form10.CheckBox1.Checked)));
    setvalue('Global', 'MapsCoop', inttostr(ord(form10.CheckBox2.Checked)));
    setvalue('Global', 'MapsSingleMaze', inttostr(ord(form10.CheckBox3.Checked)));
    setvalue('Global', 'MapsCoopMaze', inttostr(ord(form10.CheckBox4.Checked)));
    setvalue('Global', 'OnlyPlayable', inttostr(ord(form10.CheckBox5.Checked)));
    // Karte laden
    ctd.LoadMap(form10Filename);
    close;
  End;
End;

Procedure TForm10.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm10.Button3Click(Sender: TObject);
Begin
  CheckBox1.Checked := true;
  CheckBox2.Checked := true;
  CheckBox3.Checked := true;
  CheckBox4.Checked := true;
  CheckBox5.Checked := true; // Das ist Prinzipiel heikel, da es ja default mäßig Karten ausblendet, unter der Annahme dass der Großteil der Spieler aber zocken und nicht Designen will, ists ok
  edit1.text := '1';
  edit2.text := '99';
  TrackBar1.Position := 0;
  TrackBar2.Position := 5;
  ReloadLevels();
End;

Procedure TForm10.Button4Click(Sender: TObject);
Begin
  // Laden und gleich Spielen
  If ListBox1.ItemIndex <> -1 Then Begin
    form10Filename := ListBox1.Items[ListBox1.ItemIndex];
    setvalue('Global', 'minMapPlayer', form10.Edit1.Text);
    setvalue('Global', 'maxMapPlayer', form10.Edit2.Text);
    setvalue('Global', 'MapsSingle', inttostr(ord(form10.CheckBox1.Checked)));
    setvalue('Global', 'MapsCoop', inttostr(ord(form10.CheckBox2.Checked)));
    setvalue('Global', 'MapsSingleMaze', inttostr(ord(form10.CheckBox3.Checked)));
    setvalue('Global', 'MapsCoopMaze', inttostr(ord(form10.CheckBox4.Checked)));
    setvalue('Global', 'OnlyPlayable', inttostr(ord(form10.CheckBox5.Checked)));
    // Karte laden
    ctd.LoadMap(form10Filename);
    // Der "Hack" zum automatischen Starten des Spiels
    form1.MenuItem18.Enabled := false; // Abschalten Start / Restart Button (der wird wenn fertig geladen wurde wieder Aktiviert)
    form1.Timer2.Enabled := true; // Der Timer, der dann das Spiel startet.
    close;
  End;
End;

Procedure TForm10.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  SetValue('LoadMapForm', 'Left', inttostr(left));
  SetValue('LoadMapForm', 'Top', inttostr(top));
  SetValue('LoadMapForm', 'Width', inttostr(Width));
  SetValue('LoadMapForm', 'Height', inttostr(Height));
  Form10MapList.free;
  Form10MapList := Nil;
End;

Procedure TForm10.FormShow(Sender: TObject);
Begin
  If Not Form10ShowOnce Then Begin
    Form10ShowOnce := true;
    left := strtoint(GetValue('LoadMapForm', 'Left', inttostr(left)));
    top := strtoint(GetValue('LoadMapForm', 'Top', inttostr(top)));
    Width := strtoint(GetValue('LoadMapForm', 'Width', inttostr(Width)));
    Height := strtoint(GetValue('LoadMapForm', 'Height', inttostr(Height)));
    FixFormPosition(form10);
  End;
  ListBox1.SetFocus;
End;

Procedure TForm10.ListBox1Click(Sender: TObject);
Begin
  If PreviewIndex <> ListBox1.ItemIndex Then Begin
    CreatePreview;
  End;
End;

Procedure TForm10.ListBox1DblClick(Sender: TObject);
Begin
  Button4.Click;
End;

Procedure TForm10.ListBox1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    Button4.Click;
  End;
End;

(*
 *  Der String ist wie folgt aufgebaut (wird in TServer.HandleRequestMapList bzw. TServer.ExtractMapAttibutes erzeugt)
 *
 *  <MapName>:<Maptype> :<MaxPlayer>:<NumWaves>:<Rating>
 *  <String> :<Bitfield>:<integer>  :<integer> :<integer>
 *)

Function TForm10.ReadRating(Const Value: String): Single;
Var
  a: TStringArray;
Begin
  a := Value.Split(':');
  result := strtointdef(a[4], 0);
End;

Function TForm10.ReadType(Const Value: String): integer;
Var
  a: TStringArray;
Begin
  a := Value.Split(':');
  result := strtointdef(a[1], 0);
End;

Function TForm10.ReadWaveCount(Const Value: String): integer;
Var
  a: TStringArray;
Begin
  a := Value.Split(':');
  result := strtointdef(a[3], 0);
End;

Function TForm10.ReadPlayer(Const Value: String): integer;
Var
  a: TStringArray;
Begin
  a := Value.Split(':');
  result := strtointdef(a[2], 0);
End;

Function TForm10.ReadName(Const value: String): String;
Begin
  result := copy(value, 1, pos(':', value) - 1);
End;

Procedure TForm10.ReloadLevels;
Var
  i: Integer;
  w, maxp, pp, minp, j, EnterID: Integer;
  f: Single;
Begin
  EnterID := LogEnter('TForm10.ReloadLevels');
  If Updateing <> 0 Then Begin
    logleave(EnterID);
    exit;
  End;
  listbox1.Items.BeginUpdate;
  listbox1.Clear;
  listbox1.Sorted := true;
  minp := max(1, strtointdef(Edit1.Text, 1));
  maxp := min(99, strtointdef(Edit2.Text, 99));
  For i := 0 To Form10MapList.count - 1 Do Begin
    j := ReadType(Form10MapList[i]);
    pp := ReadPlayer(Form10MapList[i]);
    f := ReadRating(Form10MapList[i]);
    w := ReadWaveCount(Form10MapList[i]);
    If (j <> -1) And (minp <= pp) And (maxp >= pp) And (f >= TrackBar1.Position) And (f <= TrackBar2.Position)
      And ((Not CheckBox5.Checked) Or (CheckBox5.Checked And (w > 0))) Then Begin
      Case j Of
        0: If CheckBox1.Checked Then ListBox1.Items.Add(ReadName(Form10MapList[i]));
        1: If CheckBox2.Checked Then ListBox1.Items.Add(ReadName(Form10MapList[i]));
        2: If CheckBox3.Checked Then ListBox1.Items.Add(ReadName(Form10MapList[i]));
        3: If CheckBox4.Checked Then ListBox1.Items.Add(ReadName(Form10MapList[i]));
      End;
    End;
  End;
  listbox1.Items.EndUpdate;
  Label4.caption := format('(%d)', [ListBox1.Items.Count]);
  If ListBox1.Items.Count <> 0 Then Begin
    ListBox1.ItemIndex := 0;
    CreatePreview;
  End
  Else Begin
    PreviewIndex := -1;
    memo1.text := '';
    Image1.Picture.Assign(Nil);
  End;
  logleave(EnterID);
End;

Procedure TForm10.BeginUpdate;
Begin
  inc(Updateing);
End;

Procedure TForm10.EndUpdate;
Begin
  Updateing := max(0, Updateing - 1);
  If Updateing = 0 Then Begin
    ReloadLevels;
  End;
End;

Procedure TForm10.CreatePreview;
Var
  f: Single;
  i: Integer;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    f := 0;
    For i := 0 To Form10MapList.count - 1 Do Begin
      If ReadName(Form10MapList[i]) = ListBox1.Items[ListBox1.ItemIndex] Then Begin
        f := ReadRating(Form10MapList[i]);
        break;
      End;
    End;
    If f = 0 Then Begin
      Label8.caption := 'No rating';
    End
    Else Begin
      label8.caption := format('%0.2f', [f]);
    End;
    PreviewIndex := ListBox1.ItemIndex;
    ctd.getMapPrieviewInfo(ListBox1.Items[ListBox1.ItemIndex], Not (GetValue('Global', 'DisableBackGroundTexturing', '0') = '1'), @OnGetMapPrieviewInfoEvent);
  End;
End;

Procedure TForm10.OnGetMapPrieviewInfoEvent(Sender: TObject;
  Const Data: TMapPrieviewInfo);
Begin
  memo1.text := Data.Description;
  Image1.Picture.Assign(Data.Image);
End;

End.


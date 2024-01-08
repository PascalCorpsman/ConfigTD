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
Unit Unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Grids;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Panel1Resize(Sender: TObject);
    Procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
  private

  public
    Function LoadHighscoresFromStream(Const Data: TStream): Boolean;
  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

Uses uctd_map, uctd, uctd_common, DateUtils, math, LCLType;

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  caption := 'Map Highscores';
  Panel1.Caption := '';
  panel1.Align := alBottom;
  PageControl1.Align := alClient;
End;

Procedure TForm9.Panel1Resize(Sender: TObject);
Begin
  Image1.Left := (Panel1.Width - 61) Div 2;
  image1.top := (panel1.Height - 13) Div 2;
End;

Function Clean(Value, Allowed: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 1 To length(value) Do Begin
    If pos(value[i], Allowed) <> 0 Then Begin
      result := result + value[i];
    End;
  End;
End;

(*
 * Umkehrfunktion zu prettyTime
 * !! Achtung !!
 * Dadurch dass pretty Time die Zeit "Kürzt" kommt da nicht das selbe raus
 * Bezogen auf die Pretty Time Darstellung dann aber schon wieder
 *  1111ms -> 1s -> 1000ms -> 1s
 *)

Function UnPrettyTime(FormatedTime: String): Int64;
Var
  SubScale, Scale: int64;
  p, s: String;
Begin
  result := 0;
  scale := 1;
  SubScale := 1;
  If pos('s', FormatedTime) <> 0 Then Begin
    Scale := 1000;
    SubScale := 1;
  End;
  If pos('min', FormatedTime) <> 0 Then Begin
    Scale := 1000 * 60;
    SubScale := 1000;
  End;
  If pos('h', FormatedTime) <> 0 Then Begin
    Scale := 1000 * 60 * 60;
    SubScale := 1000 * 60;
  End;
  If pos('d', FormatedTime) <> 0 Then Begin
    Scale := 1000 * 60 * 60 * 24;
    SubScale := 1000 * 60 * 60;
  End;
  If pos(':', FormatedTime) <> 0 Then Begin
    FormatedTime := Clean(FormatedTime, '0123456789:');
    p := copy(FormatedTime, 1, pos(':', FormatedTime) - 1);
    s := copy(FormatedTime, pos(':', FormatedTime) + 1, length(FormatedTime));
    result := strtoint(p) * Scale + strtoint(s) * SubScale;
  End
  Else Begin
    FormatedTime := Clean(FormatedTime, '0123456789');
    result := strtoint(FormatedTime) * Scale;
  End;
End;

Procedure TForm9.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);

  Function CompValue(V1, V2: String; Dir: Boolean): Boolean;
  Begin
    result := false;
    Case index Of
      0: result := CompareStr(v1, v2) > 0; // Name
      1: result := strtoint(Clean(v1, '0123456789')) > strtoint(Clean(v2, '0123456789')); // Points
      2: result := UnPrettyTime(v1) > UnPrettyTime(v2); // Playingtime
      3: result := strtoint(v1) > strtoint(v2); // Rounds
      4: result := CompareStr(v1, v2) > 0; // Difficulty
      5: result := strtoint(v1) > strtoint(v2); // Lives Lost
      6: Begin
          Try
            result := ScanDateTime('YYYY.MM.DD, HH:NN', v1) > ScanDateTime('YYYY.MM.DD, HH:NN', v2); // TimeStamp
          Except
            result := false;
          End;
        End;
    End;
    If dir Then result := Not result;
  End;

Var
  sg: TStringGrid;
  dir: Boolean;
  i, j, k: Integer;
  t: String;
Begin
  sg := sender As TStringGrid;
  dir := pos('\/', sg.Cells[index, 0]) <> 0;
  sg.Cells[0, 0] := 'Name';
  sg.Cells[1, 0] := 'Points';
  sg.Cells[2, 0] := 'Playing time';
  sg.Cells[3, 0] := 'Waves';
  sg.Cells[4, 0] := 'Difficulty';
  sg.Cells[5, 0] := 'Lives lost';
  sg.Cells[6, 0] := 'Timestamp';
  If dir Then Begin
    sg.Cells[index, 0] := sg.Cells[index, 0] + '/\';
  End
  Else Begin
    sg.Cells[index, 0] := sg.Cells[index, 0] + '\/';
  End;
  // Sortieren mittels Bubble Sort, das ist Langsam, aber dafür Ordnungserhaltend !

  // Sortieren
  For i := sg.RowCount - 1 Downto 2 Do Begin
    For j := 2 To i Do Begin
      If CompValue(sg.Cells[index, j], sg.Cells[index, j - 1], dir) Then Begin
        For k := 0 To sg.ColCount - 1 Do Begin
          t := sg.cells[k, j - 1];
          sg.cells[k, j - 1] := sg.cells[k, j];
          sg.cells[k, j] := t;
        End;
      End;
    End;
  End;

  sg.AutoSizeColumns;
End;

Procedure TForm9.Button2Click(Sender: TObject);
Begin
  If id_yes = application.MessageBox('You are about to clear the highscore''s and ratings.' + LineEnding + 'Delete Highscore and ratings?', 'Question', mb_iconQuestion Or MB_YESNO) Then Begin
    ctd.RequestClearHighscore;
    modalresult := mrOK;
  End;
End;

Function TForm9.LoadHighscoresFromStream(Const Data: TStream): Boolean;
Var
  h: TIniHighscoreEngine;
  i, j: Integer;
  t: TTabSheet;
  ph: TPlayerHighscore;
  sg: TStringGrid;
  rating: Single;
  r, RatingCount: integer;
Begin
  h := TIniHighscoreEngine.Create();
  h.LoadFromStream(data);
  RatingCount := 0;
  rating := 0;
  data.Read(rating, SizeOf(rating)); // 0, 1..5
  data.Read(RatingCount, SizeOf(RatingCount));
  r := round(rating * 2); // 0, 4 .. 20
  r := min(9, max(0, r));
  image1.ShowHint := true;
  If r = 0 Then Begin
    image1.Hint := 'Map not yet rated.';
  End
  Else Begin
    image1.Hint := format('Map rating is: %0.2f' + LineEnding + '%d rates', [rating, RatingCount]);
  End;
  Image1.Picture.LoadFromFile('textures' + PathDelim + 'stars' + inttostr(r) + '.bmp');
  // 1. Alles Löschen
  For i := PageControl1.PageCount - 1 Downto 0 Do Begin
    PageControl1.Page[i].Free;
  End;
  For i := 0 To h.PlayerCatCount - 1 Do Begin
    t := PageControl1.AddTabSheet;
    ph := h.PlayerCat(i);
    t.Caption := inttostr(ph.PlayerCount) + ' Player';
    sg := TStringGrid.Create(t);
    sg.Name := 'Stringgrid' + inttostr(i);
    sg.Parent := t;
    sg.OnHeaderClick := @StringGrid1HeaderClick;
    sg.Align := alClient;
    sg.FixedRows := 1;
    sg.FixedCols := 0;
    sg.RowCount := length(ph.DataSets) + 1;
    sg.ColCount := 7;
    sg.Cells[0, 0] := 'Name';
    sg.Cells[1, 0] := 'Points';
    sg.Cells[2, 0] := 'Playing time';
    sg.Cells[3, 0] := 'Waves';
    sg.Cells[4, 0] := 'Difficulty';
    sg.Cells[5, 0] := 'Lives lost';
    sg.Cells[6, 0] := 'Timestamp';

    For j := 0 To high(ph.DataSets) Do Begin
      sg.Cells[0, 1 + j] := ph.DataSets[j].Name;
      sg.Cells[1, 1 + j] := format('%.0n', [ph.DataSets[j].Points + 0.0]);
      sg.Cells[2, 1 + j] := prettyTime(ph.DataSets[j].PlayTimeinS * 1000);
      sg.Cells[3, 1 + j] := inttostr(ph.DataSets[j].Rounds);
      Case ph.DataSets[j].Difficulty Of
        0: sg.Cells[4, 1 + j] := 'Easy';
        1: sg.Cells[4, 1 + j] := 'Normal';
        2: sg.Cells[4, 1 + j] := 'Hard';
      End;
      sg.Cells[5, 1 + j] := inttostr(ph.DataSets[j].LivesLost);
      sg.Cells[6, 1 + j] := FormatDateTime('YYYY.MM.DD, HH:NN', ph.DataSets[j].TimeStamp);
    End;
    // Aufsteigend Sortieren nach Punkten
    StringGrid1HeaderClick(sg, true, 1);

  End;
  If h.PlayerCatCount = 0 Then Begin
    result := false;
    showmessage('No highscores for map found.');
  End
  Else Begin
    result := true;
  End;
  h.free;
End;

End.


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
Unit unit8;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, TAGraph, TASeries, TATools, TAChartAxis, TAIntervalSources,
  TATransformations, Types;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations3: TChartAxisTransformations;
    ChartAxisTransformations3AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartToolset1: TChartToolset;
    ChartToolset1AxisClickTool1: TAxisClickTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure ChartToolset1AxisClickTool1Click(ASender: TChartTool;
      Axis: TChartAxis; AHitInfo: TChartAxisHitTests);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    fBoldCells: Array Of Array Of Boolean;

  public
    Procedure LoadStatistics(PlayerIndex, PlayerCount: integer; Msg: String; Const Stream: TStream);
  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

Uses uctd_common
  , uctd
  , unit4
  , unit1
  ;

Const
  DateTime10S: Double = 10 / (24 * 3600); // 1 = 1 Tag, 1 / (24 * 3600) = 1s, 10 / (24 * 3600) = 10s

  { TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'Game Statistics';
End;

Procedure TForm8.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
Var
  x, y: integer;
Begin
  If (aCol = 0) And (aRow In [12..15]) Then Begin
    x := aRect.Left + StringGrid1.Canvas.TextWidth(StringGrid1.Cells[aCol, aRow]) + 10;
    y := (aRect.Top + aRect.Bottom - Form4.Image1.Height) Div 2;
    Case aRow Of
      12: StringGrid1.Canvas.Draw(x, y, form4.Image1.Picture.Bitmap);
      13: StringGrid1.Canvas.Draw(x, y, form4.Image2.Picture.Bitmap);
      14: StringGrid1.Canvas.Draw(x, y, form4.Image3.Picture.Bitmap);
      15: StringGrid1.Canvas.Draw(x, y, form4.Image4.Picture.Bitmap);
    End;
  End;
End;

Procedure TForm8.StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Begin
  If fBoldCells[aCol, aRow] Then Begin
    StringGrid1.Canvas.Font.Color := clGreen;
    StringGrid1.Canvas.Font.Style := [fsBold];
  End
  Else Begin
    StringGrid1.Canvas.Font.Style := [];
    StringGrid1.Canvas.Font.Color := clDefault;
  End;
End;

Procedure TForm8.Button1Click(Sender: TObject);
Begin
  ctd.SendMapRating(RadioGroup1.ItemIndex);
  close;
End;

Procedure TForm8.ChartToolset1AxisClickTool1Click(ASender: TChartTool;
  Axis: TChartAxis; AHitInfo: TChartAxisHitTests);
Begin
  If (axis = Chart1.AxisList[1]) And Not (ahtLabels In AHitInfo) Then Begin
    Chart1.AxisList[2].Grid.Visible := Not Chart1.AxisList[2].Grid.Visible;
  End
  Else Begin
    Axis.Grid.Visible := Not Axis.Grid.Visible;
  End;
End;

Procedure TForm8.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  (*
   * So ganz Ideal ist das leider noch nicht
   *)
  Form1.RestoreForm4;
  If Not form4.visible Then Begin
    form4.show;
  End;
End;

Procedure TForm8.LoadStatistics(PlayerIndex, PlayerCount: integer; Msg: String;
  Const Stream: TStream);
  Function CleanCell(Value: String): String;
  Var
    i: Integer;
  Begin
    result := '';
    For i := 1 To length(value) Do Begin
      If value[i] In ['0'..'9'] Then Begin
        result := result + value[i];
      End;
    End;
  End;

Var
  lt: Double;
  cnt, i, k, j, w, d: Integer;
  indexv: uint64;
  OverallPausingtime, OverAllGameingTime, OverAllRealGameingTime: int64;
  DamageDealt: Array[0..3] Of int64;
  i64: Int64;
  s: String;
Begin
  label1.caption := Msg;
  StringGrid1.ColCount := PlayerCount + 1;
  StringGrid1.RowCount := 18;
  StringGrid1.Cells[0, 0] := '';

  StringGrid1.Cells[0, 1] := 'Lives';
  StringGrid1.Cells[0, 2] := 'Lives lost';
  StringGrid1.Cells[0, 3] := 'Kills';

  StringGrid1.Cells[0, 4] := 'Money earned';
  StringGrid1.Cells[0, 5] := 'Money spent';
  StringGrid1.Cells[0, 6] := 'Money received';
  StringGrid1.Cells[0, 7] := 'Donated money';

  StringGrid1.Cells[0, 8] := 'Buildings built';
  StringGrid1.Cells[0, 9] := 'Buildings sold';
  StringGrid1.Cells[0, 10] := 'Buildings leveled up';
  StringGrid1.Cells[0, 11] := 'Favorite Building';

  StringGrid1.Cells[0, 12] := 'Damage';
  StringGrid1.Cells[0, 13] := 'Damage';
  StringGrid1.Cells[0, 14] := 'Damage';
  StringGrid1.Cells[0, 15] := 'Damage';
  StringGrid1.Cells[0, 16] := 'Damage Overall';

  StringGrid1.Cells[0, 17] := 'Score';


  // Multiple TaChart Y-Axis: https://www.lazarusforum.de/viewtopic.php?t=9917
  Chart1LineSeries1.Title := 'Player actions';
  Chart1LineSeries1.SeriesColor := clGray;
  Chart1LineSeries1.LinePen.Width := 2;
  Chart1LineSeries1.Clear;
  Chart1LineSeries1.AxisIndexY := 0;
  Chart1.AxisList[0].Marks.LabelFont.Color := Chart1LineSeries1.SeriesColor;
  Chart1.AxisList[0].Grid.Visible := false;

  Chart1LineSeries2.Title := 'Kills';
  Chart1LineSeries2.SeriesColor := clred;
  Chart1LineSeries2.LinePen.Width := 2;
  Chart1LineSeries2.Clear;
  Chart1LineSeries2.AxisIndexY := 2;
  Chart1.AxisList[2].Marks.LabelFont.Color := Chart1LineSeries2.SeriesColor;
  Chart1.AxisList[2].Grid.Visible := false;

  Chart1LineSeries3.Title := 'Money earned';
  Chart1LineSeries3.SeriesColor := cllime;
  Chart1LineSeries3.Clear;
  Chart1LineSeries3.LinePen.Width := 2;
  Chart1LineSeries3.AxisIndexY := 3;
  Chart1.AxisList[3].Marks.LabelFont.Color := Chart1LineSeries3.SeriesColor;
  Chart1.AxisList[3].Grid.Visible := false;

  Chart1LineSeries4.Title := 'Money spent';
  Chart1LineSeries4.SeriesColor := clmaroon;
  Chart1LineSeries4.LinePen.Width := 2;
  Chart1LineSeries4.Clear;
  Chart1LineSeries4.AxisIndexY := 3;
  Chart1.AxisList[3].Marks.LabelFont.Color := Chart1LineSeries4.SeriesColor;
  Chart1.AxisList[3].Grid.Visible := false;


  OverAllGameingTime := 0;
  stream.Read(OverAllGameingTime, sizeof(OverAllGameingTime));
  OverAllRealGameingTime := 0;
  stream.Read(OverAllRealGameingTime, sizeof(OverAllRealGameingTime));
  OverallPausingtime := 0;
  stream.Read(OverallPausingtime, sizeof(OverallPausingtime));
  w := 0;
  stream.Read(w, sizeof(w));
  d := 0;
  stream.Read(d, sizeof(d));
  s := '';
  Case d Of
    0: s := 'Easy ';
    1: s := 'Normal ';
    2: s := 'Hard ';
  End;
  label2.Caption := 'Real time played: ' + prettyTime(OverAllRealGameingTime) + LineEnding +
    'simulated time: ' + prettyTime(OverAllGameingTime) + LineEnding +
    'Time paused: ' + prettyTime(OverallPausingtime) + LineEnding +
    s + 'Waves: ' + inttostr(w);
  For i := 0 To PlayerCount - 1 Do Begin
    StringGrid1.Cells[1 + i, 0] := Stream.ReadAnsiString;
    For j := 1 To 10 Do Begin
      k := 0;
      Stream.Read(k, sizeof(k));
      StringGrid1.Cells[1 + i, j] := format('%.0n', [k + 0.0]);
    End;
    StringGrid1.Cells[1 + i, 11] := stream.ReadAnsiString;
    DamageDealt[0] := 0;
    DamageDealt[1] := 0;
    DamageDealt[2] := 0;
    DamageDealt[3] := 0;
    Stream.Read(DamageDealt, sizeof(DamageDealt));
    StringGrid1.Cells[1 + i, 12] := format('%.0n', [DamageDealt[0] + 0.0]);
    StringGrid1.Cells[1 + i, 13] := format('%.0n', [DamageDealt[1] + 0.0]);
    StringGrid1.Cells[1 + i, 14] := format('%.0n', [DamageDealt[2] + 0.0]);
    StringGrid1.Cells[1 + i, 15] := format('%.0n', [DamageDealt[3] + 0.0]);
    StringGrid1.Cells[1 + i, 16] := format('%.0n', [DamageDealt[0] + DamageDealt[1] + DamageDealt[2] + DamageDealt[3] + 0.0]);
    i64 := 0;
    Stream.Read(i64, sizeof(i64));
    StringGrid1.Cells[1 + i, 17] := format('%.0n', [i64 + 0.0]);
    // Auslesen Akctions
    cnt := 0;
    Stream.Read(cnt, sizeof(cnt));
    // Konfigure x-Achsis for showing Timestamps in a "nice" Manner
    // Source: https://www.lazarusforum.de/viewtopic.php?t=5516
    If cnt >= 360 Then Begin
      DateTimeIntervalChartSource1.DateTimeFormat := 'HH:NN:SS';
    End
    Else Begin
      DateTimeIntervalChartSource1.DateTimeFormat := 'NN:SS';
    End;
    // Berechnen des "Letzten" Zeitstempels
    lt := (OverAllRealGameingTime / 1000);
    lt := (lt / 10) * DateTime10S;

    For j := 0 To cnt - 1 Do Begin
      d := 0;
      Stream.Read(d, sizeof(d));
      // Wir zeigen nur die Statistik des eigenen Spielers an, auch wenn wir alle haben ;)
      If i = PlayerIndex Then Begin
        If j = cnt - 1 Then Begin
          Chart1LineSeries1.AddXY(lt, d);
        End
        Else Begin
          Chart1LineSeries1.AddXY(j * DateTime10S, d);
        End;
      End;
    End;
    // Auslesen Kills
    cnt := 0;
    Stream.Read(cnt, sizeof(cnt));
    For j := 0 To cnt - 1 Do Begin
      d := 0;
      Stream.Read(d, sizeof(d));
      // Wir zeigen nur die Statistik des eigenen Spielers an, auch wenn wir alle haben ;)
      If i = PlayerIndex Then Begin
        If j = cnt - 1 Then Begin
          Chart1LineSeries2.AddXY(lt, d);
        End
        Else Begin
          Chart1LineSeries2.AddXY(j * DateTime10S, d);
        End;
      End;
    End;
    // Auslesen Money Earned
    cnt := 0;
    Stream.Read(cnt, sizeof(cnt));
    For j := 0 To cnt - 1 Do Begin
      d := 0;
      Stream.Read(d, sizeof(d));
      // Wir zeigen nur die Statistik des eigenen Spielers an, auch wenn wir alle haben ;)
      If i = PlayerIndex Then Begin
        If j = cnt - 1 Then Begin
          Chart1LineSeries3.AddXY(lt, d);
        End
        Else Begin
          Chart1LineSeries3.AddXY(j * DateTime10S, d);
        End;
      End;
    End;
    // Auslesen Money Spent
    cnt := 0;
    Stream.Read(cnt, sizeof(cnt));
    For j := 0 To cnt - 1 Do Begin
      d := 0;
      Stream.Read(d, sizeof(d));
      // Wir zeigen nur die Statistik des eigenen Spielers an, auch wenn wir alle haben ;)
      If i = PlayerIndex Then Begin
        If j = cnt - 1 Then Begin
          Chart1LineSeries4.AddXY(lt, d);
        End
        Else Begin
          Chart1LineSeries4.AddXY(j * DateTime10S, d);
        End;
      End;
    End;
  End;
  // Den Aktuellen Spieler Sortieren wir immer in Spalte 1
  If PlayerIndex <> 0 Then Begin
    For j := 0 To StringGrid1.RowCount - 1 Do Begin
      s := StringGrid1.Cells[1, j];
      StringGrid1.Cells[1, j] := StringGrid1.Cells[1 + PlayerIndex, j];
      StringGrid1.Cells[1 + PlayerIndex, j] := s;
    End;
  End;
  // Die Farblichen "Anpassungen" für den Besten bestimmen
  setlength(fBoldCells, StringGrid1.ColCount, StringGrid1.RowCount);
  For i := 0 To StringGrid1.ColCount - 1 Do Begin
    For j := 0 To StringGrid1.RowCount - 1 Do Begin
      fBoldCells[i, j] := false;
    End;
  End;
  If StringGrid1.ColCount > 2 Then Begin
    For j := 1 To 17 Do Begin
      If j = 11 Then Continue;
      indexv := StrToInt64(CleanCell(StringGrid1.cells[1, j]));
      For i := 2 To StringGrid1.ColCount - 1 Do Begin
        Case j Of
          1, 3, 4, 7, 8, 10, 12, 13, 14, 15, 16, 17: Begin // Alle wo "Groß" gut ist
              If indexv < StrToInt64(CleanCell(StringGrid1.cells[i, j])) Then Begin
                indexv := StrToInt64(CleanCell(StringGrid1.cells[i, j]));
              End;
            End;
          2, 5, 6, 9: Begin // Alle wo "Klein" Gut ist
              If indexv > StrToInt64(CleanCell(StringGrid1.cells[i, j])) Then Begin
                indexv := StrToInt64(CleanCell(StringGrid1.cells[i, j]));
              End;
            End;
        End;
      End;
      // Alle Spieler die den Higscore erreicht haben werden markiert
      For i := 1 To StringGrid1.ColCount - 1 Do Begin
        If indexv = StrToInt64(CleanCell(StringGrid1.cells[i, j])) Then Begin
          fBoldCells[i, j] := true;
        End;
      End;
    End;
  End;
  StringGrid1.AutoSizeColumns;
End;

End.


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
Unit unit14;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uctd_mapobject;

Type

  TFileInfo = Record
    Source: String;
    Dest: String;
  End;

  TFileInfoList = Array Of TFileInfo;

  TTransferLocalServerFiles = Record
    Files: TFileInfoList; // Die Liste der zu Kopierenden Dateien
    ListBox2Name: String; // Der Name der beim Abschluss in die Listbox2 hinzugefügt werden muss
  End;

  TTransferShareServer = Record //
    Index: integer; // Der Index, welche Datei als nächstes übertragen werden soll
    FileList: TFileInfoList;
  End;

  TDialogMode = (dmBuildings, dmOpponents, dmHeros);

  { TForm14 }

  TForm14 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox2DblClick(Sender: TObject);
  private
    { private declarations }
    fmode: TDialogMode;
    fTruncedMapfolder: String;
    OnTransferLocalServer_LastFile: String; // Der Dateiname der "zuletzt" übertragenen Datei.
    fTransferLocalServerFiles: Array Of TTransferLocalServerFiles; //TTransferLocalServerFiles;
    fTransferShareServerFiles: TTransferShareServer;
    fOnLoadFinishEvent: TNotifyEvent;

    Procedure OnTransferLocalServer(Sender: TObject; Suceed: Boolean);
    Procedure OnTransferShareServer(Sender: TObject; Suceed: Boolean);
    Procedure OnGetListbox2Content(Sender: TObject; Const Data: TStringlist);

    (*
     * Routinen Local / Share -> Server
     *)
    Procedure TransferLocalServer(LocalName, ServerName: String); // Sendet von der Lokalen Position alles zum Server in die Map

    Procedure TransferShareLocal(ShareName, LocalName: String); // Transveriert von Lokalen Share nach Lokales Verzeichnis

  public
    { public declarations }
    Procedure LoadBuildingSettings(OnLoadBuildingFinishEvent: TNotifyEvent);
    Procedure LoadOpponentSettings;
    Procedure LoadHeroSettings(OnLoadHeroFinishEvent: TNotifyEvent);

    Function TransferShareServer(Obj: tctd_mapopbject): Boolean; // Sendet ein lokales Objekt an den Server
  End;

Var
  Form14: TForm14;

Implementation

{$R *.lfm}

Uses
  LazUTF8, LCLType, LazFileUtils
  , Unit1
  , unit4 // Karten Eigenschaften Dialog
  , Unit6 // Building Editor
  , unit7 // Opponent Editor
  , unit15 // Abfrage beim copieren von Opponents / Gebäuden in Unit14
  , unit19 // Hero Editor
  , uctd, uctd_common, uctd_building, uctd_opp, uctd_map, uctd_messages, uctd_hero
  ;

{ TForm14 }

Procedure TForm14.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  fTransferLocalServerFiles := Nil;
  OnTransferLocalServer_LastFile := '';
  fTransferShareServerFiles.FileList := Nil;
End;

Procedure TForm14.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Case fmode Of
    dmBuildings: Begin
        setValue('QuestionMapDlgForm', 'Left', inttostr(Form14.left));
        setValue('QuestionMapDlgForm', 'Top', inttostr(Form14.top));
        setValue('QuestionMapDlgForm', 'Height', inttostr(Form14.Height));
      End;
    dmOpponents: Begin
        setValue('QuestionOppDlgForm', 'Left', inttostr(Form14.left));
        setValue('QuestionOppDlgForm', 'Top', inttostr(Form14.top));
        setValue('QuestionOppDlgForm', 'Height', inttostr(Form14.Height));
      End;
    dmHeros: Begin
        setValue('QuestionHeroDlgForm', 'Left', inttostr(Form14.left));
        setValue('QuestionHeroDlgForm', 'Top', inttostr(Form14.top));
        setValue('QuestionHeroDlgForm', 'Height', inttostr(Form14.Height));
      End;
  End;
  If Assigned(ctd.Map) Then Begin
    Form1.RestoreForm4;
  End;
End;

Procedure TForm14.Button1Click(Sender: TObject);
Var
  s: String;
  i: integer;
  f: Textfile;
Begin
  log('TForm14.Button1Click', llTrace);
  // Create global
  s := trim(lowercase(edit1.text));
  If s = '' Then exit;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If lowercase(trim(ListBox1.Items[i])) = s Then Begin
      Case fmode Of
        dmBuildings: LogShow('Building name already exists.', llWarning);
        dmOpponents: LogShow('Opponent name already exists.', llWarning);
        dmHeros: LogShow('Hero name already exists.', llWarning);
      End;
      LogLeave;
      exit;
    End;
  End;
  If Not DirectoryExistsUTF8(fTruncedMapfolder) Then Begin
    If Not CreateDirUTF8(fTruncedMapfolder) Then Begin
      LogShow('Could not create directory : ' + fTruncedMapfolder, llWarning);
      LogLeave;
      exit;
    End;
  End;
  If Not CreateDirUTF8(fTruncedMapfolder + edit1.text) Then Begin
    LogShow('Could not create directory : ' + fTruncedMapfolder + edit1.text, llWarning);
    LogLeave;
    exit;
  End;
  Case fmode Of
    dmBuildings: Begin
        Building.free;
        Building := Tbuilding.create;
        If Not FileExistsUTF8(fTruncedMapfolder + edit1.text + PathDelim + edit1.text + '.geb') Then Begin
          assignfile(f, utf8tosys(fTruncedMapfolder + edit1.text + PathDelim + edit1.text + '.geb'));
          rewrite(f);
          writeln(f, '');
          CloseFile(f);
        End;
      End;
    dmOpponents: Begin
        Opponent.free;
        Opponent := TOpponent.create;
        If Not FileExistsUTF8(fTruncedMapfolder + edit1.text + PathDelim + edit1.text + '.opp') Then Begin
          assignfile(f, utf8tosys(fTruncedMapfolder + edit1.text + PathDelim + edit1.text + '.opp'));
          rewrite(f);
          writeln(f, '');
          CloseFile(f);
        End;
      End;
    dmHeros: Begin
        Hero.free;
        Hero := THero.create;
        If Not FileExistsUTF8(fTruncedMapfolder + edit1.text + PathDelim + edit1.text + '.hero') Then Begin
          assignfile(f, utf8tosys(fTruncedMapfolder + edit1.text + PathDelim + edit1.text + '.hero'));
          rewrite(f);
          writeln(f, '[Hero]');
          writeln(f, 'name=' + edit1.text);
          CloseFile(f);
        End;
      End;
  End;
  // Anfügen des Elementes in die Liste und Laden
  ListBox1.Items.Add(edit1.text);
  Application.ProcessMessages;
  // Suchen in der Sortierten Liste
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If listbox1.Items[i] = edit1.text Then Begin
      ListBox1.ItemIndex := i;
      break;
    End;
  End;
  Button4.Click; // Laden
  LogLeave;
End;

Procedure TForm14.Button2Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Delete in Map
  If ListBox2.ItemIndex <> -1 Then Begin
    Case fmode Of
      dmBuildings: Begin
          ctd.DelBuilding(ListBox2.items[ListBox2.ItemIndex] + '.geb');
          form4.ListBox1.Clear;
          For i := 0 To ctd.Map.BuyAblesCount - 1 Do Begin
            form4.listbox1.Items.add(BuyableToString(ctd.Map.BuyAbles[i]));
          End;
          form4.Edit6.Text := '';
          form4.Edit7.Text := '';
          If form4.ListBox1.Items.Count = 0 Then Begin
            form4.Edit6.Enabled := false;
            form4.Edit7.Enabled := false;
            form4.Button10.Enabled := false;
          End;
        End;
      dmOpponents: Begin
          ctd.DelOpponent(ListBox2.items[ListBox2.ItemIndex] + '.opp');
          form4.RefreshOpponentsClick(Nil);
        End;
      dmHeros: Begin
          ctd.Delhero(ListBox2.items[ListBox2.ItemIndex] + '.hero');
          form4.ListBox1.Clear;
          For i := 0 To ctd.Map.BuyAblesCount - 1 Do Begin
            form4.listbox1.Items.add(BuyableToString(ctd.Map.BuyAbles[i]));
          End;
          form4.Edit6.Text := '';
          form4.Edit7.Text := '';
          If form4.ListBox1.Items.Count = 0 Then Begin
            form4.Edit6.Enabled := false;
            form4.Edit7.Enabled := false;
            form4.Button10.Enabled := false;
          End;
        End;
    End;
    ListBox2.items.Delete(ListBox2.ItemIndex);
  End;
End;

Procedure TForm14.Button3Click(Sender: TObject);
Begin
  If form6.visible Then
    form6.close;
  If form7.visible Then
    form7.close;
  close;
End;

Procedure TForm14.Button4Click(Sender: TObject);
Var
  s: String;
Begin
  // Edit Global
  If ListBox1.ItemIndex = -1 Then Begin
    LogShow('No item selected.', llWarning);
    exit;
  End;
  If form6.Visible Then exit;
  If form7.Visible Then exit;
  s := ListBox1.Items[ListBox1.ItemIndex];
  // Global Laden
  Case fmode Of
    dmBuildings: Begin // Gebäude
        form6.ClearAll;
        unit6.Building.LoadFromFile(fTruncedMapfolder + s + PathDelim + s + '.geb');
        form6.buildingtolcl(ctd.Map);
        form6.ModalResult := mrNone;
        form6.transfer := false;
        form6.Show;
        hide;
      End;
    dmOpponents: Begin // Opponent
        form7.ClearAll;
        unit7.opponent.LoadFromFile(fTruncedMapfolder + s + PathDelim + s + '.opp');
        form7.opponenttolcl(ctd.Map);
        form7.ModalResult := mrNone;
        form7.transfer := false;
        form7.Show;
        Hide;
      End;
    dmHeros: Begin
        form19.ClearAll;
        unit19.Hero.LoadFromFile(fTruncedMapfolder + s + PathDelim + s + '.hero');
        form19.herotolcl(ctd.Map);
        form19.ModalResult := mrNone;
        form19.transfer := false;
        form19.Show;
        Hide;
      End;
  End;
End;

Procedure TForm14.Button5Click(Sender: TObject);
Var
  s: String;
Begin
  // Edit Current Map
  If ListBox2.ItemIndex = -1 Then Begin
    LogShow('No item selected.', llWarning);
    exit;
  End;
  s := ListBox2.Items[ListBox2.ItemIndex];
  If form6.Visible Then exit;
  If form7.Visible Then exit;
  // Map Laden
  Case fmode Of
    dmBuildings: Begin // Gebäude
        form6.ClearAll;
        unit6.Building.LoadFromFile(Mapfolder + mapname + PathDelim + s + '.geb');
        form6.buildingtolcl(ctd.Map);
        form6.ModalResult := mrNone;
        form6.transfer := true;
        form6.Show;
        hide;
      End;
    dmOpponents: Begin // Opponent
        form7.ClearAll;
        unit7.opponent.LoadFromFile(Mapfolder + mapname + PathDelim + s + '.opp');
        form7.Opponenttolcl(ctd.Map);
        form7.ModalResult := mrNone;
        form7.transfer := true;
        form7.Show;
        hide;
      End;
    dmHeros: Begin // Hero
        form19.ClearAll;
        unit19.hero.LoadFromFile(Mapfolder + mapname + PathDelim + s + '.hero');
        form19.Herotolcl(ctd.Map);
        form19.ModalResult := mrNone;
        form19.transfer := true;
        form19.Show;
        hide;
      End;
  Else Begin
      LogShow('Unspezified mode.', llWarning);
    End;
  End;
End;

Procedure TForm14.Button6Click(Sender: TObject);
Var
  s, n: String;
  j, i: Integer;
Begin
  If Not Button6.Enabled Then exit;
  // Kopieren Local nach Karte
  For j := 0 To ListBox1.Items.Count - 1 Do Begin
    //  If ListBox1.ItemIndex <> -1 Then Begin
    If ListBox1.Selected[j] Then Begin
      s := ListBox1.Items[j];
      n := s;
      For i := 0 To ListBox2.Items.Count - 1 Do Begin
        If LowerCase(trim(ListBox2.Items[i])) = lowercase(trim(s)) Then Begin
          form15.ModalResult := mrNone;
          form15.Comparer := lowercase(trim(s));
          form15.Direction := 0;
          form15.Edit1.Text := ListBox1.Items[j] + '_new';
          form15.ShowModal;
          If Form15.ModalResult <> mrOK Then exit;
          If Form15.RadioButton2.Checked Then Begin // Umbenennen Beim Schieben
            n := form15.Edit1.Text;
          End;
          break; // Form15 stellt sicher, dass der Neue Name nicht existiert, also können wir es uns sparen weiter zu prüfen
        End;
      End;
      //  das Hier testen, wenn das Geht, dann beim OK des Editierdialogs entsprechend Lokal oder auf dem Server Speichern
      Case fmode Of
        dmBuildings: Begin
            TransferLocalServer(fTruncedMapfolder + s + PathDelim + s + '.geb', n + '.geb');
          End;
        dmOpponents: Begin
            TransferLocalServer(fTruncedMapfolder + s + PathDelim + s + '.opp', n + '.opp');
            form4.Timer1.Enabled := true;
          End;
        dmHeros: Begin
            TransferLocalServer(fTruncedMapfolder + s + PathDelim + s + '.hero', n + '.hero');
          End;
      End;
      ListBox1.Selected[j] := false;
    End;
  End;
End;

Procedure TForm14.Button7Click(Sender: TObject);
Var
  s, n: String;
  i: Integer;
Begin
  // Kopieren von Karte nach Local
  If ListBox2.ItemIndex <> -1 Then Begin
    s := ListBox2.Items[ListBox2.ItemIndex];
    n := s;
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If LowerCase(trim(ListBox1.Items[i])) = lowercase(trim(s)) Then Begin
        form15.ModalResult := mrNone;
        form15.Comparer := lowercase(trim(s));
        form15.Direction := 0;
        form15.Edit1.Text := ListBox2.Items[ListBox2.ItemIndex] + '_new';
        form15.ShowModal;
        If Form15.ModalResult <> mrOK Then exit;
        If Form15.RadioButton2.Checked Then Begin // Umbenennen Beim Schieben
          n := form15.Edit1.Text;
        End;
        break; // Form15 stellt sicher, dass der Neue Name nicht existiert, also können wir es uns sparen weiter zu prüfen
      End;
    End;
    //    das Hier testen, wenn das Geht, dann beim OK des Editierdialogs entsprechend Lokal oder auf dem Server Speichern
    Case fmode Of
      dmBuildings: Begin
          TransferShareLocal(MapFolder + MapName + PathDelim + s + '.geb', n);
        End;
      dmOpponents: Begin
          TransferShareLocal(MapFolder + MapName + PathDelim + s + '.opp', n);
        End;
      dmHeros: Begin
          TransferShareLocal(MapFolder + MapName + PathDelim + s + '.hero', n);
        End;
    End;
  End;
End;

Procedure TForm14.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    Button1.Click;
  End;
End;

Procedure TForm14.FormShow(Sender: TObject);
Begin
  Case fmode Of
    dmBuildings: Begin
        Form14.left := strtoint(GetValue('QuestionMapDlgForm', 'Left', inttostr(Form14.left)));
        Form14.top := strtoint(GetValue('QuestionMapDlgForm', 'Top', inttostr(Form14.top)));
        Form14.Height := strtoint(GetValue('QuestionMapDlgForm', 'Height', inttostr(Form14.Height)));
      End;
    dmOpponents: Begin
        Form14.left := strtoint(GetValue('QuestionOppDlgForm', 'Left', inttostr(Form14.left)));
        Form14.top := strtoint(GetValue('QuestionOppDlgForm', 'Top', inttostr(Form14.top)));
        Form14.Height := strtoint(GetValue('QuestionOppDlgForm', 'Height', inttostr(Form14.Height)));
      End;
    dmHeros: Begin
        Form14.left := strtoint(GetValue('QuestionHeroDlgForm', 'Left', inttostr(Form14.left)));
        Form14.top := strtoint(GetValue('QuestionHeroDlgForm', 'Top', inttostr(Form14.top)));
        Form14.Height := strtoint(GetValue('QuestionHeroDlgForm', 'Height', inttostr(Form14.Height)));
      End;
  End;
  FixFormPosition(form14);
End;

Procedure TForm14.ListBox1DblClick(Sender: TObject);
Begin
  // Move =>
  button6.click;
End;

Procedure TForm14.ListBox2DblClick(Sender: TObject);
Begin
  // Move <=
  Button7.Click;
End;

Procedure TForm14.OnTransferLocalServer(Sender: TObject; Suceed: Boolean);
Var
  t: String;
  i: Integer;
  m: TMemoryStream;
  b: TBuyAble;
Begin
  // Bei Mehreren Kopieen wird diese Callback einmal zu oft aufgerufen, ka warum, aber da es dann nix mehr zu kopieren gibt -> Raus und gut.
  If Not assigned(fTransferLocalServerFiles) Then Begin
    exit;
  End;
  Log('TForm14.OnFileSend', llTrace);
  If Not Suceed Then Begin
    // Wir brechen die Nummer ab mit der Fehlermeldung auf die Letzte Datei
    LogShow('Could not transfer : ' + ExtractFileName(OnTransferLocalServer_LastFile), llError);
  End
  Else Begin
    // Senden der Nächsten Datei
    If length(fTransferLocalServerFiles[0].Files) = 0 Then Begin
      // Wir haben Alle Dateien versendet
      t := ExtractFileNameOnly(fTransferLocalServerFiles[0].ListBox2Name);
      For i := 0 To ListBox2.Items.Count - 1 Do Begin
        If ListBox2.Items[i] = t Then Begin
          ListBox2.ItemIndex := i;
          LogLeave;
          exit;
        End;
      End;
      Listbox2.Items.Add(t);
      ListBox2.ItemIndex := Listbox2.Items.Count - 1;
      Case fmode Of
        dmBuildings: Begin
            // Das Gebäude ist vollständig übertragen dann wird es automatisch mit Wave 1 hinzugefügt.
            b.Item := t + '.geb';
            b.WaveNum := 0;
            b.Count := 0;
            b.Kind := bkBuilding;
            m := TMemoryStream.Create;
            m.WriteAnsiString(b.Item);
            m.Write(b.WaveNum, sizeof(b.WaveNum));
            m.Write(b.Count, sizeof(b.Count));
            m.Write(b.Kind, sizeof(b.Kind));
            //  ctd.Map.addBuyable(b.Item, b.WaveNum, b.Count); -- Das macht ctd schon
            ctd.UpdateMapProperty(mpAddBuyable, m);
            form4.listbox1.Items.add(BuyableToString(b));

            // Freischalten des Editierens der Gebäude Eigenschaften
            form4.Edit6.Enabled := true;
            form4.Edit7.Enabled := true;
            form4.Button10.Enabled := true;

          End;
        dmOpponents: Begin
            // For the Future ?
          End;
        dmHeros: Begin
            // Das Gebäude ist vollständig übertragen dann wird es automatisch mit Wave 1 hinzugefügt.
            b.Item := t + '.hero';
            b.WaveNum := 0;
            b.Count := 1; // Per Default lassen wir nur einen Helden zu, soll der Level Designer das aufbohren
            b.Kind := bkHero;
            m := TMemoryStream.Create;
            m.WriteAnsiString(b.Item);
            m.Write(b.WaveNum, sizeof(b.WaveNum));
            m.Write(b.Count, sizeof(b.Count));
            m.Write(b.Kind, sizeof(b.Kind));
            //  ctd.Map.addBuyable(b.Item, b.WaveNum, b.Count); -- Das macht ctd schon
            ctd.UpdateMapProperty(mpAddBuyable, m);
            form4.listbox1.Items.add(BuyableToString(b));

            // Freischalten des Editierens der Gebäude Eigenschaften
            form4.Edit6.Enabled := true;
            form4.Edit7.Enabled := true;
            form4.Button10.Enabled := true;
          End;
      End;
      // Das Objekt aus der Fifo nehmen
      For i := 1 To high(fTransferLocalServerFiles) Do Begin
        fTransferLocalServerFiles[i - 1] := fTransferLocalServerFiles[i];
      End;
      SetLength(fTransferLocalServerFiles, high(fTransferLocalServerFiles));
      // Es gibt noch weitere "Objekte" die gesender werden sollen, neustart durch Rekursiven Aufruf ;)
      If assigned(fTransferLocalServerFiles) Then Begin
        OnTransferLocalServer(Sender, true);
      End;
    End
    Else Begin
      // Nächste zu versendende Datei Auswählen
      OnTransferLocalServer_LastFile := fTransferLocalServerFiles[0].Files[0].Source;
      ctd.TransferFile(
        fTransferLocalServerFiles[0].Files[0].Source,
        fTransferLocalServerFiles[0].Files[0].Dest,
        @OnTransferLocalServer);
      // Die Gerade abgeschickte Datei aus der Liste nehmen ...
      For i := 1 To high(fTransferLocalServerFiles[0].Files) Do Begin
        fTransferLocalServerFiles[0].Files[i - 1] := fTransferLocalServerFiles[0].Files[i];
      End;
      setlength(fTransferLocalServerFiles[0].Files, high(fTransferLocalServerFiles[0].Files));
    End;
  End;
  LogLeave;
End;

Procedure TForm14.OnTransferShareServer(Sender: TObject; Suceed: Boolean);
Begin
  Log('TForm14.OnFileSend', llTrace);
  If Not Suceed Then Begin
    // Wir brechen die Nummer ab mit der Fehlermeldung auf die Letzte Datei
    LogShow('Could not transfer : ' + fTransferShareServerFiles.FileList[fTransferShareServerFiles.Index].Dest, llError);
  End
  Else Begin
    // Senden der Nächsten Datei
    fTransferShareServerFiles.Index := fTransferShareServerFiles.Index + 1; // Nächste zu versendende Datei auswählen
    If fTransferShareServerFiles.Index > high(fTransferShareServerFiles.FileList) Then Begin
      setlength(fTransferShareServerFiles.FileList, 0); // wir sind fertig mit senden also alles wieder resetten
      fTransferShareServerFiles.Index := -1;
    End
    Else Begin
      // Nächste zu versendende Datei Auswählen
      ctd.TransferFile(
        fTransferShareServerFiles.FileList[fTransferShareServerFiles.Index].Source,
        fTransferShareServerFiles.FileList[fTransferShareServerFiles.Index].Dest,
        @OnTransferShareServer);
    End;
  End;
  LogLeave;
End;

Procedure TForm14.OnGetListbox2Content(Sender: TObject; Const Data: TStringlist
  );
Var
  i: Integer;
Begin
  For i := 0 To Data.Count - 1 Do Begin
    listbox2.items.add(ExtractFileNameOnly(Data[i]));
  End;
  If assigned(fOnLoadFinishEvent) Then
    fOnLoadFinishEvent(Nil);
  fOnLoadFinishEvent := Nil;
End;

Procedure TForm14.LoadBuildingSettings(OnLoadBuildingFinishEvent: TNotifyEvent);
Var
  sl: TStringList;
Begin
  If form6.visible Then form6.Close; // Building Editor
  If form7.visible Then form7.Close; // Opponent Editor
  If form19.visible Then form19.Close; // Hero Editor

  fmode := dmBuildings;
  caption := 'Select buildings for map';
  label1.Caption := 'Available buildings:';
  button5.Enabled := Assigned(ctd.Map);
  button6.Enabled := Assigned(ctd.Map);
  button7.Enabled := Assigned(ctd.Map);
  // Die "Globalen"
  ListBox1.Clear;
  ListBox1.Sorted := true;
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'buildings' + PathDelim;
  sl := ListAllSubdirs(fTruncedMapfolder);
  sl.Sorted := true;
  sl.Sort;
  ListBox1.Items.Text := sl.Text;
  sl.free;
  Edit1.text := '';
  // Die der Lokalen Karte
  ListBox2.Clear;
  ListBox2.Sorted := true;
  fOnLoadFinishEvent := OnLoadBuildingFinishEvent;
  ctd.GetFileList('*.geb', @OnGetListbox2Content);
End;

Procedure TForm14.LoadOpponentSettings;
Var
  sl: TStringList;
Begin
  If form6.visible Then form6.Close; // Building Editor
  If form7.visible Then form7.Close; // Opponent Editor
  If form19.visible Then form19.Close; // Hero Editor
  fmode := dmOpponents;
  caption := 'Select opponents for map';
  label1.Caption := 'Available creeps:';
  button5.Enabled := Assigned(ctd.Map);
  button6.Enabled := Assigned(ctd.Map);
  button7.Enabled := Assigned(ctd.Map);
  // Die "Globalen"
  ListBox1.Clear;
  ListBox1.Sorted := true;
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'opponents' + PathDelim;
  sl := ListAllSubdirs(fTruncedMapfolder);
  sl.Sorted := True;
  sl.Sort;
  ListBox1.Items.Text := sl.Text;
  sl.free;
  Edit1.text := '';
  // Die der Lokalen Karte
  ListBox2.Clear;
  ListBox2.Sorted := true;
  fOnLoadFinishEvent := Nil; // Gegner können aus Form4 heraus nicht direkt geladen werden, deswegen brauchen wir den Event an der Stelle nicht..
  ctd.GetFileList('*.opp', @OnGetListbox2Content);
End;

Procedure TForm14.LoadHeroSettings(OnLoadHeroFinishEvent: TNotifyEvent);
Var
  sl: TStringList;
Begin
  If form6.visible Then form6.Close; // Building Editor
  If form7.visible Then form7.Close; // Opponent Editor
  If form19.visible Then form19.Close; // Hero Editor
  fmode := dmHeros;
  caption := 'Select heros for map';
  label1.Caption := 'Available heros:';
  button5.Enabled := Assigned(ctd.Map);
  button6.Enabled := Assigned(ctd.Map);
  button7.Enabled := Assigned(ctd.Map);
  // Die "Globalen"
  ListBox1.Clear;
  ListBox1.Sorted := true;
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'heros' + PathDelim;
  sl := ListAllSubdirs(fTruncedMapfolder);
  sl.Sorted := true;
  sl.Sort;
  ListBox1.Items.Text := sl.Text;
  sl.free;
  Edit1.text := '';
  // Die der Lokalen Karte
  ListBox2.Clear;
  ListBox2.Sorted := true;
  fOnLoadFinishEvent := OnLoadHeroFinishEvent;
  ctd.GetFileList('*.hero', @OnGetListbox2Content);
End;

Procedure TForm14.TransferLocalServer(LocalName, ServerName: String);
Var
  index, i: integer;
  sl: TStringList;
  t: String;
Begin
  log(format('TForm14.Transfer %s => %s', [LocalName, ServerName]), llTrace);
  setlength(fTransferLocalServerFiles, high(fTransferLocalServerFiles) + 2);
  index := high(fTransferLocalServerFiles);
  sl := FindAllFiles(ExtractFilePath(LocalName), '*', false);
  setlength(fTransferLocalServerFiles[index].Files, sl.Count);
  For i := 0 To sl.Count - 1 Do Begin
    t := ExtractFileName(sl[i]);
    If sl[i] = LocalName Then Begin
      t := ServerName; // Ist ja bereits ohne Pfade, deswegen muss der nicht extra extrahiert werden
    End;
    fTransferLocalServerFiles[index].Files[i].Source := sl[i];
    fTransferLocalServerFiles[index].Files[i].Dest := t;
  End;
  sl.free;
  fTransferLocalServerFiles[index].ListBox2Name := ServerName;
  OnTransferLocalServer(self, true); // Wir starten das Senden der Dateien in dem wir behaupten,das die letzte erfolgreich gesendet wurde.
  LogLeave;
End;

Function TForm14.TransferShareServer(Obj: tctd_mapopbject): Boolean;
Var
  sl: TStringList;
  lp: String;
  i: Integer;
Begin
  result := false;
  If high(fTransferShareServerFiles.FileList) <> -1 Then Begin
    logshow('Error already transmitting data, please wait..', llError);
    exit;
  End;
  sl := Obj.ListOfImages();
  lp := IncludeTrailingPathDelimiter(extractfilepath(Obj.Filename));
  setlength(fTransferShareServerFiles.FileList, sl.count + 1);
  For i := 0 To sl.Count - 1 Do Begin
    fTransferShareServerFiles.FileList[i].Source := lp + sl[i];
    fTransferShareServerFiles.FileList[i].Dest := sl[i];
    If Not FileExists(fTransferShareServerFiles.FileList[i].Source) Then Begin
      ShowMessage('Error, building is invalid, can not transfer ..');
      sl.free;
      setlength(fTransferShareServerFiles.FileList, 0);
      exit;
    End;
  End;
  sl.free;
  // Am Schluss das Objekt selbst
  i := high(fTransferShareServerFiles.FileList);
  fTransferShareServerFiles.FileList[i].Source := Obj.Filename;
  fTransferShareServerFiles.FileList[i].Dest := ExtractFileName(Obj.Filename);
  fTransferShareServerFiles.Index := -1;
  OnTransferShareServer(self, true); // Wir starten das Senden der Dateien in dem wir behaupten,das die letzte erfolgreich gesendet wurde.
  result := true;
End;

Procedure TForm14.TransferShareLocal(ShareName, LocalName: String);
Var
  sl: TStringList;
  pp, p, ext: String;
  obj: tctd_mapopbject;
  i: Integer;
Begin
  log('TForm14.TransferShareLocal', llTrace);
  ext := lowercase(ExtractFileExt(ShareName));
  If ext = '.geb' Then Begin
    obj := TBuilding.create();
  End
  Else Begin
    obj := TOpponent.create();
  End;
  // Erstellen der Notwendigen Ordner falls nicht vorhanden
  p := fTruncedMapfolder + LocalName + PathDelim;
  If Not DirectoryExistsUTF8(fTruncedMapfolder) Then Begin
    If Not CreateDirUTF8(fTruncedMapfolder) Then Begin
      LogShow('Could not create : ' + fTruncedMapfolder, llError);
      obj.free;
      LogLeave;
      exit;
    End;
  End;
  If Not DirectoryExistsUTF8(p) Then Begin
    If Not CreateDirUTF8(p) Then Begin
      LogShow('Could not create : ' + p, llError);
      obj.free;
      LogLeave;
      exit;
    End;
  End;
  // Kopieren der Objekt Datei
  If Not CopyFile(utf8tosys(ShareName), utf8tosys(p + Localname + ExtractFileext(ShareName))) Then Begin
    LogShow('Could not copy : ' + ShareName, llError);
    obj.free;
    LogLeave;
    exit;
  End;
  // Laden der Bilderliste
  obj.LoadFromFile(ShareName);
  sl := Obj.ListOfImages();
  obj.free;
  pp := IncludeTrailingPathDelimiter(ExtractFilePath(ShareName));
  For i := 0 To sl.Count - 1 Do Begin
    If (sl[i] <> '') Then Begin
      If FileExistsUTF8(pp + sl[i]) Then Begin
        If Not CopyFile(utf8tosys(pp + sl[i]), p + sl[i]) Then Begin
          LogShow('Could not copy : ' + pp + sl[i], llError);
          sl.free;
          LogLeave;
          exit;
        End;
      End
      Else Begin
        LogShow('Could not find : ' + pp + sl[i], llError);
        sl.free;
        LogLeave;
        exit;
      End;
    End;
  End;
  sl.free;
  p := ExtractFileNameOnly(ShareName);
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Items[i] = p Then Begin
      LogLeave;
      exit;
    End;
  End;
  Listbox1.Items.Add(p);
  LogLeave;
End;

End.


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
Unit unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Spin, ExtDlgs, Buttons, Types,
  uctd_mapobject, uctd_map, ufifo, uwave_frame, uwave_oppenent_frame;

Type

  (*
   * Liste aller Events die über den OnIdle Process verarbeitet werden (und damit nicht mehr im L-Net Task)
   *)

  TCmd = (
    cNone
    , cUpdateMapProperty
    , cCloneWave
    , cExchangeWave
    );

  TLCLRecord = Record
    Cmd: TCmd;
    AdditionalInfo: Integer;
    SenderUid: Integer;
    Data: TStream;
  End;

  TLCLFifo = specialize TFifo < TLCLRecord > ;

  TOnTransferFileDoneReason = (frUnknown, frBackTex, frDC1Tex, frDC2Tex, frDC3Tex, frDC4Tex);

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button20: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    SpinEdit1: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Timer1: TTimer;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Button14Click(Sender: TObject);
    Procedure Button15Click(Sender: TObject);
    Procedure Button16Click(Sender: TObject);
    Procedure Button17Click(Sender: TObject);
    Procedure Button18Click(Sender: TObject);
    Procedure Button19Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button20Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure CheckBox5Change(Sender: TObject);
    Procedure CheckBox6Change(Sender: TObject);
    Procedure CheckBox7Change(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure Memo1Change(Sender: TObject);
    Procedure PageControl2Change(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure OnEditEnter(Sender: TObject);
    Procedure OnEditExit(Sender: TObject);
  private
    { private declarations }
    fTransferFilename: String;
    fOnTransferFileDoneReason: TOnTransferFileDoneReason;
    fOnFinishedLoadListDuringLoadObj: String;
    Procedure OnFinishedLoadListDuringLoad(Sender: TObject);
    Procedure OnGetComboBox2Content(Sender: TObject; Const Data: TStringlist);
    Procedure OnGetPageControl2Content(Sender: TObject; Const Data: TStringlist);
    Procedure OnTransferFileDone(Sender: TObject; Suceed: Boolean);
    Procedure AdjustMoveWaveButtons;
    Procedure SendSetWaveCountTo(WaveCount: integer);
    Procedure AddWave(Reset: Boolean = true);
    Procedure SortForm4Buyables;
    Procedure DoUpdateMapProperty(Sender: TObject; MapProperty: Integer;
      SenderUid: Integer; Const Data: TStream);
    Procedure DoLCLWaveClone(Sender: TObject; SourceWaveNum, SenderUid: Integer);
    Procedure DoWaveExChange(Sender: TObject; SenderUid: Integer; Const Data: TStream);
  public
    { public declarations }
    Procedure SetWaveCountTo(WaveCount: integer; SenderUid: Integer);
    Procedure AddForm4Buyable(b: TBuyAble);

    Procedure ResetWave(Const Page: TTabSheet);
    Procedure DelLastWave;
    Procedure RefreshOpponentsClick(Sender: TObject);
    Procedure ChangeWaveIndexClick(WaveIndex: integer; Direction: Boolean);
    Procedure OnCTDWaveClone(Sender: TObject; SourceWaveNum, SenderUid: Integer);

    Procedure RefreshForm4Buyables;
    Procedure HandleLCLEvent(Const Event: TLCLRecord);
    Procedure UpdateLCLWayLen;
  End;

Var
  Form4: TForm4;
  PlacementeObject: tctd_mapopbject = Nil;
  Form4updating: Boolean = false;
  LclFifo: TLCLFifo; // Sync Fifo Lnet Incomming Msg's to LCL

Function BuyableToString(Const data: TBuyAble): String;
Function StringToBuyAble(Data: String): TBuyAble;

Implementation

{$R *.lfm}

Uses
  math, LazFileUtils, LazUTF8,
  Unit1
  , unit3
  , unit6
  , unit7
  , unit14
  , unit19
  , uctd, uctd_messages, uctd_common, uctd_building, uctd_opp,
  LCLType;

Function BuyableToString(Const data: TBuyAble): String;
  Function KindToString(aKind: TBuyAbleKind): String;
  Begin
    Case aKind Of
      bkBuilding: result := 'building';
      bkHero: result := 'hero';
    Else Begin
        Raise exception.Create('BuyableToString: missing implementation');
      End;
    End;
  End;

Begin
  result := format('%s : %d : %d : %s', [ExtractFileNameOnly(data.Item), data.WaveNum + 1, data.Count, KindToString(data.Kind)]);
End;

Function StringToBuyAble(Data: String): TBuyAble;
Var
  pre: String;
Begin
  pre := trim(Copy(data, 1, pos(':', data) - 1));
  result.Item := pre;
  delete(data, 1, pos(':', data));

  pre := trim(Copy(data, 1, pos(':', data) - 1));
  result.WaveNum := strtointdef(pre, 1) - 1;
  delete(data, 1, pos(':', data));

  pre := trim(Copy(data, 1, pos(':', data) - 1));
  result.Count := strtointdef(pre, 1);
  delete(data, 1, pos(':', data));

  pre := trim(data);
  Case pre Of
    'building': result.Kind := bkBuilding;
    'hero': result.Kind := bkHero;
  Else Begin
      Raise exception.create('StringToBuyAble: missing implementation');
    End;
  End;

  Case result.Kind Of
    bkBuilding: result.item := result.item + '.geb';
    bkHero: result.item := result.item + '.hero';
  Else Begin
      Raise exception.Create('StringToBuyAble: missing implementation');
    End;
  End;
End;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  fOnTransferFileDoneReason := frUnknown;
  PageControl1.ActivePageIndex := 0;
  ComboBox2.Clear;
  Edit5.text := '1';
  Edit6.text := '1';
  Edit7.text := '0';
  label20.caption := '';
End;

Procedure TForm4.FormDestroy(Sender: TObject);
Begin
  If assigned(PlacementeObject) Then
    PlacementeObject.free;
  PlacementeObject := Nil;
End;

Procedure TForm4.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  setValue('MapEditorForm', 'Left', inttostr(form4.left));
  setValue('MapEditorForm', 'Top', inttostr(form4.top));
  setValue('MapEditorForm', 'Width', inttostr(form4.Width));
  setValue('MapEditorForm', 'Height', inttostr(form4.Height));
  canclose := Not ctd.IsInEditMode;
End;

Procedure TForm4.RefreshForm4Buyables;
Var
  i: Integer;
Begin
  form4.ListBox1.Clear;
  For i := 0 To ctd.Map.BuyAblesCount - 1 Do Begin
    AddForm4Buyable(ctd.Map.BuyAbles[i]);
  End;
  form4.Edit6.Text := '';
  form4.Edit7.Text := '';
  If form4.ListBox1.Items.Count = 0 Then Begin
    form4.Edit6.Enabled := false;
    form4.Edit7.Enabled := false;
    form4.Button10.Enabled := false;
  End;
End;

Procedure TForm4.SortForm4Buyables;
  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: TBuyAble;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := StringToBuyAble(ListBox1.Items[Trunc((li + re) / 2)]); // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareBuyAble(StringToBuyAble(ListBox1.Items[l]), p) < 0 Do
          inc(l);
        While CompareBuyAble(StringToBuyAble(ListBox1.Items[r]), p) > 0 Do
          dec(r);
        If L <= R Then Begin
          ListBox1.Items.Exchange(l, r);
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Var
  s: String;
  i: Integer;
Begin
  s := '';
  If listbox1.ItemIndex <> -1 Then Begin
    s := listbox1.items[listbox1.ItemIndex];
  End;
  quick(0, listbox1.items.Count - 1);
  If s <> '' Then Begin
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If s = listbox1.items[I] Then Begin
        listbox1.ItemIndex := i;
        break;
      End;
    End;
  End;
End;

Procedure TForm4.DoLCLWaveClone(Sender: TObject; SourceWaveNum,
  SenderUid: Integer);
Var
  DestWaveIndex: Integer;
  fcloneWave: Twave;
Begin
  AddWave(false); // Die Neue Wave in der LCL anlegen
  DestWaveIndex := PageControl2.PageCount - 1;
  If ctd.OwnServerUid = SenderUid Then Begin
    PageControl2.ActivePageIndex := DestWaveIndex;
  End;
  fcloneWave := ctd.Map.Waves[SourceWaveNum];
  (PageControl2.Pages[DestWaveIndex].Components[0] As TWaveFrame).LoadWave(fcloneWave);
  Timer1.Enabled := true; // Refresh der Comboboxen anstoßen
  AdjustMoveWaveButtons;
End;

Procedure TForm4.DoWaveExChange(Sender: TObject; SenderUid: Integer;
  Const Data: TStream);
Var
  w1i, w2i: integer;
  w1, w2: TWave;
Begin
  w1i := -1;
  data.Read(w1i, sizeof(w1i));
  w2i := -1;
  data.Read(w2i, sizeof(w2i));
  w1 := (PageControl2.Pages[w1i].Components[0] As TWaveFrame).GetWave();
  w2 := (PageControl2.Pages[w2i].Components[0] As TWaveFrame).GetWave();
  (PageControl2.Pages[w1i].Components[0] As TWaveFrame).LoadWave(w2);
  (PageControl2.Pages[w2i].Components[0] As TWaveFrame).LoadWave(w1);
  If SenderUid = ctd.OwnServerUid Then Begin
    PageControl2.PageIndex := w2i;
  End;
  Timer1.Enabled := true; // Refresh der Comboboxen anstoßen
  AdjustMoveWaveButtons;
End;

Procedure TForm4.OnEditEnter(Sender: TObject);
Var
  e: TEdit absolute Sender;
Begin
  e.Tag := strtointdef(e.Text, 0);
End;

Procedure TForm4.OnEditExit(Sender: TObject);
Var
  e: TEdit absolute Sender;
Begin
  e.Text := inttostr(e.Tag);
End;

Procedure TForm4.HandleLCLEvent(Const Event: TLCLRecord);
Begin
  // Wenn die LCL Aktualisiert wird darf nicht erneut Update generiert werden -> Block an (nur hier und beim Laden)
  ctd.BlockMapUpdateSending := true;
  Case event.Cmd Of
    cUpdateMapProperty: Begin
        DoUpdateMapProperty(self, event.AdditionalInfo, event.SenderUid, event.Data);
      End;
    cCloneWave: Begin
        DoLCLWaveClone(self, event.AdditionalInfo, event.SenderUid);
      End;
    cExchangeWave: Begin
        DoWaveExChange(self, event.SenderUid, event.Data);
      End;
  End;
  If assigned(event.Data) Then event.Data.Free;
  ctd.BlockMapUpdateSending := false;
End;

Procedure TForm4.UpdateLCLWayLen;
Begin
  If CheckBox5.Checked Then Begin
    label20.caption := format('%0.1f', [ctd.Map.GetWayLen(ctd.Map.ViewWaypoints)]);
  End
  Else Begin
    label20.caption := '';
  End;
End;

Procedure TForm4.ComboBox1Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  i := ComboBox1.ItemIndex;
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpMapType, m);
End;

Procedure TForm4.ComboBox2Change(Sender: TObject);
Var
  s: String;
Begin
  s := MapFolder + MapName + PathDelim + ComboBox2.Text;
  If assigned(PlacementeObject) Then
    PlacementeObject.free;
  PlacementeObject := Nil;
  Case FilenameToType(s) Of
    moBuilding: PlacementeObject := TBuilding.create();
    moOpponent: PlacementeObject := TOpponent.create();
  End;
  If assigned(PlacementeObject) Then Begin
    PlacementeObject.LoadFromFile(s);
    PlacementeObject.Position.x := 0;
    PlacementeObject.Position.y := 0;
  End;
End;

Procedure TForm4.Button5Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
  s: String;
Begin
  // Check and Save Map
  log('TForm4.Button5Click', llTrace);
  If Form1.SaveAndCheckMap(true, true) Then Begin
    LogShow('Congratulations. Your maps has been approved to be playable and without warnings!', llInfo);
  End
  Else Begin
    sl := ctd.Map.GetListOfUnusedOpponents();
    If sl.count <> 0 Then Begin
      s := '';
      For i := 0 To sl.count - 1 Do Begin
        If i <> 0 Then s := s + ', ';
        s := s + ExtractFileName(ExtractFileNameWithoutExt(sl[i]));
      End;
      If ID_YES = Application.MessageBox(pchar('There are opponents [' + s + '] which where spawned in no wave, do you want to remove them now ?'), 'Question', mb_iconquestion Or MB_YESNO) Then Begin
        ctd.CleanupUnusedOpponets();
      End;
    End;
    sl.free;
  End;
  LogLeave;
End;

Procedure TForm4.Button6Click(Sender: TObject);
Begin
  // Open building Editor
  Form1.MenuItem15Click(Nil);
End;

Procedure TForm4.Button7Click(Sender: TObject);
Begin
  // Add new Wave
  SendSetWaveCountTo(length(ctd.Map.Waves) + 1);
End;

Procedure TForm4.SendSetWaveCountTo(WaveCount: integer);
Var
  m: TMemoryStream;
Begin
  If Not ctd.BlockMapUpdateSending Then Begin
    m := TMemoryStream.Create;
    m.write(WaveCount, sizeof(WaveCount));
    ctd.UpdateMapProperty(mpWaveCount, m);
  End;
End;

Procedure TForm4.SetWaveCountTo(WaveCount: integer; SenderUid: Integer);
Var
  waves: Integer;
Begin
  // Set Waves
  Form4updating := true;
  waves := WaveCount;
  While waves > PageControl2.PageCount Do Begin
    AddWave;
    ResetWave(PageControl2.pages[PageControl2.PageCount - 1]);
  End;
  While waves < PageControl2.PageCount Do
    DelLastWave;
  AdjustMoveWaveButtons;

  If (SenderUid = ctd.OwnServerUid) Then Begin
    PageControl2.ActivePageIndex := PageControl2.PageCount - 1;
  End;
  Timer1.Enabled := true; // Refresh der Comboboxen anstoßen
  Form4updating := false;
End;

Procedure TForm4.OnCTDWaveClone(Sender: TObject; SourceWaveNum,
  SenderUid: Integer);
Var
  Event: TLCLRecord;
Begin
  event.cmd := cCloneWave;
  Event.AdditionalInfo := SourceWaveNum;
  Event.SenderUid := SenderUid;
  Event.data := Nil;
  LclFifo.Push(Event);
End;

Procedure TForm4.Button8Click(Sender: TObject);
Begin
  If (PageControl2.PageCount = 0) Then exit; // Es gibt keine Waves zum Clonen
  ctd.CloneMapWave((PageControl2.Pages[PageControl2.ActivePageIndex].Components[0] As TWaveFrame).WaveNum);
End;

Procedure TForm4.Button9Click(Sender: TObject);
Begin
  // Start restart Game
  Form1.MenuItem18Click(Nil);
End;

Procedure TForm4.CheckBox1Change(Sender: TObject);
Begin
  If assigned(ctd.Map) Then
    ctd.Map.ShowBackTex := CheckBox1.Checked;
  If sender = CheckBox1 Then Begin
    CheckBox8.Checked := CheckBox1.Checked;
  End
  Else Begin
    CheckBox1.Checked := CheckBox8.Checked;
  End;
End;

Procedure TForm4.CheckBox2Change(Sender: TObject);
Begin
  If CheckBox2.Checked Then Begin
    If assigned(ctd.Map) Then Begin
      ctd.Map.ShowBackTex := false;
    End;
    CheckBox1.Checked := false;
    // Waypoints Deaktivieren
    CheckBox5.Checked := false;
    ctd.Map.ViewWaypoints := -1;
    // Placements Deaktivieren
    CheckBox6.Checked := false;
    // Wegpunkt Flächen Deaktivieren
    CheckBox7.Checked := false;
  End;
  If assigned(ctd.Map) Then Begin
    ctd.Map.EditTerrain := CheckBox2.Checked
  End;
End;

Procedure TForm4.CheckBox5Change(Sender: TObject);
Begin
  If CheckBox5.Checked Then Begin
    ctd.Map.ViewWaypoints := SpinEdit1.Value - 1;
    // Edit Terrain Deaktivieren
    CheckBox2.Checked := false;
    // Placements Deaktivieren
    CheckBox6.Checked := false;
    // Wegpunkt Flächen Deaktivieren
    CheckBox7.Checked := false;
  End
  Else Begin
    ctd.Map.ViewWaypoints := -1;
  End;
  UpdateLCLWayLen;
End;

Procedure TForm4.CheckBox6Change(Sender: TObject);
Begin
  If CheckBox6.Checked Then Begin
    // Edit Terrain Deaktivieren
    CheckBox2.Checked := false;
    // Waypoints Deaktivieren
    CheckBox5.Checked := false;
    // Wegpunkt Flächen Deaktivieren
    CheckBox7.Checked := false;
    ctd.Map.ViewWaypoints := -1;
  End;
End;

Procedure TForm4.CheckBox7Change(Sender: TObject);
Begin
  If CheckBox7.Checked Then Begin
    CheckBox1.Checked := false;
    CheckBox2.Checked := false;
    // Waypoints Deaktivieren
    CheckBox5.Checked := false;
    ctd.Map.ViewWaypoints := -1;
    // Placements Deaktivieren
    CheckBox6.Checked := false;
  End;
End;

Procedure TForm4.Button3Click(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  form3.caption := 'Resize map...';
  form3.Edit3.Visible := false;
  form3.Label3.Visible := false;
  Form3.Button3.Visible := false;
  form3.Edit1.Text := inttostr(ctd.Map.Width);
  form3.Edit2.Text := inttostr(ctd.Map.Height);
  form3.ModalResult := mrNone;
  If form3.ShowModal = mrOK Then Begin
    m := TMemoryStream.Create;
    i := max(1, strtointdef(form3.Edit1.Text, ctd.Map.Width));
    m.Write(i, sizeof(i));
    i := max(1, strtointdef(form3.Edit2.Text, ctd.Map.Height));
    m.Write(i, sizeof(i));
    ctd.UpdateMapProperty(mpResize, m);
  End;
End;

Procedure TForm4.Button10Click(Sender: TObject);
Var
  b: TBuyAble;
  m: TMemoryStream;
Begin
  // Set = Übernehmen der Einstellungen für das Gebäude
  If ListBox1.ItemIndex = -1 Then exit;
  log('TForm4.Button10Click', lltrace);
  b := StringToBuyAble(ListBox1.Items[ListBox1.ItemIndex]);
  b.WaveNum := strtointdef(edit6.text, 1) - 1;
  b.Count := strtointdef(edit7.text, 0);
  m := TMemoryStream.Create;
  m.WriteAnsiString(b.Item);
  m.Write(b.WaveNum, sizeof(b.WaveNum));
  m.Write(b.Count, sizeof(b.Count));
  m.Write(b.Kind, sizeof(b.Kind));
  //  ctd.Map.addBuyable(b.Item, b.WaveNum, b.Count); -- Das macht ctd schon
  ctd.UpdateMapProperty(mpUpdateBuyable, m);
  logleave;
End;

Procedure TForm4.Button11Click(Sender: TObject);
Begin
  // Anfrage Highscores der Karte
  ctd.OnGetHighscores := @form1.OnShowHighScores;
  ctd.RequestHighscores;
End;

Procedure TForm4.Button12Click(Sender: TObject);
Begin
  // Remove last wave
  If high(ctd.Map.Waves) > -1 Then Begin
    SendSetWaveCountTo(high(ctd.Map.Waves));
  End;
End;

Procedure TForm4.Button13Click(Sender: TObject);
Begin
  // Set Damageclass 1
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC1Tex;
    ctd.TransferFile(fTransferFilename, 'dc1.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button14Click(Sender: TObject);
Begin
  // Set Damageclass 2
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC2Tex;
    ctd.TransferFile(fTransferFilename, 'dc2.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button15Click(Sender: TObject);
Begin
  // Set Damageclass 3
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC3Tex;
    ctd.TransferFile(fTransferFilename, 'dc3.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button16Click(Sender: TObject);
Begin
  // Set Damageclass 4
  If OpenPictureDialog1.Execute Then Begin
    If LowerCase(ExtractFileExt(OpenPictureDialog1.FileName)) <> '.png' Then Begin
      showmessage('Sorry only .png files supported, please convert your image first.');
      exit;
    End;
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frDC4Tex;
    ctd.TransferFile(fTransferFilename, 'dc4.png', @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button17Click(Sender: TObject);
Var
  i: integer;
Begin
  i := -1;
  Case QuestionDlg('Question', 'Please select difficulty to create wave with:', mtConfirmation, [mrOK, 'Hard', mrNo, 'Medium', mrYes, 'Easy', 'IsDefault'], '') Of
    mrYes: i := 0;
    mrNo: i := 1;
    mrOK: i := 2;
  End;
  If i In [0..2] Then Begin
    ctd.AddNewRandomWave(i);
  End;
End;

Procedure TForm4.Button18Click(Sender: TObject);
Begin
  // Create Map Texture
  form1.MenuItem29Click(Nil);
End;

Procedure TForm4.Button19Click(Sender: TObject);
Begin
  // Open Hero Editor
  form1.MenuItem31Click(Nil);
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    fTransferFilename := OpenPictureDialog1.FileName;
    fOnTransferFileDoneReason := frBackTex;
    ctd.TransferFile(fTransferFilename, ExtractFileName(fTransferFilename), @OnTransferFileDone);
  End;
End;

Procedure TForm4.Button20Click(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  // Recreate Map by Image
  If OpenPictureDialog1.Execute Then Begin
    m := form1.ImageToMapStream(OpenPictureDialog1.FileName);
    ctd.UpdateMapProperty(mpCoordData, m);
  End;
End;

Procedure TForm4.Button2Click(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  // Clear Background Image
  m := TMemoryStream.Create;
  m.WriteAnsiString('');
  ctd.UpdateMapProperty(mpBackTex, m);
  CheckBox1.Checked := false;
End;

Procedure TForm4.Button4Click(Sender: TObject);
Begin
  // Refresh Placements List
  ctd.GetFileList('*.opp;*.geb', @OnGetComboBox2Content);
End;

Procedure TForm4.Edit1Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  If edit1.text = '' Then exit;
  m := TMemoryStream.Create;
  i := strtoint(edit1.text);
  Edit1.Tag := i;
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpMaxPlayer, m);
End;

Procedure TForm4.Edit2Change(Sender: TObject);
Var
  m: TMemoryStream;
  i: integer;
  e: TEdit absolute Sender;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  If e.Text = '' Then exit;
  i := StrToInt(e.text);
  e.Tag := i;
  m := TMemoryStream.Create;
  i := StrToIntDef(edit2.text, 1);
  m.Write(i, sizeof(i));
  i := StrToIntDef(edit3.text, 1);
  m.Write(i, sizeof(i));
  i := StrToIntDef(edit4.text, 1);
  m.Write(i, sizeof(i));
  ctd.UpdateMapProperty(mpLives, m);
End;

Procedure TForm4.FormShow(Sender: TObject);
Begin
  form4.left := strtoint(GetValue('MapEditorForm', 'Left', inttostr(Form1.Left + Form1.Width)));
  form4.top := strtoint(GetValue('MapEditorForm', 'Top', inttostr(form1.Top)));
  // Mit a bissl Glück reicht das schon für Scallierungen ?
  //form4.Width := strtoint(GetValue('MapEditorForm', 'Width', inttostr(form4.Width)));
  //form4.Height := strtoint(GetValue('MapEditorForm', 'Height', inttostr(form4.Height)));

  Tform(self).Constraints.MaxHeight := Tform(self).Height; // TODO: remove Limit
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width; // TODO: remove Limit
  Tform(self).Constraints.Minwidth := Tform(self).width;
  FixFormPosition(form4);
End;

Procedure TForm4.ListBox1Click(Sender: TObject);
Var
  by: TBuyAble;
Begin
  // Preview Buyable Item
  If ListBox1.ItemIndex = -1 Then exit;
  by := StringToBuyAble(ListBox1.Items[ListBox1.ItemIndex]);
  edit6.text := inttostr(by.WaveNum + 1);
  edit7.text := inttostr(by.Count);
End;

Procedure TForm4.ListBox1DblClick(Sender: TObject);
Var
  by: TBuyAble;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    by := StringToBuyAble(ListBox1.Items[ListBox1.ItemIndex]);
    Case by.Kind Of
      bkBuilding: Begin
          // Öffnen eines Gebäudes im Editor
          fOnFinishedLoadListDuringLoadObj := ExtractFileNameOnly(by.Item);
          Form14.LoadBuildingSettings(@OnFinishedLoadListDuringLoad);
        End;
      bkHero: Begin
          fOnFinishedLoadListDuringLoadObj := ExtractFileNameOnly(by.Item);
          Form14.LoadHeroSettings(@OnFinishedLoadListDuringLoad);
        End;
    Else Begin
        Raise exception.Create('TForm4.ListBox1DblClick: implementieren.');
      End;
    End;
  End;
End;

Procedure TForm4.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var
  h: integer;
  lb: TListBox;
  t: String;
  obj: TItemObject;
Begin
  lb := TListBox(Control);
  h := 0;
  t := lb.Items[index];
  obj := TItemObject(lb.Items.Objects[index]);
  If assigned(obj) Then Begin
    h := ARect.Bottom - ARect.top;
    t := t + ' : ' + obj.Text;
  End;
  lb.Canvas.Pen.Color := lb.Canvas.Brush.Color;
  lb.Canvas.Rectangle(ARect);
  lb.Canvas.TextRect(aRect, ARect.Left + h, (ARect.Bottom + ARect.top - lb.Canvas.TextHeight('8')) Div 2, ' ' + t);
  // Gibt es Meta Infos -> Anzeigen
  If assigned(obj) Then Begin
    lb.Canvas.StretchDraw(rect(ARect.Left, arect.Top, ARect.Left + h, ARect.Bottom), obj.Image);
  End;
End;

Procedure TForm4.Memo1Change(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  If Not assigned(ctd) Or ctd.BlockMapUpdateSending Then exit;
  m := TMemoryStream.Create;
  m.WriteAnsiString(Serialize(memo1.Text));
  ctd.UpdateMapProperty(mpDescription, m);
End;

Procedure TForm4.PageControl2Change(Sender: TObject);
Begin
  // Braucht nicht, weil beim Wechsel zwischen den Opponent Tabs ebenfalls ein Refresh gesendet wird.
  //  RefreshOpponentsClick(Nil);
End;

Procedure TForm4.ScrollBar1Change(Sender: TObject);
Begin
  label7.caption := 'Cursor size: ' + inttostr(ScrollBar1.Position + 1);
End;

Procedure TForm4.ScrollBar2Change(Sender: TObject);
Begin
  label18.caption := 'Cursor size: ' + inttostr((ScrollBar2.Position * 2) + 1);
End;

Procedure TForm4.Timer1Timer(Sender: TObject);
Begin
  If ctd.BlockMapUpdateSending Or Form4updating Then exit;
  timer1.enabled := false; // Auf jeden Abschalten
  If Not assigned(ctd) Then Begin
    exit;
  End;
  Button4.Click; // refresh der Placements liste
  RefreshOpponentsClick(Nil); // Wave Opponents
End;

Procedure TForm4.OnFinishedLoadListDuringLoad(Sender: TObject);
Var
  obj: String;
  j: Integer;
Begin
  obj := fOnFinishedLoadListDuringLoadObj;
  fOnFinishedLoadListDuringLoadObj := '';
  If obj = '' Then exit;
  // Suchen des Eintrages in der Listbox2
  For j := 0 To form14.ListBox2.Items.Count - 1 Do
    If obj = form14.ListBox2.Items[j] Then Begin
      form1.HideForm4;
      Form14.Show;
      form14.ListBox2.ItemIndex := j;
      If form6.Visible Then form6.hide;
      If form7.Visible Then form7.hide;
      If form19.Visible Then form19.hide;
      // Öffnen des Gebäudes
      form14.Button5.Click;
      exit;
    End;
End;

Procedure TForm4.OnGetComboBox2Content(Sender: TObject; Const Data: TStringlist
  );
Var
  i: Integer;
Begin
  // TODO: Hier muss theoretisch auch noch die TItemObject Vorschau mit rein !
  ComboBox2.Sorted := true;
  ComboBox2.Items.Clear;
  For i := 0 To data.Count - 1 Do Begin
    ComboBox2.Items.add(data[i]);
  End;
  //  ComboBox2.Items.Text := Data.Text;

  If ComboBox2.Items.Count <> 0 Then Begin
    ComboBox2.ItemIndex := 0;
  End
  Else Begin
    ComboBox2.Text := '';
  End;
  ComboBox2Change(Nil);
End;

Procedure TForm4.OnGetPageControl2Content(Sender: TObject;
  Const Data: TStringlist);
Var
  f: TWaveFrame;
  i: Integer;
Begin
  For i := 0 To PageControl2.PageCount - 1 Do Begin
    f := (PageControl2.Pages[i].Components[0]) As TWaveFrame;
    f.UpdateOpponents(Data.CommaText);
  End;
End;

Procedure TForm4.OnTransferFileDone(Sender: TObject; Suceed: Boolean);
Var
  m: TMemoryStream;
Begin
  If Suceed Then Begin
    Case fOnTransferFileDoneReason Of
      frBackTex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + ExtractFileName(fTransferFilename))) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          (*
           * KA warum, aber wenn man nicht ein "bisschen" wartet, dann wird der Client nicht richtig aktualisiert..
           * Empirsch gemessen gehts ab ca. 2ms
           *)
          sleep(250);
          m := TMemoryStream.Create;
          m.WriteAnsiString(ExtractFileName(fTransferFilename));
          ctd.UpdateMapProperty(mpBackTex, m);
          CheckBox1.Checked := true; // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC1Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc1.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc1.png');
          ctd.UpdateMapProperty(mpDC1Tex, m);
          Image1.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc1.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC2Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc2.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc2.png');
          ctd.UpdateMapProperty(mpDC2Tex, m);
          Image2.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc2.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC3Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc3.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc3.png');
          ctd.UpdateMapProperty(mpDC3Tex, m);
          Image3.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc3.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End;
      frDC4Tex: Begin
          If Not CopyFile(UTF8ToSys(fTransferFilename), UTF8ToSys(MapFolder + MapName + PathDelim + 'dc4.png')) Then Begin
            logshow(format('Could not copy %s to share folder', [fTransferFilename]), llerror);
            exit;
          End;
          m := TMemoryStream.Create;
          m.WriteAnsiString('dc4.png');
          ctd.UpdateMapProperty(mpDC4Tex, m);
          Image4.Picture.LoadFromFile(MapFolder + MapName + PathDelim + 'dc4.png'); // Update LCL, weil UpdateMapProperty das noch nicht macht !
        End
    Else Begin
        logshow('TForm4.OnTransferFileDone: unknown reason.', llerror);
        exit;
      End;
    End;
  End
  Else Begin
    logshow(format('Could not send %s to map server', [fTransferFilename]), llerror);
  End;
  fOnTransferFileDoneReason := frUnknown;
End;

Procedure TForm4.AdjustMoveWaveButtons;
Var
  i: Integer;
Begin
  For i := 0 To PageControl2.PageCount - 1 Do Begin
    (PageControl2.Pages[i].Components[0] As TWaveFrame).Button3.Visible := i <> (PageControl2.PageCount - 1);
    (PageControl2.Pages[i].Components[0] As TWaveFrame).label1.Visible := (PageControl2.Pages[i].Components[0] As TWaveFrame).Button2.Visible Or (PageControl2.Pages[i].Components[0] As TWaveFrame).Button3.Visible;
  End;
End;

Procedure TForm4.DoUpdateMapProperty(Sender: TObject; MapProperty: Integer;
  SenderUid: Integer; Const Data: TStream);
Var
  i, j, c: Integer;
  s: String;
  b: Boolean;
  by: TBuyAble;
  bk: TBuyAbleKind;
Begin
  (*
   * ! WICHTIG !
   * Hier dürfen nur LCL-Sachen gesetzt werden, fMap wird automatisch beim Empfangen gesetzt !
   *)
  log('TForm4.OnUpdateMapProperty : ' + MessageMapPropertyToString(MapProperty), llTrace);
  Case MapProperty Of
    mpSaveMap,
      mpResize,
      mpCoord,
      mpDelPlacement,
      mpAddPlacement,
      mpBackTex,
      mpWCoord,
      mpCoordData: Begin
        // Nichts, irgend ein Client wollte lediglich dass alle "Speichern"
      End;
    // General
    mpDescription: Begin
        s := Data.ReadAnsiString;
        Memo1.Text := DeSerialize(s);
      End;
    mpMapType: Begin
        i := -1;
        data.Read(i, sizeof(i));
        ComboBox1.ItemIndex := i;
      End;
    mpMaxPlayer: Begin
        i := -1;
        data.Read(i, sizeof(i));
        edit1.text := inttostr(i);
      End;
    mpLives: Begin
        i := -1;
        data.Read(i, sizeof(i));
        edit2.text := inttostr(i);
        i := -1;
        data.Read(i, sizeof(i));
        edit3.text := inttostr(i);
        i := -1;
        data.Read(i, sizeof(i));
        edit4.text := inttostr(i);
      End;
    mpDC1Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          Image1.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          Image1.Picture.Clear;
        End;
      End;
    mpDC2Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          Image2.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          Image2.Picture.Clear;
        End;
      End;
    mpDC3Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          Image3.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          Image3.Picture.Clear;
        End;
      End;
    mpDC4Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          Image4.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          Image4.Picture.Clear;
        End;
      End;
    // Terrain
    // Waypoints
    mpAddWayPoint,
      mpDelWayPoint,
      mpDecPointOrder: Begin
        form4.UpdateLCLWayLen;
      End;
    // Buyables
    mpAddBuyable: Begin
        s := Data.ReadAnsiString;
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        bk := bkBuilding;
        data.read(bk, sizeof(bk));
        by.Item := s;
        by.WaveNum := i;
        by.Count := j;
        by.Kind := bk;
        AddForm4Buyable(by);
      End;
    {mpDelBuyable: Begin
        s := Data.ReadAnsiString;
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        bk := bkBuilding;
        data.read(bk, sizeof(bk));
        by.Item := s;
        by.WaveNum := i;
        by.Count := j;
        by.Kind := bk;
        s := BuyableToString(by);
        For i := 0 To Form4.ListBox1.Items.Count - 1 Do
          If Form4.ListBox1.Items[i] = s Then Begin
            Form4.ListBox1.Items.Delete(i);
            break;
          End;
      End; }
    mpUpdateBuyable: Begin
        s := Data.ReadAnsiString;
        c := -1;
        data.Read(c, sizeof(c));
        j := -1;
        data.Read(j, sizeof(j));
        bk := bkBuilding;
        data.read(bk, sizeof(bk));
        For i := 0 To ListBox1.Items.Count - 1 Do Begin
          by := StringToBuyAble(ListBox1.Items[i]);
          If by.Item = s Then Begin
            by.Item := s;
            by.WaveNum := c;
            by.Count := j;
            by.Kind := bk;
            ListBox1.Items[i] := BuyableToString(by);
            If SenderUid = ctd.OwnServerUid Then Begin
              ListBox1.ItemIndex := i;
            End;
            break;
          End;
        End;
        SortForm4Buyables;
      End;
    // Waves
    mpWaveCount: Begin
        i := -1;
        data.Read(i, sizeof(i));
        SetWaveCountTo(i, SenderUid);
      End;
    mpDelOppInWave: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Free;
        TWaveFrame(PageControl2.Pages[i].Components[0]).FixOpponentNums();
      End;
    mpCashOnWaveStart: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        TWaveFrame(PageControl2.Pages[i].Components[0]).Edit2.Text := inttostr(j);
      End;
    mpWaveHint: Begin
        i := -1;
        data.Read(i, sizeof(i));
        s := data.ReadAnsiString;
        TWaveFrame(PageControl2.Pages[i].Components[0]).Edit1.Text := s;
      End;
    mpWaveOpponents: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        TWaveFrame(PageControl2.Pages[i].Components[0]).SetOppenentCountTo(j, SenderUid);
        RefreshOpponentsClick(Nil);
      End;
    mpWaveOpponent: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        s := Data.ReadAnsiString;
        TWaveOpponentFrame(TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).SetOppentTo(s);
      End;
    mpWaveOpponentCount: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit1.Text := inttostr(c);
      End;
    mpWaveOpponentCashPerUnit: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit2.Text := inttostr(c);
      End;
    mpWaveOpponentUnitsPerSpawn: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit3.Text := inttostr(c);
      End;
    mpWaveOpponentSpawnDelay: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit4.Text := inttostr(c);
      End;
    mpWaveOpponentSpawnDelta: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit5.Text := inttostr(c);
      End
  Else Begin
      LogShow('Form4.OnUpdateMapProperty: missing handler for ' + inttostr(MapProperty), llWarning);
    End;
  End;
  LogLeave;
End;

Procedure TForm4.RefreshOpponentsClick(Sender: TObject);
Begin
  If Form4updating Then exit;
  ctd.GetFileList('*.opp', @OnGetPageControl2Content);
End;

Procedure TForm4.ChangeWaveIndexClick(WaveIndex: integer; Direction: Boolean);
Var
  w1i, w2i: Integer;
Begin
  // Verschieben einer Kompletten Wave entlang der Richtung
  If Direction Then Begin
    w1i := WaveIndex;
    w2i := WaveIndex + 1;
  End
  Else Begin
    w1i := WaveIndex;
    w2i := WaveIndex - 1;
  End;
  ctd.ExchangeWaves(w1i, w2i);
End;

Procedure TForm4.AddWave(Reset: Boolean);
Var
  t: TTabSheet;
  f: TWaveframe;
Begin
  t := PageControl2.AddTabSheet;
  t.caption := 'Wave' + inttostr(PageControl2.PageCount);
  t.name := t.caption;
  f := TWaveFrame.Create(t);
  f.name := 'WaveFrame' + inttostr(PageControl2.PageCount);
  f.Parent := t;
  f.RefreshOppentList := @RefreshOpponentsClick;
  f.ChangeWaveIndex := @ChangeWaveIndexClick;
  f.WaveNum := PageControl2.PageCount - 1;
  If reset Then Begin
    ResetWave(t);
  End;
End;

Procedure TForm4.AddForm4Buyable(b: TBuyAble);
Var
  obj: TItemObject;
  s: String;
  i: Integer;
Begin
  // Buyables werden Sortiert nach Reihenfolge in der die Gebs in den Waves Kaufbar sind
  // Wenn es dann Ding schon gibt nicht mehr adden
  s := BuyableToString(b);
  For i := 0 To ListBox1.items.count - 1 Do Begin
    If ListBox1.items[i] = s Then Begin
      // Über das Update Map Properties kann es sein, das das Objekt schon
      // geladen wurde obwohl es noch nicht "da" ist -> dann wird es nun aktualisiert
      obj := ListBox1.items.Objects[i] As TItemObject;
      If Not assigned(obj) Then Begin
        // TODO: Klären, was da dann gemacht werden muss ..
      End
      Else Begin
        Case b.Kind Of
          bkBuilding: Begin
              obj.LoadGebInfo(MapFolder + MapName + PathDelim + b.Item);
            End;
          bkHero: Begin
              obj.LoadHeroInfo(MapFolder + MapName + PathDelim + b.Item);
            End;
        End;
      End;
      exit;
    End;
  End;
  Case b.Kind Of
    bkBuilding: Begin
        obj := TItemObject.Create;
        obj.LoadGebInfo(MapFolder + MapName + PathDelim + b.Item);
        listbox1.Items.AddObject(s, obj);
      End;
    bkHero: Begin
        obj := TItemObject.Create;
        obj.LoadHeroInfo(MapFolder + MapName + PathDelim + b.Item);
        listbox1.Items.AddObject(s, obj);
      End;
  Else Begin
      listbox1.items.add(s);
    End;
  End;
  SortForm4Buyables;
End;

Procedure TForm4.ResetWave(Const Page: TTabSheet);
Var
  f: TWaveFrame;
Begin
  f := page.Components[0] As TWaveFrame;
  f.Reset;
  //  PageControl2.Height := f.Height;
  //  PageControl2.Width := f.Width;
End;

Procedure TForm4.DelLastWave;
Begin
  PageControl2.Pages[PageControl2.PageCount - 1].free;
End;

End.


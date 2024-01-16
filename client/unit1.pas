(******************************************************************************)
(* config_td                                                       ??.??.???? *)
(*                                                                            *)
(* Version     : see updater_settings.inc                                     *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : see uctd_common.pas                                          *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils, FileUtil, OpenGLContext, lNetComponents, Forms, Controls,
  Graphics, Dialogs, Menus, ExtCtrls, StdCtrls,
  dglOpenGL, // http://wiki.delphigl.com/index.php/dglOpenGL.pas (innerhalb der .pas datei muss am Anfang stehen  {$HINTS off} und am Ende  {$HINTS on}
  uctd_common, uctd, uopengl_widgetset, uwave_frame, uwave_oppenent_frame,
  types, uupdate;

{$IFDEF AUTOMODE}
Const
  AM_Idle = 0; // Nichts
  AM_StartClient = 1; // Wenn Parameter -d übergeben wird, dann wird automatisch als client auf 127.0.0.1 mit User "Client" verbunden.
{$ENDIF}

Type

{$IFDEF AUTOMODE}
  TAutomodeData = Record
    State: integer;
  End;
{$ENDIF}

  TNewMapRecord = Record
    w, h: Integer;
    n: String;
  End;

  (*
   * Alle Nachrichten, die aus dem OnPaint des OpenGLControl heraus erzeugt werden
   * Können durch die LCL nicht angezeigt werden.
   * Also müssen Sie auf den "OnIdle" umgeleitet werden.
   *)
  TUserMessages = Record
    Msg: String;
    WarnLevel: TLogLevel;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    LTCPComponent1: TLTCPComponent;
    LUDPComponent1: TLUDPComponent;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Timer2: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem15Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem18Click(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure MenuItem20Click(Sender: TObject);
    Procedure MenuItem22Click(Sender: TObject);
    Procedure MenuItem23Click(Sender: TObject);
    Procedure MenuItem24Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem28Click(Sender: TObject);
    Procedure MenuItem29Click(Sender: TObject);
    Procedure MenuItem31Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControl1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
    fUpdater: TUpdater;
    fUserMessages: Array Of TUserMessages;
    fnmr: TNewMapRecord;
    fMapTransferStream: TMemorystream;
    Procedure CloseAllModalWindows;
    Procedure Form10GetMapListEvent(Sender: TObject; Const Data: TStringlist);
    Procedure Form1NewMapEvent(Sender: TObject; Const Data: TStringlist);
    Procedure Form1GetSavegamesforLoadEvent(Sender: TObject; Const Data: TStream);
    Procedure Form1GetSavegamesforSaveEvent(Sender: TObject; Const Data: TStream);
    Procedure StartHostGame;
    Procedure StartClientGame;
    Procedure OnIdle(Sender: TObject; Var Done: Boolean);
    Procedure OnShowGameStatistics(Msg: String; Const Data: TStream);
    Procedure UpdateResultCallback(AlwaysShowResult: Boolean;
      OnlineVersions: TVersionArray);
  public
    { public declarations }
    Procedure CheckForNewVersion(AlwaysShowResult: Boolean);
    Procedure OnConnectToServer(Sender: TObject);
    Procedure OnDisconnectFromServer(Sender: TObject);
    Procedure OnLoadMap(Sender: TObject);
    Procedure OnUpdateMapProperty(Sender: TObject; MapProperty: Integer; Const Data: TStream);
    Procedure OnStartRound(Sender: TObject);
    Procedure OnEndRound(Sender: TObject; Succeed: Boolean; Round_: Integer);
    Procedure OnForceEditMode(Sender: TObject);
    Procedure OnHandleLoadGameingData(Sender: TObject);
    Procedure OnRefreshPlayerStats(Sendet: TObject);
    Function SaveAndCheckMap(ShowWarnings: Boolean; ShowErrors: Boolean = True): Boolean;
    Procedure AddUserMessage(Msg: String; WarnLevel: TLogLevel);
    Procedure OnShowHighScores(Sender: TObject; Const Data: Tstream);

    Procedure Load_CT_Settings;

    (*
     * ggf sollte das nicht nur auf form4 bezogen sondern auf alles bezogen werden, dann bit einem Stack und hide all expect ?
     *)
    Procedure HideForm4;
    Procedure RestoreForm4;

  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  ShowFPS: Boolean = false;
  FPS_Counter, LastFPS_Counter: integer;
  LastFPSTime: int64;
  ServerMapFolder: String;
{$IFDEF AUTOMODE}
  AutomodeData: TAutomodeData;
{$ENDIF}

Procedure ShowUserMessage(Msg: String; WarnLevel: TLogLevel);

Implementation

{$R *.lfm}

Uses
  process, UTF8Process, lazutf8, lazfileutils, lclintf, LCLType
{$IFDEF Windows}
  , windows // Für Konsolenmodus zur Laufzeit
{$ENDIF}
  , uOpenGL_ASCII_Font
  , ucolorselectbox
  , uctd_messages, uctd_map
  , unit2 // Host Join Dialog
  , unit3 // Select Map Size Dialog (New Map)
  , unit4 // Karten Eigenschaften Dialog
  , unit5 // Optionen
  , Unit6 // Building Editor
  , unit7 // Opponent Editor
  , unit8 // Game Statistiks
  , unit9 // Map Highscores
  , unit10 // Load Map Dialog
  , unit11 // New Game Dialog mit Spieler Plazierungs auswahl
  , unit12 // Player Statistics
  , unit13 // Chat
  , unit14 // Building / Oppenent Übersicht Global / Local
  , unit15 // Abfrage beim copieren von Opponents / Gebäuden in Unit14
  , unit16 // Savegame Dialog
  , unit17 // Map Texture Generator Dialog
  , unit18 // Asynchrone Messagebox
  //, unit19 // Hero Editor
  ;

Var
  allowcnt: Integer = 0;
  Form1ShowOnce: Boolean = false;
  DefFormat: TFormatSettings;

Procedure ShowUserMessage(Msg: String; WarnLevel: TLogLevel);
Begin
  form1.AddUserMessage(msg, WarnLevel);
End;

Function GetWorkDir(Out Directory: String): Boolean;
Begin
  result := false;
  Try
    Directory := IncludeTrailingPathDelimiter(GetTempDir(false)) + 'CTD_Temp';
    If Not DirectoryExistsUTF8(Directory) Then Begin
      If Not ForceDirectoriesUTF8(Directory) Then Begin
        showmessage(format('Error could not create: %s', [Directory]));
        exit;
      End;
    End;
    Directory := IncludeTrailingPathDelimiter(Directory);
    // Todo: Prüfen ob man hier tatsächlich Schreibrechte hat, Wenn nicht Knallts aber sowieso ;)
    result := true;
  Except
  End;
End;

{ TForm1 }

Procedure TForm1.CheckForNewVersion(AlwaysShowResult: Boolean);
Begin
  If URL_CheckForUpdate = '' Then exit; // Feature atm disabled -> exit
  //  fUpdater.ProxyHost := getvalue('General', 'ProxyHost', '');
  //  fUpdater.ProxyPort := getvalue('General', 'ProxyPort', '');
  //  fUpdater.ProxyUser := getvalue('General', 'ProxyUser', '');
  //  fUpdater.ProxyPass := getvalue('General', 'ProxyPass', '');
  fUpdater.GetVersions(URL_CheckForUpdate, AlwaysShowResult, @UpdateResultCallback);
End;

Procedure TForm1.UpdateResultCallback(AlwaysShowResult: Boolean;
  OnlineVersions: TVersionArray);
Var
  ReleaseText, Dir: String;
  Ver: TVersion;
  i: Integer;
Begin
  If Not assigned(OnlineVersions) Then Begin
{$IFDEF Linux}
    ctd.VersionInfoString := 'Error, could not download infos, did you install libssldev ?' + LineEnding + LineEnding +
      'sudo apt-get install libssl-dev';
{$ELSE}
    ctd.VersionInfoString := fUpdater.LastError;
{$ENDIF}
    exit;
  End;
  // Suchen der Version die zu uns gehört
  ver.name := '';
  For i := 0 To high(OnlineVersions) Do Begin
    If OnlineVersions[i].Name = updater_AppName Then Begin
      ver := OnlineVersions[i];
      break;
    End;
  End;
  If ver.name = '' Then Begin
    ctd.VersionInfoString := 'Could not download valid version informations.';
    exit;
  End;
  If strtofloat(ver.Version, DefFormat) > strtofloat(updater_Version, DefFormat) Then Begin
    ReleaseText := ver.ReleaseText;
    If ID_YES = application.MessageBox(pchar(format(RF_VersionInfo, [updater_Version, ver.Version, ReleaseText])), 'Information', MB_YESNO Or MB_ICONINFORMATION) Then Begin
      If Not GetWorkDir(dir) Then Begin
        ctd.VersionInfoString := 'Unable to create temporary folder.';
      End
      Else Begin
        self.Enabled := false;
        // Proaktiv schon mal so viel wie möglich abschalten
        ctd.DisConnect;
        Timer2.Enabled := false;
        timer1.Enabled := false;
        form18.timer1.enabled := true; // So tun wie wenn was passieren würde ..
        form18.Show; // Dem User Anzeigen dass wir nun Downloaden
        If fUpdater.DoUpdate_Part1(dir, ver) Then Begin
          form18.timer1.enabled := false; // So tun wie wenn was passieren würde ..
          self.Enabled := true;
        End
        Else Begin
          ctd.VersionInfoString := format(
            'An error occured, the update process is not finished correct:' + LineEnding + LineEnding +
            '%s' + LineEnding + LineEnding +
            'Please retry by hand (or retry with admin rights).', [fUpdater.LastError]);
          self.Enabled := true;
        End;
        // Egal ob mit oder ohne Fehler wir gehen aus !
        close; // Wenn das auch nicht geht, dann hilft hier nur noch ein Halt :/
      End;
    End;
  End
  Else Begin
    ctd.VersionInfoString := 'Your version is up to date.';
  End;
End;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    DefaultFormatSettings.DecimalSeparator := '.'; // Eigentlich brauchts das nur 1 mal, aber anscheinend bringt der Startup Code da manchmal was durcheinander.
    Create_ASCII_Font;
    glenable(GL_TEXTURE_2D); // Texturen
    glDepthFunc(gl_less);
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glBlendFunc(gl_one, GL_ONE_MINUS_SRC_ALPHA);

    // Sorgt dafür, dass Voll Transparente Pixel nicht in den Tiefenpuffer Schreiben.
    (*
     * Das ist so gedreht, damit der Benutzer ein "initializing" sieht
     *)
    FPS_Counter := 0;
    Initialized := True; // Der Anwendung erlauben zu Rendern (bzw. damit das onResize Funktioniert)
    // Splasch Screen das wir alles laden
    OpenGLControl1Resize(Nil);
    WidgetSetGo2d(OpenGLControl1.Width, OpenGLControl1.Height);
    OpenGL_ASCII_Font.Color := clred;
    CenterTextOut(OpenGLControl1.Width, OpenGLControl1.Height, 'Initialising...');
    WidgetSetExit2d();
    OpenGLControl1.SwapBuffers;
    // Das Game Initialisieren
    LastFPS_Counter := 0;
    LastFPSTime := GetTick();
    ctd.Initialize(OpenGLControl1);
    ctd.OnHostButtonClick := @MenuItem3Click;
    ctd.OnJoinButtonClick := @MenuItem4Click;
    ctd.OnNewMapButtonClick := @MenuItem7Click;
    ctd.OnLoadMapButtonClick := @MenuItem8Click;
    ctd.OnLoadGameButtonClick := @MenuItem9Click;
{$IFDEF Windows}
    // Unter Windoof funktionieren sonst die Tasten nicht.
    form1.OnKeyDown := OpenGLControl1.OnKeyDown;
    form1.OnKeyUp := OpenGLControl1.OnKeyUp;
{$ENDIF}
    ctd.OnConnectToServer := @OnConnectToServer;
    ctd.OnDisconnectFromServer := @OnDisconnectFromServer;
    ctd.OnLoadMap := @OnLoadMap;
    ctd.OnUpdateMapProperty := @OnUpdateMapProperty;
    ctd.OnStartRound := @OnStartRound;
    ctd.OnEndRound := @OnEndRound;
    ctd.OnForceEditMode := @OnForceEditMode;
    ctd.OnHandleLoadGameingData := @OnHandleLoadGameingData;
    ctd.OnRefreshPlayerStats := @OnRefreshPlayerStats;
    ctd.OnShowGameStatistics := @OnShowGameStatistics;
    ctd.OnWaveCloneEvent := @form4.OnCTDWaveClone;

    ctd.RegisterTCPConnection(LTCPComponent1);
    ctd.RegisterUDPConnection(LUDPComponent1);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  If (GetValue('Global', 'Autozoom', '1') = '1') Then Begin
    ctd.Zoom(True);
  End;
End;

Procedure TForm1.OpenGLControl1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  If (GetValue('Global', 'Autozoom', '1') = '1') Then Begin
    ctd.Zoom(false);
  End;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  s: String;
  offset: integer;
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glcolor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  ctd.Render(OpenGLControl1.Width, OpenGLControl1.Height);
  Go2d(OpenGLControl1.Width, OpenGLControl1.Height);
  glBindTexture(GL_TEXTURE_2D, 0);
  OpenGL_ASCII_Font.Color := clwhite;
  glTranslatef(0, 0, 0.95);
  If ShowFPS Then Begin
    s := 'FPS : ' + inttostr(LastFPS_Counter);
    offset := 0;
    If (ctd.GameState = gs_Gaming) Then Begin
      offset := 64;
    End;
    OpenGL_ASCII_Font.Textout(5, 5 + offset, s);
  End;
  Exit2d();
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
  gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Var
  w, h: integer;
  n: String;
Begin
  // New Map
  log('TForm1.MenuItem7Click', llTrace);
  //  timer2.enabled := false;
  form3.caption := 'New map...';
  form3.Edit3.Visible := true;
  form3.Label3.Visible := true;
  Form3.Button3.Visible := true;
  Form3Filename := '';
  form3.ModalResult := mrNone;
  form3.Edit1.Text := '50';
  form3.Edit2.Text := '50';
  form3.Edit3.Text := 'New Map';
  HideForm4;
  If form3.ShowModal = mrOK Then Begin
    w := strtoint(form3.edit1.text);
    h := strtoint(form3.edit2.text);
    n := form3.Edit3.Text;
    // 1. Rudimentärcheck
    If (w <= 0) Or (h <= 0) Or (n = '') Then Begin
      LogShow('Map invalid settings.', llError);
      RestoreForm4;
      LogLeave;
      exit;
    End;
    // 2. Prüfen ob Kartenname bereits existiert => Anlegen der Karte
    fnmr.w := w;
    fnmr.h := h;
    fnmr.n := n;
    ctd.getMapList(@Form1NewMapEvent);
  End;
  RestoreForm4;
  LogLeave;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  // Load Map
  log('TForm1.MenuItem8Click', llTrace);
  ctd.getMapList(@Form10GetMapListEvent);
  HideForm4;
  LogLeave;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Begin
  // Load game
  HideForm4;
  SaveAndCheckMap(false, false); // Speichern der Karte
  form16.ModalResult := mrCancel; // Das Formular schließen, falls es gerade offen ist..
  ctd.GetSavegames(@Form1GetSavegamesforLoadEvent);
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If ssalt In shift Then Begin
    If assigned(ctd.Map) Then Begin
      ctd.ShowLifepoints := Not (GetValue('Global', 'AlwaysShowLifepoints', '0') = '1');
    End;
  End;
  // Open Chat
  If key = VK_RETURN Then Begin
    If Not ctd.IsInEditMode Then Begin
      MenuItem19Click(Nil);
    End;
  End;
End;

Procedure TForm1.OpenGLControl1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Not assigned(ctd) Then exit;
  If Not (ssalt In shift) Then Begin // -- Eigentlich sollte es mit dem Alt funktionieren, ka was da verdreht ist
    If assigned(ctd.Map) Then Begin
      ctd.ShowLifepoints := GetValue('Global', 'AlwaysShowLifepoints', '0') = '1';
    End;
  End;
  ctd.ShowPlayerStartNames := false;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
  t: int64;
{$ENDIF}
Begin
  If Initialized Then Begin
    inc(FPS_Counter);
    t := GetTick();
{$IFDEF WinXPMode}
    If LastFPSTime + 1000 <= t Then Begin
      LastFPSTime := t;
{$ELSE}
    If LastFPSTime + 1000 <= t Then Begin
      LastFPSTime := t;
{$ENDIF}
      LastFPS_Counter := FPS_Counter;
      FPS_Counter := 0;
    End;
    OpenGLControl1.DoOnPaint;
    // OpenGLControl1.Invalidate; -- Warum auch immer das nicht mehr geht ??
{$IFDEF DebuggMode}
    i := glGetError();
    If i <> 0 Then Begin
      Timer1.Enabled := false;
      p := gluErrorString(i);
      logshow('OpenGL Error (' + inttostr(i) + ') occured.' + #13#13 +
        'OpenGL Message : "' + p + '"'#13#13 +
        'Applikation will be terminated.', llFatal);
      close;
    End;
{$ENDIF}
  End;
End;

Procedure TForm1.Timer2Timer(Sender: TObject);
Begin
  // Der Timer, welcher automatisch das Spiel startet, nachdem die Karte geladen wurde.
  If MenuItem18.Enabled Then Begin
    timer2.Enabled := false;
    MenuItem18.Click;
  End;
End;

Procedure TForm1.Form10GetMapListEvent(Sender: TObject; Const Data: TStringlist
  );
Begin
  Form10MapList := TStringList.Create;
  Form10MapList.text := data.text;
  If Form10MapList.Count <> 0 Then Begin
    form10.BeginUpdate;
    form10.Edit1.Text := getvalue('Global', 'minMapPlayer', '1');
    form10.Edit2.Text := getvalue('Global', 'maxMapPlayer', '99');
    form10.CheckBox1.Checked := getvalue('Global', 'MapsSingle', '1') = '1';
    form10.CheckBox2.Checked := getvalue('Global', 'MapsCoop', '1') = '1';
    form10.CheckBox3.Checked := getvalue('Global', 'MapsSingleMaze', '1') = '1';
    form10.CheckBox4.Checked := getvalue('Global', 'MapsCoopMaze', '1') = '1';
    form10.EndUpdate;
    form10.show;
  End
  Else Begin
    RestoreForm4;
    Form10MapList.free;
    Form10MapList := Nil;
    LogShow('Server has no levels to load.', llInfo);
  End;
End;

Procedure TForm1.CloseAllModalWindows;
Begin
  (*
   * Eine Liste aller Modalen Fenster die es gibt und deren Schließen !
   *)
  If form2.visible Then form2.Button2.Click; // Host / Join Dialog
  If form3.visible Then form3.Button2.Click; // Map Size
  If form9.Visible Then form9.Button1.Click; // Map Highscore
  If form11.Visible Then form11.Button4.Click; // New Game Dialog
  If form15.Visible Then form15.Button1.Click; // Neuer Name für ein Gebäude / Opp
  If form16.Visible Then form16.Button2.Click; // Savegame Dialog
  If form17.Visible Then form17.Button1.Click; // Map texture Dialog
End;

Procedure TForm1.Form1NewMapEvent(Sender: TObject; Const Data: TStringlist);
Var
  b: Graphics.Tbitmap;
  p: TPortableNetworkGraphic;
  d, i, j: Integer;
  c: TColor;
Begin
  log('TForm1.Form1NewMapEvent', llTrace);
  // 2.2 Eigentliche Prüfung auf Doppelten Namen
  For i := 0 To Data.Count - 1 Do
    If lowercase(fnmr.n) = lowercase(copy(Data[i], 1, pos(':', Data[i]) - 1)) Then Begin
      LogShow('Map already exists.', llError);
      LogLeave;
      exit;
    End;
  // 3. Senden Befehl zum Anlegen und der Server fordert uns dann automatisch zum laden auf.
  ctd.NewMap(fnmr.w, fnmr.h, fnmr.n);
  If Form3Filename <> '' Then Begin
    If lowercase(ExtractFileExt(Form3Filename)) = '.png' Then Begin
      p := TPortableNetworkGraphic.Create;
      p.LoadFromFile(Form3Filename);
      b := Graphics.TBitmap.Create;
      b.Width := p.Width;
      b.Height := p.Height;
      For i := 0 To b.Width - 1 Do Begin
        For j := 0 To b.Height - 1 Do Begin
          b.canvas.Pixels[i, j] := p.Canvas.Pixels[i, j];
        End;
      End;
      p.free;
    End
    Else Begin
      b := Graphics.Tbitmap.create;
      b.LoadFromFile(Form3Filename);
    End;
    // Importieren der Kartendaten Local
    fMapTransferStream.clear;
    i := b.Width;
    fMapTransferStream.Write(i, sizeof(i));
    i := b.Height;
    fMapTransferStream.Write(i, sizeof(i));
    For i := 0 To b.Width - 1 Do Begin
      For j := 0 To b.Height - 1 Do Begin
        // Rot     = Gar nichts
        // Schwarz = nur Begehbar
        // Weiß    = nur Bebaubar
        // Grau    = Begehbar und Bebaubar
        c := b.Canvas.Pixels[i, j];
        d := 0;
        If c <> clred Then Begin
          If Not (c = clwhite) Then Begin
            d := d Or Begehbar;
          End;
          If Not (c = clBlack) Then Begin
            d := d Or Bebaubar;
          End;
        End;
        fMapTransferStream.write(d, sizeof(d));
      End;
    End;
    b.free;
  End;
  LogLeave;
End;

Procedure TForm1.Form1GetSavegamesforLoadEvent(Sender: TObject;
  Const Data: TStream);
Begin
  form16.caption := 'Load game:';
  form16.Button1.caption := 'Load';
  form16.Edit1.Visible := false;
  form16.CheckBox1.Visible := true;
  form16.LoadList(data);
  form16.CheckBox1.Checked := false;
  form16.CheckBox1.OnChange(Nil);
  If form16.ShowModal = mrOK Then Begin
    If Form16.ListBox1.ItemIndex <> -1 Then Begin
      ctd.LoadGame(Form16.ListBox1.Items[Form16.ListBox1.ItemIndex]);
    End;
  End;
  RestoreForm4;
End;

Procedure TForm1.Form1GetSavegamesforSaveEvent(Sender: TObject;
  Const Data: TStream);
Begin
  form16.caption := 'Enter name:';
  form16.Button1.caption := 'Save';
  form16.Edit1.Visible := true;
  form16.Edit1.Text := '';
  form16.CheckBox1.Visible := false;
  form16.LoadList(data);
  // Beim Speichern müssen alle angezeigt werden, damit man die nicht ausversehen überschreibt
  form16.CheckBox1.Checked := true;
  form16.CheckBox1.OnChange(Nil);
  If form16.ShowModal = mrOK Then Begin
    ctd.SaveGame(form16.Edit1.Text);
  End;
  RestoreForm4;
End;

Procedure TForm1.StartClientGame;
Begin
  log('TForm1.StartClientGame', llTrace);
  If Not ctd.Join(form2.ComboBox1.text, strtoint(form2.Edit1.Text), form2.Edit2.Text, form2.edit3.text) Then Begin
    logshow('Error while joining session, could not connect to :' + form2.ComboBox1.text, llError);
  End;
  LogLeave;
End;

Procedure TForm1.Load_CT_Settings;
Begin
  fontscale := strtoint(getvalue('Global', 'Fontscale', '10')) / 10 + 1;
  ctd.InvertMouseScrolling := GetValue('Global', 'InvertMouseScrolling', '0') = '1';
  MenuItem12.Checked := GetValue('Global', 'ShowGrid', '0') = '1';
  ctd.ShowGrid := MenuItem12.Checked;
  ctd.HintShowBuildingRange := GetValue('Global', 'ShowBuildingRanges', '1') = '1';
  ctd.HintShowHeroRange := GetValue('Global', 'ShowHeroRanges', '1') = '1';
  ctd.ShowLifepoints := GetValue('Global', 'AlwaysShowLifepoints', '0') = '1';
  ctd.DarkOtherBuildings := GetValue('Global', 'Building_Darkning', '1') = '1';
  ctd.UseMiddleMouseButtonToScroll := GetValue('Global', 'Middle_Mouse_Map_Scolling', '0') = '1';
  ShowFPS := getValue('Global', 'ShowFPS', '0') = '1';
  MapBlockSize := strtointdef(getValue('Global', 'MapBlockSize', inttostr(MapBlockSize)), MapBlockSize);
  Good_Col := StringToColor(GetValue('Global', 'Good_Color', ColorToString(Good_Col)));
  Middle_Col := StringToColor(GetValue('Global', 'Damages_Color', ColorToString(Middle_Col)));
  Bad_col := StringToColor(GetValue('Global', 'Broken_Color', ColorToString(Bad_col)));
  Nichts_Col := StringToColor(GetValue('Global', 'Nothing_Color', ColorToString(Nichts_Col)));
  Begehbar_col := StringToColor(GetValue('Global', 'Walkable_Color', ColorToString(Begehbar_col)));
  Bebaubar_col := StringToColor(GetValue('Global', 'Buildable_Color', ColorToString(Bebaubar_col)));
  Beides_Col := StringToColor(GetValue('Global', 'Both_Color', ColorToString(Beides_Col)));
  Grid_color := StringToColor(GetValue('Global', 'Grid_Color', ColorToString(Grid_color)));
  Building_Range_Col := StringToColor(GetValue('Global', 'BuildingRange_Color', ColorToString(Building_Range_Col)));
  If assigned(ctd.map) Then Begin // Die Farben in der Map übernehmen
    ctd.Map.RefreshBackTex;
    If Not form4.Visible Then
      ctd.Map.ShowBackTex := Not (GetValue('Global', 'DisableBackGroundTexturing', '0') = '1');
  End;
  Case GetValue('Global', 'Menupos', 'Right') Of
    'Bottom': ctd.MenuPosition := mpBottom;
    'Right': ctd.MenuPosition := mpRight;
  End;
  ctd.ShowBuildableTilesDuringBuild := getValue('Global', 'ShowBuildableTilesDuringBuild', '0') = '1';
  ctd.Resize; // Notwendig dass die fDashSeparator variable richtig initialisiert wird
End;


Procedure TForm1.OnIdle(Sender: TObject; Var Done: Boolean);
Var
  i: Integer;
  msg: String;
Begin
{$IFDEF AUTOMODE}
  Case AutomodeData.State Of
    AM_Idle: Begin
        // Nichts zu tun ..
      End;
    AM_StartClient: Begin
        If Initialized Then Begin
          AutomodeData.State := AM_Idle;
          form2.Edit1.Text := GetValue('Global', 'Joinport', '9876');
          form2.Edit2.Text := GetValue('Global', 'Joinpassword', '');
          form2.Edit3.Text := GetValue('Global', 'Joinusername', 'Player2');
          form2.ComboBox1.text := GetValue('Global', 'Joinip', '127.0.0.1');
          Text := 'Client';
          form1.Left := 0;
          StartClientGame;
        End;
      End;
  End;
{$ENDIF}
  // Anzeigen der Usernachrichten, welche aus OpenGLControl.OnPaint kommen
  While Length(fUserMessages) <> 0 Do Begin
    // LogShow(fUserMessages[0].Msg, fUserMessages[0].WarnLevel); -- Das würde eine EndlosRekursion geben ...
    // Weiterleiten an den Logger
    log(fUserMessages[0].Msg, fUserMessages[0].WarnLevel);
    // Merken der Nachricht für die Anzeige nachher
    msg := LogLevelToString(fUserMessages[0].WarnLevel) + ' : ' + fUserMessages[0].Msg;
    // Entfernen aus der Merkliste
    For i := 0 To high(fUserMessages) - 1 Do Begin
      fUserMessages[i] := fUserMessages[i + 1];
    End;
    setlength(fUserMessages, high(fUserMessages));
    // Anzeigen der Nachricht
    ShowMessage(msg); // Unter Windows ist das nicht Blockend, deswegen muss die Nachricht gelöscht werden bevor sie angezeigt wird
  End;
End;

Procedure TForm1.StartHostGame;
Var
  serv: String;
  p: TProcessUTF8;
Begin
  log('TForm1.StartHostGame', llTrace);
  // Starten des CTD_servers, dann als Client verbinden
  serv := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'ctd_server';
{$IFDEF Windows}
  serv := serv + '.exe';
{$ENDIF}
  If FileExistsUTF8(serv) Then Begin
    p := TProcessUTF8.Create(Nil);
    p.Options := [poNewConsole];
    p.Executable := serv;
    p.Parameters.Add('-p');
    p.Parameters.Add(form2.Edit1.Text);
    If form2.Edit2.Text <> '' Then Begin
      p.Parameters.Add('-pw');
      p.Parameters.Add(form2.Edit2.Text);
    End;
    If trim(ServerMapFolder) <> '' Then Begin
      p.Parameters.Add('-m');
      p.Parameters.Add(ServerMapFolder);
    End;
    p.Parameters.Add('-l');
    p.Parameters.Add(IntToStr(GetLoggerLoglevel()));
    p.Parameters.Add('-f');
    p.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'logs' + PathDelim + 'server.log');
    p.Execute;
    p.free;
  End
  Else Begin
    LogShow('Error: could not find server application, abort now', llError);
    LogLeave;
    exit;
  End;
  sleep(1000); // Bis der Server Steht dauerts ein bischen, also warten wir
  StartClientGame;
  Application.BringToFront;
  LogLeave;
End;

Procedure TForm1.OnConnectToServer(Sender: TObject);
Begin
  log('TForm1.OnConnectToServer', lltrace);
  MenuItem7.Enabled := True; // New Map
  MenuItem8.Enabled := True; // Load Map
  MenuItem9.Enabled := True; // Load game
  MenuItem19.Enabled := True; // Open Chat
  LogLeave;
End;

Procedure TForm1.OnDisconnectFromServer(Sender: TObject);
Begin
  log('TForm1.OnDisconnectFromServer', lltrace);
  caption := defCaption;
  MenuItem7.Enabled := false; // New Map
  MenuItem8.Enabled := false; // Load Map
  MenuItem18.Enabled := false; // Start / Restart game
  MenuItem19.Enabled := false; // Open Chat
  MenuItem20.enabled := false; // Transfer
  //  MenuItem23.enabled := false; // Restart last Wave
  MenuItem24.enabled := false; // Continue Game
  MenuItem25.Enabled := false; // Abort Round
  MenuItem27.enabled := false; // Save Game
  MenuItem29.enabled := false; // Map Tex Editor

  // Form2 = Connect Dialog
  If form3.Visible Then form3.Close; // Select Map Size Dialog (New Map)
  If form4.Visible Then form4.Close; // Map Editor Dialog
  // Form5 = Optionen
  If form6.Visible Then form6.Close; // Building Editor
  If form7.Visible Then form7.Close; // Opponent Editor
  If form8.Visible Then form8.Close; // Game Statistiks
  If form9.Visible Then form9.Close; // Map Highscores
  If form10.Visible Then form10.ModalResult := mrCancel; // Load Map Dialog
  If form11.Visible Then form11.ModalResult := mrCancel; // New Game Dialog mit Spieler Plazierungs auswahl
  If form12.Visible Then form12.close; // Player infos
  If form13.Visible Then form13.close; // Chat Dialog
  If form14.Visible Then form14.close; // Building / Oppenent Übersicht Global / Local
  If form15.Visible Then form15.close; // Abfrage beim copieren von Opponents / Gebäuden in Unit14
  If form16.Visible Then form16.close; // Savegame Dialog
  If form17.Visible Then form17.close; // Map Textrure Generator Dialog

  LogLeave;
End;

Procedure TForm1.OnLoadMap(Sender: TObject);
Var
  i: Integer;
  f: TWaveFrame;
  m: TMemoryStream;
  b: Boolean;
  s: String;
Begin
  // Wenn eine Neue Karte geladen wurde
  log('TForm1.OnLoadMap', lltrace);
  caption := defCaption + ' : ' + MapName;
  form4.caption := 'Map Editor : ' + MapName;
  b := ctd.BlockMapUpdateSending;
  ctd.BlockMapUpdateSending := true; // Unterdrücken dass die LCL Komponenten irgendwelche Refreshes zum Server Senden

  // Laden aller Kartenparameter aus CTD.Map

  // --- Karte General ---
  form4.Memo1.Text := DeSerialize(ctd.map.Description);
  form4.ComboBox1.ItemIndex := MapTypeToInt(ctd.Map.MapType);
  form4.edit1.text := inttostr(ctd.Map.MaxPlayer);
  form4.Edit2.Text := IntToStr(ctd.Map.Lives[0]);
  form4.Edit3.Text := IntToStr(ctd.Map.Lives[1]);
  form4.Edit4.Text := IntToStr(ctd.Map.Lives[2]);
  s := MapFolder + MapName + PathDelim + ctd.Map.DamageClass1Tex;
  If FileExistsUTF8(s) Then Begin
    form4.Image1.Picture.LoadFromFile(s);
  End
  Else Begin
    form4.Image1.Picture.Clear;
  End;
  s := MapFolder + MapName + PathDelim + ctd.Map.DamageClass2Tex;
  If FileExistsUTF8(s) Then Begin
    form4.Image2.Picture.LoadFromFile(s);
  End
  Else Begin
    form4.Image2.Picture.Clear;
  End;
  s := MapFolder + MapName + PathDelim + ctd.Map.DamageClass3Tex;
  If FileExistsUTF8(s) Then Begin
    form4.Image3.Picture.LoadFromFile(s);
  End
  Else Begin
    form4.Image3.Picture.Clear;
  End;
  s := MapFolder + MapName + PathDelim + ctd.Map.DamageClass4Tex;
  If FileExistsUTF8(s) Then Begin
    form4.Image4.Picture.LoadFromFile(s);
  End
  Else Begin
    form4.Image4.Picture.Clear;
  End;
  // --- Buyables ---
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

  // --- Waves ---
  form4.SetWaveCountTo(high(ctd.Map.Waves) + 1);
  Form4updating := true; // Das Laden einer Wave fragt die Opponentliste ab, das darf aber nur 1 mal gemacht werden, und da macht es der Timer, also wird das laden hier geblockt.
  For i := 0 To high(ctd.map.waves) Do Begin
    f := form4.PageControl2.pages[i].Components[0] As twaveFrame;
    f.LoadWave(ctd.Map.Waves[i]);
  End;
  Form4updating := false;

  ctd.BlockMapUpdateSending := b; // Das Aktualisieren zum Server wieder freischalten
  // starten der Aufgaben, welche nicht jetzt sondern erst später erledigt werden können
  form4.Timer1.Enabled := true;

  If fMapTransferStream.Size <> 0 Then Begin
    m := TMemoryStream.Create;
    fMapTransferStream.Position := 0;
    m.CopyFrom(fMapTransferStream, fMapTransferStream.Size);
    ctd.TransferCompleteMapTerrain(m);
    fMapTransferStream.Clear;
  End;
  form4.CheckBox1.Checked := ctd.Map.BackTex <> '';
  RestoreForm4();
  Form4.CheckBox1Change(Nil); // ggf Anzeigen der MapBacktex
  MenuItem18.Enabled := true; // Start / Restart game
  MenuItem20.enabled := true; // Transfer
  MenuItem23.enabled := true; // Restart last Wave
  MenuItem24.enabled := true; // Continue Wave
  MenuItem25.enabled := true; // Abort Wave
  MenuItem27.enabled := true; // Save game
  MenuItem29.enabled := true; // Map Tex Editor

  LogLeave;
End;

Procedure TForm1.OnUpdateMapProperty(Sender: TObject; MapProperty: Integer;
  Const Data: TStream);
Var
  s: String;
  i, j, c: integer;
  by: TBuyAble;
  b: Boolean;
  bk: TBuyAbleKind;
Begin
  log('TForm1.OnUpdateMapProperty : ' + MessageMapPropertyToString(MapProperty), llTrace);
  b := ctd.BlockMapUpdateSending;
  ctd.BlockMapUpdateSending := true;
  Case MapProperty Of
    mpDelOppInWave: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        ctd.Map.DelOppInWave(i, j);
        TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Free;
        TWaveFrame(form4.PageControl2.Pages[i].Components[0]).FixOpponentNums();
      End;
    mpDC1Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          form4.Image1.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          form4.Image1.Picture.Clear;
        End;
      End;
    mpDC2Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          form4.Image2.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          form4.Image2.Picture.Clear;
        End;
      End;
    mpDC3Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          form4.Image3.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          form4.Image3.Picture.Clear;
        End;
      End;
    mpDC4Tex: Begin
        s := MapFolder + MapName + PathDelim + data.ReadAnsiString;
        If FileExistsUTF8(s) Then Begin
          form4.Image4.Picture.LoadFromFile(UTF8ToSys(s));
        End
        Else Begin
          form4.Image4.Picture.Clear;
        End;
      End;
    mpWaveOpponentCount: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit1.Text := inttostr(c);
      End;
    mpWaveOpponentCashPerUnit: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit2.Text := inttostr(c);
      End;
    mpWaveOpponentSpawnDelta: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit5.Text := inttostr(c);
      End;
    mpWaveOpponentSpawnDelay: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit4.Text := inttostr(c);
      End;
    mpWaveOpponentUnitsPerSpawn: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        c := -1;
        data.Read(c, sizeof(c));
        TWaveOpponentFrame(TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).Edit3.Text := inttostr(c);
      End;
    mpWaveOpponent: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        s := Data.ReadAnsiString;
        TWaveOpponentFrame(TWaveFrame(form4.PageControl2.Pages[i].Components[0]).PageControl1.pages[j].Components[0]).ComboBox1.Text := s;
      End;
    mpWaveOpponents: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        TWaveFrame(form4.PageControl2.Pages[i].Components[0]).SetOppenentCountTo(j);
        Form4.RefreshOpponentsClick(Nil);
      End;
    mpWaveHint: Begin
        i := -1;
        data.Read(i, sizeof(i));
        s := data.ReadAnsiString;
        TWaveFrame(form4.PageControl2.Pages[i].Components[0]).Edit1.Text := s;
      End;
    mpCashOnWaveStart: Begin
        i := -1;
        data.Read(i, sizeof(i));
        j := -1;
        data.Read(j, sizeof(j));
        TWaveFrame(form4.PageControl2.Pages[i].Components[0]).Edit2.Text := inttostr(j);
      End;
    mpWaveCount: Begin
        i := -1;
        data.Read(i, sizeof(i));
        form4.SetWaveCountTo(i);
        form4.Timer1.Enabled := true; // refresh aller möglichen listen
      End;
    mpMapType: Begin
        i := -1;
        data.Read(i, sizeof(i));
        form4.ComboBox1.ItemIndex := i;
      End;
    mpMaxPlayer: Begin
        i := -1;
        data.Read(i, sizeof(i));
        form4.edit1.text := inttostr(i);
      End;
    mpDescription: Begin
        s := Data.ReadAnsiString;
        form4.Memo1.Text := DeSerialize(s);
      End;
    mpLives: Begin
        i := -1;
        data.Read(i, sizeof(i));
        form4.edit2.text := inttostr(i);
        i := -1;
        data.Read(i, sizeof(i));
        form4.edit3.text := inttostr(i);
        i := -1;
        data.Read(i, sizeof(i));
        form4.edit4.text := inttostr(i);
      End;
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
        form4.ListBox1.Items.Add(BuyableToString(by));
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
        i := -1;
        data.Read(c, sizeof(c));
        j := -1;
        data.Read(j, sizeof(j));
        bk := bkBuilding;
        data.read(bk, sizeof(bk));
        For i := 0 To Form4.ListBox1.Items.Count - 1 Do Begin
          by := StringToBuyAble(Form4.ListBox1.Items[i]);
          If by.Item = s Then Begin
            by.Item := s;
            by.WaveNum := c;
            by.Count := j;
            by.Kind := bk;
            Form4.ListBox1.Items[i] := BuyableToString(by);
            break;
          End;
        End;
      End;
  End;
  ctd.BlockMapUpdateSending := b;
  LogLeave;
End;

Procedure TForm1.OnStartRound(Sender: TObject);
Var
  dummy: Boolean;
  s: String;
Begin
  log('TForm1.OnStartRound', lltrace);
  caption := defCaption + ' : ' + MapName + ' Round : ' + inttostr(ctd.AktualWave + 1);
  If GetValue('Global', 'ShowWaveOppHint', '1') = '1' Then Begin
    s := 'Opponts for round ' + inttostr(ctd.AktualWave) + ':' + LineEnding + ctd.Map.WaveOppList(ctd.AktualWave);
    form13.AddMessage(s);
    ctd.Splashhint(s, DefaultSplashHintColor);
  End;
  ctd.ShowLifepoints := GetValue('Global', 'AlwaysShowLifepoints', '0') = '1';
  ctd.Map.ShowBackTex := Not (GetValue('Global', 'DisableBackGroundTexturing', '0') = '1');
  MenuItem25.Enabled := true; // Abort Round
  MenuItem28.Enabled := true; // Player infos
  // Der Server Startet eine neue Runde
  // Schließen aller Fenster..
  If form4.Visible Then Begin // Der Editor Dialog
    dummy := true;
    form4.FormCloseQuery(Nil, dummy); // Positionsdaten Speichern
    form4.Hide;
  End;
  If Form8.visible Then Begin // Der Game Statistik Dialog
    Form8.Hide;
  End;
  LogLeave;
End;

Procedure TForm1.OnEndRound(Sender: TObject; Succeed: Boolean; Round_: Integer);
Begin
  MenuItem23.enabled := true; // Restart last Wave
  MenuItem24.enabled := true; // Continue Game
  MenuItem25.Enabled := true; // Abort Round
  MenuItem27.enabled := true; // Save Game
  MenuItem28.Enabled := false; // Player infos
  If Not Succeed Then Begin
    RestoreForm4();
  End;
End;

Procedure TForm1.OnForceEditMode(Sender: TObject);
Begin
  If assigned(ctd) And assigned(ctd.Map) Then Begin
    (*
     * Das Spiel schaltet bereits in den Edit mode um, aber die LCL steht immer noch auf Highscore Anzeigen
     * Beim beenden der Highscores wird das dann "erledigt"
     *)
    If Not form8.Visible Then Begin
      RestoreForm4();
    End;
  End;
  MenuItem25.Enabled := true; // Abort Round
  If form12.Showing Then form12.hide; // Die Player Infos Ausblenden, falls sichtbar
  form4.CheckBox1Change(Nil); // Retrigger "ShowBackTex"
End;

Procedure TForm1.OnHandleLoadGameingData(Sender: TObject);
Begin
  MenuItem23.enabled := false; // Restart last Wave
  MenuItem24.enabled := true; // Continue Game
End;

Procedure TForm1.OnRefreshPlayerStats(Sendet: TObject);
Var
  i: Integer;
Begin
  // Aktualisieren der Player Info im Player Dialog
  If form12.Visible Then
    form12.Button1.Click;
  // Aktualisieren der Spieler Reihenfolge im Start Game Dialog
  If form11.Visible Then Begin
    form11.ListBox1.Items.Clear;
    For i := 0 To ctd.PlayerCount - 1 Do Begin
      form11.ListBox1.Items.Add(format('%d - %s', [i + 1, ctd.Player[i].Name]));
    End;
  End;
End;

Function TForm1.SaveAndCheckMap(ShowWarnings: Boolean; ShowErrors: Boolean
  ): Boolean;
Var
  r: TCheckResult;
Begin
  If Not assigned(ctd) Then exit;
  If Not assigned(ctd.Map) Then exit;
  log('TForm1.SaveAndCheckMap', llTrace);
  DefaultFormatSettings.DecimalSeparator := '.';
  ctd.UpdateMapProperty(mpSaveMap, Nil);
  ctd.Map.Save(MapName); // Speichern Lokal
  If (ShowErrors Or ShowWarnings) Then Begin
    r := ctd.Map.CheckForErrors(ShowWarnings);
    result := (Not r.Warnings) And (Not r.Errors);
  End
  Else Begin
    result := true;
  End;
  LogLeave;
End;

Procedure TForm1.AddUserMessage(Msg: String; WarnLevel: TLogLevel);
Begin
  setlength(fUserMessages, length(fUserMessages) + 1);
  fUserMessages[high(fUserMessages)].Msg := Msg;
  fUserMessages[high(fUserMessages)].WarnLevel := WarnLevel;
End;

Procedure TForm1.OnShowGameStatistics(Msg: String; Const Data: TStream);
Begin
  HideForm4;
  CloseAllModalWindows;
  form8.LoadStatistics(ctd.PlayerIndex, ctd.PlayerCount, Msg, data);
  form8.RadioGroup1.ItemIndex := 0; // Reset des Karten Ratings
  form8.Show;
  Form8.BringToFront;
End;

Var
  f4v: Boolean = false;
  f4t, f4l: integer;

Procedure TForm1.HideForm4;
Begin
  If form4.Visible Then Begin
    f4v := true;
    f4t := form4.Top;
    f4l := form4.Left;
    form4.Hide;
  End;
End;

Procedure TForm1.RestoreForm4;
Begin
  If Not assigned(ctd.Map) Then exit; // Es ist keine Karte geladen -> die Form braucht auch nicht wieder hergestellt zu werden.
  If f4v And (Not form4.Visible) Then Begin
    form4.Top := f4t;
    form4.Left := f4l;
    f4v := false;
  End;
  If ctd.GameState = gs_EditMode Then Begin
    form4.Show;
  End;
End;

Procedure TForm1.OnShowHighScores(Sender: TObject; Const Data: Tstream);
Begin
  If form9.LoadHighscoresFromStream(Data) Then Begin
    HideForm4;
    form9.ShowModal;
    RestoreForm4;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Host a game
  log('TForm1.MenuItem3Click', lltrace);
  form2.caption := HostGame;
  form2.width := 280;
  form2.ModalResult := mrNone;
  form2.Edit1.Text := GetValue('Global', 'Hostport', '1234');
  form2.Edit2.Text := GetValue('Global', 'Hostpassword', '');
  form2.Edit3.Text := GetValue('Global', 'Hostusername', 'Player1');
  form2.ComboBox1.text := '127.0.0.1';
  form2.ComboBox1.Visible := false;
  Form2ShowOnce := false;
  form2.Button3.Left := 80;
  form2.Button3.Width := 102;
  form2.Button3.Caption := 'Create';
  If form2.ShowModal = mrOK Then Begin
    SetValue('Global', 'Hostport', form2.Edit1.Text);
    SetValue('Global', 'Hostpassword', form2.Edit2.Text);
    SetValue('Global', 'Hostusername', form2.Edit3.Text);
    StartHostGame;
  End;
  LogLeave;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Join Game
  log('TForm1.MenuItem4Click', lltrace);
  form2.caption := Joingame;
  form2.width := 533;
  form2.ListBox1.ItemIndex := -1;
  form2.ListBox1.Clear;
  form2.Timer1.Enabled := true;
  form2.ModalResult := mrNone;
  form2.Edit1.Text := GetValue('Global', 'Joinport', '1234');
  form2.Edit2.Text := GetValue('Global', 'Joinpassword', '');
  form2.Edit3.Text := GetValue('Global', 'Joinusername', 'Player2');
  form2.ComboBox1.text := GetValue('Global', 'Joinip', '127.0.0.1');
  form2.ComboBox1.Visible := true;
  form2.Button3.Left := 292;
  form2.Button3.Width := 230;
  form2.Button3.Caption := 'Connect';
  If form2.ShowModal = mrOK Then Begin
    SetValue('Global', 'Joinport', form2.Edit1.Text);
    SetValue('Global', 'Joinpassword', form2.Edit2.Text);
    SetValue('Global', 'Joinusername', form2.Edit3.Text);
    SetValue('Global', 'Joinip', form2.ComboBox1.text);
    StartClientGame;
  End;
  form2.Timer1.Enabled := false;
  LogLeave;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  FileloggingDir: String;
  i: integer;
Begin
  DefFormat := DefaultFormatSettings;
  DefFormat.DecimalSeparator := '.';
  LogShowHandler := @ShowUserMessage;
  fUserMessages := Nil;
  fMapTransferStream := TMemoryStream.Create;
  DefaultFormatSettings.DecimalSeparator := '.';
  InitLogger();
  MapFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'share' + PathDelim;
  ServerMapFolder := '';
  FileloggingDir := '';
  For i := 1 To Paramcount Do Begin
{$IFDEF Windows}
    If lowercase(ParamStrUTF8(i)) = '-c' Then Begin
      AllocConsole; // in Windows unit
      IsConsole := True; // in System unit
      SysInitStdIO; // in System unit
      EnableLogToConsole();
      SetConsoleOutputCP(CP_UTF8);
    End;
{$ENDIF}
    If i < Paramcount Then Begin // Alle Parameter, welche einen weiteren Parameter auslesen
      // FileLogging
      If lowercase(ParamStrUTF8(i)) = '-f' Then Begin
        FileloggingDir := ExtractFilePath(ParamStrUTF8(i + 1));
        If Not DirectoryExistsUTF8(FileloggingDir) Then Begin
          If Not CreateDirUTF8(FileloggingDir) Then Begin
            FileloggingDir := '';
          End;
        End;
        If FileloggingDir <> '' Then Begin
          FileloggingDir := ParamStrUTF8(i + 1);
          SetLoggerLogFile(FileloggingDir);
        End;
      End;
      // Loglevel
      If lowercase(ParamStrUTF8(i)) = '-l' Then Begin
        SetLogLevel(strtointdef(ParamStrUTF8(i + 1), 2));
      End;
      // Anderes Share Folder
      If lowercase(ParamStrUTF8(i)) = '-s' Then Begin
        MapFolder := IncludeTrailingPathDelimiter(ParamStrUTF8(i + 1));
      End;
      // Anderes Map Folder (für Server)
      If lowercase(ParamStrUTF8(i)) = '-m' Then Begin
        ServerMapFolder := ParamStrUTF8(i + 1)
      End;
    End;
  End;
  log('TForm1.FormCreate', llInfo);
  log('TForm1.FormCreate', lltrace);
  If FileloggingDir = '' Then Begin
    log('Disabled, file logging.', llWarning);
  End;
  If Not DirectoryExistsUTF8(MapFolder) Then Begin
    If Not CreateDirUTF8(MapFolder) Then Begin
      LogShow('Could not create share folder : ' + MapFolder, llfatal);
      halt(1);
    End;
  End;
  log('Using share folder : ' + MapFolder, llInfo);
  caption := defCaption;
  fUpdater := TUpdater.Create;
  Tform(self).Constraints.MinHeight := 480;
  Tform(self).Constraints.Minwidth := 640;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    LogShow('Error, could not init dglOpenGL.pas', llfatal);
    LogLeave;
    Halt(1);
  End;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
  OpenGLControl1.Align := alClient;
  ctd := Tctd.create;
  Load_CT_Settings;
{$IFDEF AUTOMODE}
  AutomodeData.State := AM_Idle;
{$ENDIF}
  Application.AddOnIdleHandler(@OnIdle);
  LogLeave;

{$IFDEF WinXPMode}
  LogShow('Compiled in WinXP mode, make shure the server is also compiled in this mode.');
{$ENDIF}
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  log('TForm1.FormDestroy', llTrace);
  If assigned(ctd) Then
    ctd.free;
  ctd := Nil;
  fMapTransferStream.free;
  LogLeave;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  log('TForm1.FormCloseQuery', llTrace);
  // Todo: Speichern der Map, oder wenigstens Nachfragen ob gespeichert werden soll
  log('Shuting down.', llInfo);
  If Form4.Visible Then form4.Close; // Map Editor Dialog Schließen falls geöffnet
  If Form13.Visible Then form13.Close; // Chat Fenster Schließen falls geöffnet

  setValue('MainForm', 'Left', inttostr(Form1.left));
  setValue('MainForm', 'Top', inttostr(Form1.top));
  setValue('MainForm', 'Width', inttostr(Form1.Width));
  setValue('MainForm', 'Height', inttostr(Form1.Height));

  timer1.Enabled := false;
  Initialized := false;
  // Eine Evtl bestehende Verbindung Kappen, so lange die LCL und alles andere noch Lebt.
  ctd.DisConnect;
  fUpdater.free;
  fUpdater := Nil;
  LogLeave;
End;

Procedure TForm1.FormShow(Sender: TObject);
{$IFDEF AUTOMODE}
Var
  i: integer;
{$ENDIF}
Begin
  If Not Form1ShowOnce Then Begin
    Form1ShowOnce := true;
    Form1.left := strtoint(GetValue('MainForm', 'Left', inttostr(Form1.left)));
    Form1.top := strtoint(GetValue('MainForm', 'Top', inttostr(Form1.top)));
    Form1.Width := strtoint(GetValue('MainForm', 'Width', inttostr(Form1.Width)));
    Form1.Height := strtoint(GetValue('MainForm', 'Height', inttostr(Form1.Height)));
    FixFormPosition(form1);
    CheckForNewVersion(false);
  End;
{$IFDEF AUTOMODE}
  If Paramcount <> 0 Then Begin
    For i := 1 To Paramcount Do Begin
      If (lowercase(ParamStrUTF8(i)) = '-d') Then Begin
        // Verbinden auf einen Lokalen Server
        AutomodeData.State := AM_StartClient;
        break;
      End;
    End;
  End;
{$ENDIF}
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  // Grid
  MenuItem12.Checked := Not MenuItem12.Checked;
  ctd.ShowGrid := MenuItem12.Checked;
  SetValue('Global', 'ShowGrid', inttostr(ord(MenuItem12.Checked)));
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Begin
  // Options
  HideForm4;
  form5.CheckBox1.Checked := GetValue('Global', 'InvertMouseScrolling', '0') = '1';
  form5.CheckBox2.Checked := GetValue('Global', 'AutoNextWave', '1') = '1';
  form5.CheckBox3.Checked := GetValue('Global', 'AlwaysShowLifepoints', '0') = '1';
  form5.CheckBox4.Checked := GetValue('Global', 'ShowBuildingRanges', '1') = '1';
  form5.CheckBox5.Checked := GetValue('Global', 'Building_Darkning', '1') = '1';
  form5.CheckBox6.Checked := getValue('Global', 'ShowFPS', '0') = '1';
  form5.CheckBox7.Checked := GetValue('Global', 'DisableBackGroundTexturing', '0') = '1';
  form5.CheckBox8.Checked := GetValue('Global', 'Autozoom', '1') = '1';
  form5.CheckBox9.Checked := GetValue('Global', 'Middle_Mouse_Map_Scolling', '0') = '1';
  form5.CheckBox10.Checked := getValue('Global', 'ShowBuildableTilesDuringBuild', '0') = '1';
  form5.CheckBox11.Checked := GetValue('Global', 'ShowWaveOppHint', '1') = '1';
  form5.CheckBox12.Checked := GetValue('Global', 'ShowHeroRanges', '1') = '1';

  form5.Edit1.text := GetValue('Global', 'MapBlockSize', inttostr(MapBlockSize));
  form5.Edit2.text := GetValue('Global', 'AutoNextWaveDelay', '10');

  TColorSelectBox(form5.FindComp('ColorBox1')).Selected := StringToColor(GetValue('Global', 'Good_Color', ColorToString(Good_Col)));
  TColorSelectBox(form5.FindComp('ColorBox2')).Selected := StringToColor(GetValue('Global', 'Damages_Color', ColorToString(Middle_Col)));
  TColorSelectBox(form5.FindComp('ColorBox3')).Selected := StringToColor(GetValue('Global', 'Broken_Color', ColorToString(Bad_col)));
  TColorSelectBox(form5.FindComp('ColorBox4')).Selected := StringToColor(GetValue('Global', 'Nothing_Color', ColorToString(Nichts_Col)));
  TColorSelectBox(form5.FindComp('ColorBox5')).Selected := StringToColor(GetValue('Global', 'Walkable_Color', ColorToString(Begehbar_col)));
  TColorSelectBox(form5.FindComp('ColorBox6')).Selected := StringToColor(GetValue('Global', 'Buildable_Color', ColorToString(Bebaubar_col)));
  TColorSelectBox(form5.FindComp('ColorBox7')).Selected := StringToColor(GetValue('Global', 'Both_Color', ColorToString(Beides_Col)));
  TColorSelectBox(form5.FindComp('ColorBox8')).Selected := StringToColor(GetValue('Global', 'Grid_Color', ColorToString(Grid_color)));
  TColorSelectBox(form5.FindComp('ColorBox9')).Selected := StringToColor(GetValue('Global', 'BuildingRange_Color', ColorToString(Building_Range_Col)));

  form5.ComboBox1.Text := GetValue('Global', 'Menupos', 'Right');

  form5.ScrollBar1.Position := strtoint(getvalue('Global', 'Fontscale', '10'));
  Form5.ScrollBar1Change(Nil);
  form5.show;
End;

Procedure TForm1.MenuItem15Click(Sender: TObject);
Begin
  // Building Editor
  HideForm4;
  Form14.Edit1.Text := '';
  Form14.LoadBuildingSettings(Nil);
  Form14.Show;
End;

Procedure TForm1.MenuItem16Click(Sender: TObject);
Begin
  // Opponent Editor
  HideForm4;
  Form14.Edit1.Text := '';
  Form14.LoadOpponentSettings;
  Form14.Show;
End;

Procedure TForm1.MenuItem18Click(Sender: TObject);
Begin
  // Start / Restart Game
  // Egal wie ein Spielneustart speichert auf jedenfall die Karte !
  If SaveAndCheckMap(false, true) Then Begin // Erst mal Lokal Checken ob die Karte überhaupt Spielbar ist
    HideForm4;
    // Die Karte Ist Spielbar, dann dem Server das Startkommando geben !
    form11Difficulty := -1;
    form11.Label2.Caption := inttostr(ctd.Map.Lives[0]);
    form11.Label3.Caption := inttostr(ctd.Map.Lives[1]);
    form11.Label4.Caption := inttostr(ctd.Map.Lives[2]);
    ctd.RequestPlayerInfos();
    form11.Showmodal;
    // Start des Spieles
    If form11Difficulty <> -1 Then Begin
      ctd.InitiateNewGame(form11Difficulty);
      ctd.Splashhint('Initiated new game, ...', DefaultSplashHintColor);
    End
    Else Begin
      RestoreForm4;
    End;
  End;
End;

Procedure TForm1.MenuItem19Click(Sender: TObject);
Begin
  // Open Chat
  If form13.visible Then Begin
    form13.BringToFront;
  End
  Else Begin
    form13.show;
  End;
End;

Procedure TForm1.MenuItem20Click(Sender: TObject);
Begin
  // Transfer
End;

Procedure TForm1.MenuItem22Click(Sender: TObject);
Begin
  // about
  showmessage(
    defCaption + LineEnding +
    'License: https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md ' + LineEnding +
    '' + LineEnding +
    'Warranty : There is no warranty, neither in correctness of the' + LineEnding +
    '           implementation, nor anything other that could happen' + LineEnding +
    '           or go wrong, use at your own risk.' + LineEnding +
    '' + LineEnding +
    'Idea : Christian Wimmer, Uwe Schächterle' + LineEnding +
    'Implementation : Uwe Schächterle' + LineEnding +
    'Graphics : Uwe Schächterle' + LineEnding +
    'Leveldesign : Christian Wimmer, Uwe Schächterle' + LineEnding + LineEnding +
    'Sourcecode: https://github.com/PascalCorpsman/ConfigTD'
    );
End;

Procedure TForm1.MenuItem23Click(Sender: TObject);
Begin
  // Restart Last Wave
  log('TForm1.MenuItem23Click', llTrace);
  If Not SaveAndCheckMap(false, true) Then Begin
    LogLeave;
    exit;
  End;
  If ctd.AktualWave = 0 Then Begin
    MenuItem18Click(Nil); // Start / restart
  End
  Else Begin
    ctd.RestartRound;
  End;
  LogLeave;
End;

Procedure TForm1.MenuItem24Click(Sender: TObject);
Begin
  // Continue Game
  log('TForm1.MenuItem24Click', llTrace);
  If Not SaveAndCheckMap(false, true) Then Begin
    LogLeave;
    exit;
  End;
  ctd.continue;
  LogLeave;
End;

Procedure TForm1.MenuItem25Click(Sender: TObject);
Begin
  // Abort Actual wave
  ctd.AbortWave;
End;

Procedure TForm1.MenuItem27Click(Sender: TObject);
Begin
  // Save game
  HideForm4;
  form16.ModalResult := mrCancel;
  ctd.GetSavegames(@Form1GetSavegamesforSaveEvent);
End;

Procedure TForm1.MenuItem28Click(Sender: TObject);
Begin
  // Player infos
  form12.RecreateFrames;
  form12.RefreshInfos;
  form12.Show;
End;

Procedure TForm1.MenuItem29Click(Sender: TObject);
Begin
  // Map texture Editor
  SaveAndCheckMap(false, false); // Der Check ist uns egal, aber gespeichert muss die Karte sein !!
  form17.LoadMap;
  form17.showmodal;
End;

Procedure TForm1.MenuItem31Click(Sender: TObject);
Begin
  // Heros
  HideForm4;
  Form14.LoadHeroSettings(Nil);
  Form14.Show;
End;

End.


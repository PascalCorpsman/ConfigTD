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
Unit uctdserver;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils, Lnet, crt, Graphics,

  uChunkmanager, uctd_map, uvectormath, uctd_common, uctd_spawnmodule;

Type

  TGraphCounter = Record
    Counter: Integer; // Der Wert der immer 10s weise addiert wird
    Ptr: Integer; // Länge der Gültigen Daten in Data
    Data: Array Of Integer; // Die Historie
  End;

  TPlayer = Record
    UserName: String;
    UID: integer;
    Cash: integer; // Aktuelles Geld
    Lives: integer; // Aktuelle Anzahl der Leben die der Spieler hat
    LivesLost: integer; // Anzahl der leben die der Spieler Overall verloren hat
    Kills: integer; // Anzahl der Kills die der Spieler macht
    BonusFinisher: integer; // Anzahl der Bonus Gegner, welche das Ziel Erreicht haben ohne gekillt zu werden.
    Ready: Boolean; // Wenn eine neue Runde Gestartet wird, dann müssen dies alle Clients bestätigen, das wird hier gemerkt
    LastSynchronTimestamp: int64; // Der Zeitstempel bei welchem wir zuletzt was vom CLient empfangen haben.
    RequestPing: Boolean;
    PingArrived: boolean;
    PingStartTimestamp: int64; // Merken, wann der Ping zum Player gesendet wird, damit dann beim eintreffen des Echo's die differenz berechnet werden kann
    DamageDealt: Array[0..3] Of int64; // Schaden je Gegner und Schadensklasse
    (*
     * Daten Die über ein Spiel angesammelt werden
     *)
    BuildingsBuild: Array Of Record
      BuildingName: String; // Der Anzeigename in S1
      BildingFilename: String; // Der Dateiname, zur Eindeutigen identifizierung (falls Gebäude gleiche Namen haben)
      Count: integer;
    End;
    OverAllEarned: integer; // Das Geld das Der Spieler Insgesammt eingenommen hat
    OverAllMoneySpend: integer; // Das Geld das der Spieler Insgesammt Ausgegeben hat
    OverAllMoneyForFree: integer; // Das Geld das der Spieler von anderen Spielern geschenkt bekommen hat
    OverAllGiveAwayMoney: integer; // Das Geld das der Spieler anderen Spielern geschenkt hat
    OverAllBuildingsBuild: integer; // Die Anzahl an Gebäuden die der Spieler gebaut hat
    OverAllBuildingsSold: integer; // Die Anzahl an Gebäuden die der Spieler Verkauft hat
    OverAllBuildingsLeveledUp: integer; // Die Anzahl an Level Ups über alle Gebäude
    (*
     * Statistiken die am Ende in der TaChart Landen
     *)
    gActions: TGraphCounter; // Anzahl der User Aktionen
    gKills: TGraphCounter; // Anzahl der Kills
    gMoneyEarnded: TGraphCounter; // Integral über das eingenommene Geld
    gMoneySpend: TGraphCounter; // Integral über das ausgegebene Geld
  End;

  TFrameLog = Record
    Timestamp: int64; // Zeitstempel an dem das letzte mal der 10000ms info Text generiert wurde
    AccumulatedSize: int64; // Summe aller Daten die bis dahin an den Chunkmanager übergeben wurden
    Count: int64; // Anzahl der versendeten Datenpakete (idealerweise sollte das Zeit / 40ms sein)
  End;

  { TServer }

  TServer = Class
  private
    fCounterfor10s: integer; // Zähler für die 10s Zeitscheibe für die Spielerstatistiken
    fOverAllGameingTime: int64; // Simulierte Zeit in ms, ohne die Zeit die in Pause gewartet wurde
    fOverAllRealGameingTime: int64; // Zeit in ms die Echt gespielt wurde, ohne die Zeit die in Pause gewartet wurde
    fOverAllPausingTime: int64; // Die Zeit in Summe Pause gemacht wurde
    fFrameLog: TFrameLog; // Zum Loggen der versendeten Daten alle 10s
    fUDP: TLUdp; // Zum Empfangen und Senden der Aktiven Server Verbindungen
    fTCP: TLTcp; // Die L-Net TCP Komponente
    fChunkManager: TChunkManager; // Der Chunkmanager zum senden der Daten
    fTCPPort: integer; // Der Port auf welchem wir den Chunkmanager gestaret haben (für die UDP-Broadcaster)

    factive: Boolean; // True, so lange clients verbunden sind
    fLastActiveTickTimestamp: Int64; // Zum Bestimmen ob der Server sich automatisch beenden soll, wenn Keine Spieler mehr aktiv sind.
    fAutotimeout: integer;
    fPassword: String;
    fPLayer: Array Of TPlayer;
    fMap: TMap;
    fAktualRound: Integer;
    fGameState: TGameState;
    fSpawnModul: TSpawnModul; // Das Modul, welches die Gegner Aussendet
    FLastFrameTimestamp: int64; // Zeitpunkt an dem die Einheiten das letzte Mal bewegt wurden
    fLastClientUpdateTimestamp: int64; // Zeitpunkt an dem zuletzt alle Clients Aktualisiert wurden
    fpausing: Boolean; // True = Spiel Angehalten
    fSyncPause: Boolean;
    foldpausevalue: Boolean;
    fPauseTimestamp: int64; // Der Zeitpunkt, an dem eine Pause gestartet wurde
    fLastUpdateDataSend: Boolean; // Zum Erkennen der Negativen Flanke der Bewegten Einheiten auf der Karte (= 0)
    fLastHeartbeatTimestamp: int64; // Der Zeitpunkt an welchen der Letzte Heartbeat versendet wird

    fRestartWave, // Wird immer zu beginn eine Runde gespeichert
    fLastSaveable: TMemoryStream; // Wird immer zum Ende einer Runde gespeichert

    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);

    Procedure OnUDPReceiveEvent(aSocket: TLSocket);
    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
    Function SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer): Boolean;

    Function UidToUsername(Uid: integer): String;

    Procedure PlayerLeaves(PlayerUid: integer);
    Procedure CreateFileSendRequest(MapName, Filename: String; UID: integer);
    Procedure HandleCreateMap(W, H: integer; MapName_: String; dc1, dc2, dc3, dc4: TPortableNetworkGraphic);
    Procedure HandleFileReceiveRequest(MapName_, Filename: String; CRC: Uint32; ResonceID, UID: integer);
    Procedure HandleFileReceifed(MapName_, Filename: String; Const data: TStream; UID: integer);
    Procedure HandleFileListrequest(Extension: String; ResponceID, UID: integer);
    Procedure HandleRequestSavegames(ResponceID, UID: integer);
    Procedure HandleRequestSaveGame(Filename: String; UID: integer);
    Procedure HandleRequestLoadGame(Filename: String; UID: integer);
    Procedure HandleDelSaveGame(Filename: String);
    Procedure HandleDelAllSaveGames();
    Procedure HandleTransferCash(DestPlayer, Count, uid: integer);
    Procedure HandleBuildingStrategyChange(Owner: integer; Const Stream: TStream);
    Procedure HandleHeroStrategyChange(Owner: integer; Const Stream: TStream);
    Procedure HandleSetHeroTargets(Owner: integer; Const Stream: TStream);
    Procedure HandleTransferCompleteMapTerrain(Const Stream: TStream);

    Procedure HandleSendFile(MapName_, FileName: String; ResponceID, UID: integer);
    Procedure HandleRequestPingtimes(UID: integer);
    Procedure HandlePing(UID: integer);
    Procedure HandleGetPlayerStats(uid: integer);
    Procedure HandleRequestPlayerPosChange(Playerindex: integer; Up: Boolean);

    Procedure HandleRequestUserLogin(Username, Password: String; ClientVersion: uint32; UID: integer);
    Procedure HandleRequestMapList(ResponceID, Uid: integer);
    Procedure HandleRequestMap(ResponceID, Uid: integer);
    Procedure HandleRequestMapPreviewInfo(MapName_: String; ResponceID, Uid: integer);
    Procedure HandleRequestLoadMap(MapName_: String; ResponceId, Uid: Integer);
    Procedure HandleUpdateMapProperty(MapProperty: integer; Const Data: TStream; UID: integer);
    Procedure HandleChatMessage(Text: String; Uid: integer);
    Procedure HandleKickPlayerOut(PlayerName: String; Uid: integer);
    Procedure HandleBuyBuilding(x, y: integer; Bname: String; Uid: integer);
    Procedure HandleBuyHero(x, y: integer; Bname: String; Uid: integer);
    Procedure HandleSellBuilding(Owner: integer; Const Stream: TStream);
    Procedure HandleLevelUpBuilding(Owner: integer; Const Stream: TStream);
    Procedure HandleReceiveHeartBeat(p: integer; t: int64);
    Procedure HandleSendLastWaveToAll();

    Procedure HandleStartRound(); // Alle Vorarbeiten sind Geleistet, es geht nun Tatsächlich los
    Procedure HandleInitiateNewRound(Difficulty, Round, UID: integer); // Startet ein neues Spiel
    Procedure HandleOnEndRound(Succeed: Boolean); // Beendet eine Runde
    Procedure HandleOnEndGame(); // Wird aufgerufen, wenn alle Waves gespielt sind oder die Spieler verloren haben.
    Procedure HandleAbortWave(UID: integer);
    Procedure HandleRestartLastWave(UID: integer);
    Procedure Handle10sStatistikEvaluation(); // Alle 10s Statistikwerte in die Array's übertragen..
    Procedure HandleResetPlayerStatisikAndValues(Difficulty: integer; Maptype: TMapType);

    Procedure HandleTogglePause();
    Function ExtractMapAttibutes(MapName: String): String;
    Procedure SendSplashMessage(Text: String; Delay: Integer; Color: tvector3; Uid: integer);

    Procedure CreateNewFrame; // Quasi das Virtuelle render
    Procedure UpdateAllClients; // Aktualisierung aller Clients

    Procedure UpdateEvent(Player: integer; Reason, ID2: integer);
    Procedure EndGameCheck;

    Procedure RefreshAllPlayerStats(Uid: integer = 0);
    Procedure CheckSynchrons;

    Procedure ApplyPause(Value: Boolean);

    Procedure SaveGame(Const Stream: Tstream);
    Function LoadGame(Const Stream: TStream; uid: integer): boolean;
    Procedure BakupWave();
    Procedure AppendGameStatistics(Const Stream: TStream); // Am Ende eines Spiels werden die Spielstatistiken an den Stream Angehängt
    Function CalcHighScoreForPlayer(PlayerIndex: integer): Int64;
  public
    Constructor create(Port, AutoTimeOut: Integer; Password: String);
    Destructor destroy; override;
    Procedure Execute;
    Procedure SaveWhatPossible;
  End;

Implementation

Uses
  lclintf, FileUtil, LazFileUtils, IniFiles, math, IntfGraphics, fpImage, LCLType,

  ugraphics, uctd_messages, uctd_building, uctd_hero, uctd_mapobject;

{ TServer }

Procedure TServer.OnAccept(aSocket: TLSocket);
Begin
  // Wir Aktzeptieren eine Engehende Verbindung
  log('TServer.OnAccept : ' + aSocket.PeerAddress, llTrace);
  fChunkManager.SetNoDelay(true);
  LogLeave;
End;

Procedure TServer.OnDisconnect(aSocket: TLSocket);
Var
  uid: integer;
Begin
  // Wir verlieren einen Spieler
  If assigned(asocket) Then Begin
    log('TServer.OnDisconnect : ' + aSocket.PeerAddress, llTrace);
  End
  Else Begin
    log('TServer.OnDisconnect', llTrace);
  End;
  uid := fChunkManager.SocketToUID(aSocket);
  PlayerLeaves(uid);
  LogLeave;
End;

Procedure TServer.OnError(Const msg: String; aSocket: TLSocket);
Begin
  log('TServer.OnError', llTrace);
  If assigned(asocket) Then Begin
    log(asocket.PeerAddress + msg, llError)
  End
  Else Begin
    log(msg, llError)
  End;
  LogLeave;
End;

Procedure TServer.OnUDPReceiveEvent(aSocket: TLSocket);
Var
  UserName: String;
  Buffer: Array[0..1023] Of byte;
  cnt, i, ReadCnt: Integer;
  b: Byte;
Begin
  If assigned(aSocket) Then Begin
    log('TServer.OnUDPReceiveEvent : ' + aSocket.PeerAddress, llTrace);
  End
  Else Begin
    log('TServer.OnUDPReceiveEvent', llTrace);
  End;
  Repeat
    ReadCnt := aSocket.Get(buffer, 1024); // Irgendwas Lesen, sonst initialisierts die Daten nicht Richtig, bzw die Puffer laufen über
    If fGameState = gs_EditMode Then Begin // Nur im Editor Modus, aktzeptieren wir Spieler, also Antworten wir auch nur wenn wir im Editor Modus sind
      If High(fPLayer) <> -1 Then Begin // Es ist midnestens 1 Spieler Angemeldet, also geben wir den Namen des 1. Spielers an
        UserName := fPLayer[0].UserName + 's game';
      End
      Else Begin // Gar kleine Spieler, dann nur den Servernamen
        UserName := 'no user connected';
      End;
      username := username + ':' + inttostr(fTCPPort);
      cnt := length(username);
      b := 42;
      For i := 0 To cnt - 1 Do Begin
        buffer[i] := ord(username[i + 1]);
        b := b Xor buffer[i];
      End;
      Buffer[cnt] := b;
      aSocket.Send(buffer, cnt + 1);
    End;
  Until ReadCnt = 0;
  LogLeave;
End;

Procedure TServer.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Var
  s, t: String;
  w, h, k: integer;
  b: Boolean;
  crc: Uint32;
  m: TMemoryStream;
  ts: int64;
  responceID: integer;
  ver: uint32;
  p1, p2, p3, p4: TPortableNetworkGraphic;
Begin
  responceID := Chunk.UserDefinedID And $FFFF0000;
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    log(format('TServer.OnReceivedChunk : %d, %s', [Chunk.UID, MessageIdentifierToString(Chunk.UserDefinedID)]), llTrace);

  Case (Chunk.UserDefinedID And $FFFF) Of
    miCleanupUnusedOpponents: Begin
        If assigned(fMap) Then Begin
          fMap.DeleteUnusedOpponents();
          SendChunk(miCleanupUnusedOpponents, Nil, 0);
        End;
      End;
    miAddRandomWave: Begin
        If assigned(fMap) Then Begin
          k := 0;
          chunk.Data.Read(k, sizeof(k));
          // Die Karte um eine Zufällige Wave erweitern
          s := fMap.AddRandomWave(k);
          If s = '' Then Begin
            fMap.Save(MapName);
            // Alle informieren
            //  HandleRequestMap(0, 0); -- Das Killt auch die gekauften Geaddeten Sachen der Karte -> wir müssen alles von "hand" an Alle senden
            HandleSendLastWaveToAll();
          End
          Else Begin
            // Dem User sagen warum es nicht geklappt hat.
            SendSplashMessage('Error: ' + s, DefaultSplashHintDelay, v3(1, 0, 0), Chunk.uid);
          End;
        End;
      End;
    miSendMapRating: Begin
        k := 0;
        chunk.Data.Read(k, sizeof(k));
        If assigned(fMap) Then fMap.AddRating(k);
      End;
    miDelBuilding: Begin
        s := Chunk.Data.ReadAnsiString;
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        If assigned(fMap) Then fMap.delBuilding(s);
        SendChunk(miDelBuilding, m, -Chunk.UID);
      End;
    miDelHero: Begin
        s := Chunk.Data.ReadAnsiString;
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        If assigned(fMap) Then fMap.delHero(s);
        SendChunk(miDelHero, m, -Chunk.UID);
      End;
    miDelOpponent: Begin
        s := Chunk.Data.ReadAnsiString;
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        If assigned(fMap) Then fMap.delOpponent(s);
        SendChunk(miDelOpponent, m, -Chunk.UID);
      End;
    miRequestClearMapHighscores: Begin
        fMap.ClearHighScore();
        fMap.Save(MapName);
      End;
    miRequestMapHighscoresAndRating: Begin
        m := TMemoryStream.Create;
        fMap.GetHighscoreAndRating(m);
        SendChunk(miRequestMapHighscoresAndRatingResult, m, Chunk.UID);
      End;
    miCreateSplashMark: Begin
        // Dem Server sind die "Ausrufezeichen" Egal, er leitet sie einfach nur weiter an alle
        m := TMemoryStream.Create;
        m.CopyFrom(chunk.Data, Chunk.Data.Size);
        SendChunk(miCreateSplashMark, m, 0);
      End;
    miincSpeed: Begin
        w := Speedup;
        Speedup := min(Speedup * 2, 4);
        If w <> Speedup Then Begin
          m := TMemoryStream.create;
          m.write(Speedup, sizeof(Speedup));
          SendChunk(miSetSpeedUp, m, 0);
        End;
      End;
    miCloneMapWave: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        If assigned(fMap) Then Begin
          fMap.CloneWave(k, w);
          m := TMemoryStream.Create;
          m.Write(k, sizeof(k));
          m.Write(w, sizeof(w));
          SendChunk(miCloneMapWave, m, -Chunk.UID); // An alle außer dem Absender melden was sache ist ;)
        End;
      End;
    midecSpeed: Begin
        w := Speedup;
        Speedup := max(Speedup Div 2, 1);
        If w <> Speedup Then Begin
          m := TMemoryStream.create;
          m.write(Speedup, sizeof(Speedup));
          SendChunk(miSetSpeedUp, m, 0);
        End;
      End;
    miTransferCompleteMapTerrain: Begin
        HandleTransferCompleteMapTerrain(chunk.data);
      End;
    miRestartLastWave: Begin
        HandleRestartLastWave(Chunk.UID);
      End;
    miChangeBuildingStrategy: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        HandleBuildingStrategyChange(k, Chunk.Data);
      End;
    miChangeHeroStrategy: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        HandleHeroStrategyChange(k, Chunk.Data);
      End;
    miSetHeroTargets: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        HandleSetHeroTargets(k, Chunk.Data);
      End;
    miTransferCash: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        HandleTransferCash(k, w, chunk.UID);
      End;
    miLoadGame: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleRequestLoadGame(s, chunk.UID);
      End;
    misavegame: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleRequestSaveGame(s, chunk.UID);
      End;
    midelSaveGames: Begin
        HandleDelAllSaveGames();
      End;
    miRefreshPlayerStats: Begin
        HandleGetPlayerStats(Chunk.Uid);
      End;
    miRequestPlayerPosChange: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        HandleRequestPlayerPosChange(k, w = 1);
      End;
    midelSaveGame: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleDelSaveGame(s);
      End;
    miRequestSavegames: Begin
        HandleRequestSavegames(responceID, chunk.UID);
      End;
    miPing: Begin
        HandlePing(chunk.UID);
      End;
    miAbortWave: Begin
        HandleAbortWave(chunk.UID);
      End;
    miRequestPingTimes: Begin
        HandleRequestPingtimes(chunk.UID);
      End;
    miHeartBeat: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        ts := 0;
        chunk.Data.Read(ts, sizeof(ts));
        HandleReceiveHeartBeat(k, ts);
      End;
    miRequestFileList: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleFileListrequest(s, ResponceID, Chunk.UID);
      End;
    miFileTransfer: Begin
        s := Chunk.Data.ReadAnsiString;
        t := Chunk.Data.ReadAnsiString;
        m := TMemoryStream.Create;
        m.CopyFrom(Chunk.Data, Chunk.Data.Size - Chunk.Data.Position);
        m.position := 0;
        HandleFileReceifed(s, t, m, Chunk.UID);
        m.free;
      End;
    miRequestFileTransfer: Begin
        s := Chunk.Data.ReadAnsiString;
        t := Chunk.Data.ReadAnsiString;
        crc := 0;
        chunk.data.Read(crc, sizeof(crc));
        HandleFileReceiveRequest(s, t, crc, responceID, Chunk.UID);
      End;
    miWantLevelUpBuilding: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        HandleLevelUpBuilding(k, Chunk.Data);
      End;
    miSellBuilding: Begin
        k := -1;
        chunk.Data.Read(k, sizeof(k));
        HandleSellBuilding(k, Chunk.Data);
      End;
    miBuyBuilding: Begin
        s := Chunk.Data.ReadAnsiString;
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        h := -1;
        chunk.Data.Read(h, sizeof(h));
        HandleBuyBuilding(w, h, s, Chunk.UID);
      End;
    miBuyHero: Begin
        s := Chunk.Data.ReadAnsiString;
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        h := -1;
        chunk.Data.Read(h, sizeof(h));
        HandleBuyHero(w, h, s, Chunk.UID);
      End;
    miChatMessage: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleChatMessage(s, Chunk.UID);
      End;
    miKickPlayerOut: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleKickPlayerOut(s, Chunk.UID);
      End;
    miTogglePause: Begin
        HandleTogglePause();
      End;
    miStartRoundresult: Begin
        b := true;
        For w := 0 To high(fPLayer) Do Begin
          If fPLayer[w].UID = Chunk.UID Then Begin
            fPLayer[w].Ready := true;
          End;
          If Not fPLayer[w].Ready Then b := false; // Mindestens 1 Spieler ist noch nicht soweit.
        End;
        If b Then Begin // Alle Mitspieler sind soweit, es kann losgehen
          HandleStartRound();
        End;
      End;
    miInitiateNewGame: Begin
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        HandleInitiateNewRound(w, 0, chunk.UID);
      End;
    miContinue: Begin
        If fGameState = gs_EditMode Then Begin
          HandleInitiateNewRound(fmap.Difficulty, fAktualRound + 1, chunk.UID);
        End;
      End;
    miUpdateMapProperty: Begin
        w := -1;
        chunk.Data.Read(w, sizeof(w));
        h := -1;
        chunk.Data.Read(h, sizeof(h)); // Die Absender Server UID brauchen wir nicht, die Kennen wir eh schon
        HandleUpdateMapProperty(w, chunk.Data, chunk.UID);
      End;
    miStartFileTransfer: Begin
        s := Chunk.Data.ReadAnsiString;
        t := Chunk.Data.ReadAnsiString;
        HandleSendFile(s, t, responceID, Chunk.uid);
      End;
    miRequestLogin: Begin
        s := Chunk.Data.ReadAnsiString;
        t := Chunk.Data.ReadAnsiString;
        ver := $FFFFFFFF;
        Chunk.Data.Read(ver, sizeof(ver));
        HandleRequestUserLogin(s, t, ver, chunk.UID);
      End;
    miRequestMap: Begin
        HandleRequestMap(ResponceId, chunk.UID);
      End;
    miRequestLoadMap: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleRequestLoadMap(s, ResponceId, chunk.uid);
      End;
    miRequestMapList: Begin
        HandleRequestMapList(responceID, chunk.UID);
      End;
    miRequestMapPreviewInfo: Begin
        s := Chunk.Data.ReadAnsiString;
        HandleRequestMapPreviewInfo(s, responceID, chunk.UID);
      End;
    miNewMap: Begin
        w := 0;
        h := 0;
        chunk.Data.Read(w, sizeof(w));
        chunk.Data.Read(h, sizeof(h));
        s := chunk.Data.ReadAnsiString();
        p1 := TPortableNetworkGraphic.Create;
        p1.LoadFromStream(Chunk.Data);
        p2 := TPortableNetworkGraphic.Create;
        p2.LoadFromStream(Chunk.Data);
        p3 := TPortableNetworkGraphic.Create;
        p3.LoadFromStream(Chunk.Data);
        p4 := TPortableNetworkGraphic.Create;
        p4.LoadFromStream(Chunk.Data);
        HandleCreateMap(w, h, s, p1, p2, p3, p4);
        p1.free;
        p2.free;
        p3.free;
        p4.free;
        // Broadcast an Alle zum Laden der Karte
        HandleRequestMap(0, 0);
      End
  Else Begin
      log('Unknown user defined id : ' + inttostr(Chunk.UserDefinedID And $FFFF), llError);
    End;
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    LogLeave;
End;

Function TServer.SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer
  ): Boolean;
Var
  i: integer;
Begin
{$IFDEF DoNotLog_CyclicMessages}
  If ((UserDefinedID And $FFFF) <> miUpdateMoveables) And
    ((UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    log(format('TServer.SendChunk : %d, %s', [uid, MessageIdentifierToString(UserDefinedID)]), llTrace);
  result := fChunkManager.SendChunk(UserDefinedID, data, uid);
  If Not result Then Begin
    For i := low(fPLayer) To high(fPLayer) Do Begin
      If fPLayer[i].UID = UID Then Begin
        log('Could not send to player : ' + fPLayer[i].UserName, llCritical);
        LogLeave;
        exit;
      End;
    End;
    If uid < 0 Then Begin
      log('Could not send, maybe no more clients connected.', llWarning);
    End
    Else Begin
      log('Could not send to player : ' + inttostr(uid), llCritical);
    End;
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If (UserDefinedID <> miUpdateMoveables) And
    (UserDefinedID <> miHeartBeat) Then
{$ENDIF}
    LogLeave;
End;

Function TServer.UidToUsername(Uid: integer): String;
Var
  i: Integer;
Begin
  result := inttostr(uid); // UID Ausgeben, falls wir den User nicht finden.
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = Uid Then Begin
      result := fPLayer[i].UserName;
      break;
    End;
  End;
End;

Procedure TServer.PlayerLeaves(PlayerUid: integer);
Var
  i, j: integer;
  m: TMemoryStream;
  b: Boolean;
Begin
  log('TServer.PlayerLeaves', llTrace);
  If PlayerUid = 0 Then Begin
    AssertLog(PlayerUid = 0, 'Disconnect user with unknown uid', llCritical);
    LogLeave;
    exit;
  End;
  For i := low(fPLayer) To high(fPLayer) Do Begin
    If fPLayer[i].UID = PlayerUid Then Begin
      log('Lost player : ' + fPLayer[i].UserName, llInfo);
      // Den Anderen Mitteilen, das wir einen Spieler verloren haben
      m := TMemoryStream.Create;
      m.WriteAnsiString('Lost player : ' + fPLayer[i].UserName);
      b := SendChunk(miChatMessage, m, -PlayerUid);

      // Todo : Wenn das Spiel gerade läuft, muss alles gelöscht werden, was dem Spieler gehört
      If assigned(fMap) Then Begin
        fmap.Save(MapName); // Vorsichtshalber mal die Karte Speichern
      End;
      setlength(fPLayer[i].gActions.Data, 0);
      setlength(fPLayer[i].gKills.Data, 0);
      setlength(fPLayer[i].gMoneyEarnded.Data, 0);
      setlength(fPLayer[i].gMoneySpend.Data, 0);

      For j := i To high(fPLayer) - 1 Do Begin
        fPLayer[j] := fPLayer[j + 1];
      End;
      setlength(fPLayer, high(fPLayer));

      // Das Autospeichern ruiniert das Savegame nun, da die Spielerzahl nicht mehr stimmt, Das Automatische Bakup Serverseitig retet das aber wieder ;)
      fGameState := gs_EditMode;
      If b Then Begin // Wenn wir schon die byby meldung nicht durchgebracht haben, geht diese hier auch nicht durch.
        SendChunk(miForceEditMode, Nil, 0); // So schmeisen wir alle Raus, und es passt auf jeden Fall
      End;
      break;
    End;
  End;
  If assigned(fPLayer) Then Begin
    // Wenn es noch andere Spieler gibt Aktualisieren der Spielerliste (z.B. im Start Game Mode wichtig..)
    HandleGetPlayerStats(0);
  End
  Else Begin
    // Der letzte Geht, zurück in den Edit Modus, macht nur im Timeout=0 Modus sinn, sonst beeden wir eh nach 1000ms
    fGameState := gs_EditMode;
  End;
  If (high(fPLayer) = -1) And (fAutotimeout <> 0) Then Begin
    log(format('Lost last client, will shut down in %0.3fs', [fAutotimeout / 1000]), llInfo);
  End;
  LogLeave;
End;

Procedure TServer.CreateFileSendRequest(MapName, Filename: String; UID: integer
  );
Var
  crc: UInt32;
  m: TMemoryStream;
  s: String;
Begin
  log(format('TServer.CreateFileSendRequest: %d, %s, %s', [uid, MapName, Filename]), llTrace);
  s := MapFolder + MapName + PathDelim + Filename;
  If FileExistsUTF8(s) And (Filename <> '') Then Begin
    crc := CRC_of_File(s);
    m := TMemorystream.create;
    m.WriteAnsiString(MapName);
    m.WriteAnsiString(Filename);
    m.Write(crc, sizeof(crc));
    SendChunk(miRequestFileTransfer, m, uid);
  End;
  LogLeave;
End;

Procedure TServer.HandleRequestUserLogin(Username, Password: String;
  ClientVersion: uint32; UID: integer);
Var
  m: TMemoryStream;
  i, j: integer;
Begin
  log('TServer.HandleUserLoginRequest', llTrace);
  m := TMemoryStream.Create;
  // 0. Check ob die beiden Versionen Compatibel sind
  If version <> ClientVersion Then Begin
    i := EC_Invalid_Versions;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;
  // 1. Check : Passwort
  If Password <> fPassword Then Begin
    i := EC_Invalid_Password;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;
  // 2. Check : Darf der Spieler überhaupt verbinden = Läuft gerade ein Spiel ?
  If fGameState <> gs_EditMode Then Begin
    i := EC_game_full;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;
  // 3. Check : Gibt es den Spielernamen bereits ?
  For j := low(fPLayer) To high(fPLayer) Do Begin
    If fPLayer[j].UserName = Username Then Begin
      i := EC_User_already_exists;
      m.Write(i, sizeof(i));
      SendChunk(miRequestLoginResult, m, UID);
      LogLeave;
      exit;
    End;
  End;
  // Alles I.O. Der User kann beitreten
  If assigned(fMap) Then Begin // Die Karte muss gespeichert werden, sonst lädt der Client evtl. nicht die Aktuellste version.
    fMap.Save(MapName);
  End;
  setlength(fPLayer, high(fPLayer) + 2);
  fPLayer[high(fPLayer)].UID := UID;
  fPLayer[high(fPLayer)].RequestPing := false;
  fPLayer[high(fPLayer)].PingArrived := false;
  fPLayer[high(fPLayer)].UserName := Username;
  fPLayer[high(fPLayer)].gActions.Data := Nil;
  fPLayer[high(fPLayer)].gKills.Data := Nil;
  fPLayer[high(fPLayer)].gMoneyEarnded.Data := Nil;
  fPLayer[high(fPLayer)].gMoneySpend.Data := Nil;
  log(format('Accepted player : %s as %d, now %d player in logged in', [Username, uid, length(fplayer)]), llInfo);
  i := EC_No_Error;
  m.Write(i, sizeof(i));
  i := uid;
  m.Write(i, sizeof(i));
  SendChunk(miRequestLoginResult, m, UID);
  // Allen Mitteilen das wir nen neuen Spieler haben
  m := TMemoryStream.Create;
  m.WriteAnsiString(Username + ' joined the game');
  SendChunk(miChatMessage, m, 0);
  HandleGetPlayerStats(0); // Aktualisieren der Spielerliste (z.B. im Start Game Mode wichtig..)
  LogLeave;
End;

Procedure TServer.HandleRequestMapList(ResponceID, Uid: integer);
Var
  sl: TStringlist;
  m: TMemoryStream;
  i: integer;
  s: String;
Begin
  log('TServer.HandleMapListRequest', lltrace);
  sl := ListAllSubdirs(MapFolder);
  // Aufbau "Name : Attribute(bitkodiert)"
  m := Tmemorystream.Create;
  i := sl.Count;
  m.Write(i, sizeof(i));
  For i := 0 To sl.Count - 1 Do Begin
    s := sl[i] + ':' + ExtractMapAttibutes(sl[i]);
    m.WriteAnsiString(s);
  End;
  SendChunk(miRequestMapListResult Or ResponceID, m, uid);
  sl.free;
  LogLeave;
End;

Procedure TServer.HandleCreateMap(W, H: integer; MapName_: String; dc1, dc2,
  dc3, dc4: TPortableNetworkGraphic);
Var
  s: String;
  m: TMap;
Begin
  log('TServer.HandleCreateMap', lltrace);
  s := MapFolder + MapName_;
  If Not CreateDirutf8(s) Then Begin
    log('Could not create directory : ' + s, llCritical);
    LogLeave;
    exit;
  End;
  s := s + PathDelim;
  // Erstellen einer Leeren Karte und Speichern
  dc1.SaveToFile(s + 'dc1.png');
  dc2.SaveToFile(s + 'dc2.png');
  dc3.SaveToFile(s + 'dc3.png');
  dc4.SaveToFile(s + 'dc4.png');
  m := TMap.Create;
  m.Resize(w, h);
  m.MapType := mtSingle;
  m.MaxPlayer := 1;
  m.DamageClass1Tex := 'dc1.png';
  m.DamageClass2Tex := 'dc2.png';
  m.DamageClass3Tex := 'dc3.png';
  m.DamageClass4Tex := 'dc4.png';
  m.Save(MapName_);
  m.free;
  If assigned(fmap) Then fmap.free;
  fmap := TMap.Create;
  fmap.Load(MapName_);
  LogLeave;
End;

Procedure TServer.HandleFileReceiveRequest(MapName_, Filename: String;
  CRC: Uint32; ResonceID, UID: integer);
Var
  m: TMemoryStream;
  b: Boolean;
  cr: UInt32;
Begin
  log(format('TServer.HandleFileReceiveRequest : %d, %s, %s', [uid, MapName_, FileName]), llTrace);
  b := false;
  If FileExistsUTF8(MapFolder + MapName_ + PathDelim + Filename) Then Begin
    cr := CRC_of_File(MapFolder + MapName_ + PathDelim + Filename);
    b := cr <> crc;
  End
  Else Begin
    b := true;
  End;
  m := TMemoryStream.Create;
  m.write(b, SizeOf(b));
  SendChunk(miStartFileTransfer Or ResonceID, m, uid);
  LogLeave;
End;

Procedure TServer.HandleFileReceifed(MapName_, Filename: String;
  Const data: TStream; UID: integer);
Var
  f: TFileStream;
Begin
  log(format('TServer.HandleFileReceifed : %d, %s, %s', [uid, MapName_, Filename]), llTrace);
  Try
    f := TFileStream.Create(MapFolder + MapName_ + PathDelim + Filename, fmCreate Or fmOpenWrite);
    data.Position := 0;
    f.CopyFrom(data, data.Size);
    f.free;
  Except
    On e: Exception Do Begin
      log(e.Message, llCritical);
      LogLeave;
      exit;
    End;
  End;
  // An Alle die Datei verteilen
  CreateFileSendRequest(MapName_, Filename, 0);
  LogLeave;
End;

Procedure TServer.HandleFileListrequest(Extension: String; ResponceID,
  UID: integer);
Var
  sl: TStringList;
  m: TMemoryStream;
  i: integer;
Begin
  sl := FindAllFiles(MapFolder + MapName, Extension, false);
  sl.Sorted := true;
  sl.Sort;
  m := TMemoryStream.Create;
  i := sl.Count;
  m.Write(i, sizeof(i));
  For i := 0 To sl.Count - 1 Do Begin
    m.WriteAnsiString(ExtractFileName(sl[i]));
  End;
  sl.free;
  SendChunk(miRequestFileListResult Or ResponceID, m, UID);
End;

Procedure TServer.HandleRequestSavegames(ResponceID, UID: integer);
  Function ExtractfileTimeStamp(Const Filename: String): String;
  Var
    age: Longint;
    dt: TDateTime;
  Begin
    age := FileAge(Filename);
    dt := FileDateToDateTime(age);
    result := FormatDateTime('yyyy.mm.dd hh:nn:ss', dt);
  End;

Var
  sl2, sl: TStringList;
  m: TMemoryStream;
  i: integer;
  fTruncedMapfolder: String;
  f: TFileStream;
  flag: byte;
Begin
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
  sl := FindAllFiles(fTruncedMapfolder, '*.sg', false);
  m := TMemoryStream.Create;
  i := sl.Count;
  m.Write(i, sizeof(i));
  sl2 := TStringList.Create;
  For i := 0 To sl.Count - 1 Do Begin
    sl2.add(ExtractfileTimeStamp(sl[i]) + '| ' + ExtractFileName(sl[i]));
  End;
  sl.free;
  sl2.Sort; // Sortieren das die Jüngste Datei immer Oben steht
  For i := sl2.Count - 1 Downto 0 Do Begin
    m.WriteAnsiString(sl2[i]);
  End;
  // Nun fügen wir noch i Bytes an, damit der Client weis, ob es sich um Bakups oder echte Savegames handelt.
  For i := sl2.count - 1 Downto 0 Do Begin
    f := TFileStream.Create(fTruncedMapfolder + RemoveTimestampInfoFromFilename(sl2[i]), fmOpenRead);
    flag := 0;
    f.Read(flag, sizeof(flag));
    f.free;
    m.write(flag, SizeOf(flag));
  End;
  sl2.free;
  SendChunk(miRequestSavegamesResult Or ResponceID, m, UID);
End;

Procedure TServer.HandleRequestSaveGame(Filename: String; UID: integer);
Var
  fTruncedMapfolder: String;
  f: TFileStream;
  flag: Byte;
Begin
  log('TServer.HandeRequestSaveGame : ' + Filename, llTrace);
  If fLastSaveable.Size = 0 Then Begin
    SendSplashMessage('Error nothing to save', DefaultSplashHintDelay, v3(1, 0, 0), uid);
    LogLeave;
    exit;
  End;
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
  If Not DirectoryExistsUTF8(fTruncedMapfolder) Then Begin
    If Not CreateDirUTF8(fTruncedMapfolder) Then Begin
      log('Could not create savegames directory : ' + fTruncedMapfolder, llError);
      SendSplashMessage('Could not create savegames directory : ' + fTruncedMapfolder, DefaultSplashHintDelay, v3(1, 0, 0), uid);
      LogLeave;
      exit;
    End;
  End;
  Try
    f := TFileStream.Create(fTruncedMapfolder + Filename + '.sg', fmCreate Or fmOpenWrite);
    fLastSaveable.Position := 0 + sizeof(flag); // FLast Saveable hat auch das Flag, aber das steht ja auf Bakup
    flag := 0; // das Savegame als Bakup markieren [0 = Normales Savegame]
    f.Write(flag, sizeof(flag));
    f.CopyFrom(fLastSaveable, fLastSaveable.Size - sizeof(flag));
    f.free;
  Except
    log('Could save file: ' + fTruncedMapfolder + Filename + '.sg', llError);
    SendSplashMessage('Could save file: ' + fTruncedMapfolder + Filename + '.sg', DefaultSplashHintDelay, v3(1, 0, 0), uid);
    LogLeave;
    exit;
  End;
  SendSplashMessage('Saved "' + Filename + '" on server.', DefaultSplashHintDelay, v3(0, 1, 0), uid);
  LogLeave;
End;

Procedure TServer.HandleRequestLoadGame(Filename: String; UID: integer);
Var
  fTruncedMapfolder: String;
  f: TFileStream;
  m: TMemoryStream;
Begin
  filename := RemoveTimestampInfoFromFilename(Filename);
  log('TServer.HandeRequestLoadGame : ' + Filename, llTrace);
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
  If FileExistsUTF8(fTruncedMapfolder + Filename) Then Begin
    f := TFileStream.Create(fTruncedMapfolder + Filename, fmOpenRead);
    m := TMemoryStream.Create;
    m.CopyFrom(f, f.Size);
    m.Position := 0;
    f.free;
    If LoadGame(m, UID) Then Begin
      fRestartWave.Clear; // Es gibt keine Runde mehr die neu gestartet werden könnte
      fLastSaveable.Clear; // Das Geladene Savegame kann regulär gespeichert werden ..
      m.Position := 0;
      fLastSaveable.CopyFrom(m, m.size);
    End;
    m.free;
  End;
  LogLeave;
End;

Procedure TServer.HandleDelSaveGame(Filename: String);
Var
  fTruncedMapfolder: String;
Begin
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
  Filename := fTruncedMapfolder + Filename + '.sg';
  If FileExistsUTF8(Filename) Then Begin
    DeleteFileUTF8(Filename);
  End;
End;

Procedure TServer.HandleDelAllSaveGames;
Var
  sl: TStringList;
  fTruncedMapfolder: String;
  i: Integer;
Begin
  fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
  While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
    delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
  End;
  fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
  sl := FindAllFiles(fTruncedMapfolder, '*.sg', false);
  For i := 0 To sl.count - 1 Do Begin
    If FileExistsUTF8(sl[i]) Then Begin
      DeleteFileUTF8(sl[i]);
    End;
  End;
  sl.free;
End;

Procedure TServer.HandleTransferCash(DestPlayer, Count, uid: integer);
Var
  i: integer;
Begin
  log('TServer.HandleTransferCash', llTrace);
  log(format('Transfer %d from %s to %s', [Count, UidToUsername(uid), UidToUsername(DestPlayer)]), llInfo);
  If (uid = DestPlayer) Then Begin
    LogLeave;
    exit;
  End;
  // 1. Abziehen
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = uid Then Begin
      fPLayer[i].gActions.Counter := fPLayer[i].gActions.Counter + 1;
      If count > fPLayer[i].Cash Then count := fPLayer[i].Cash; // Wir können nicht mehr ausgeben als wir haben
      fPLayer[i].Cash := fPLayer[i].Cash - Count;
      fPLayer[i].gMoneySpend.Counter := fPLayer[i].gMoneySpend.Counter + Count;
      fPLayer[i].OverAllMoneySpend := fPLayer[i].OverAllMoneySpend + Count;
      fPLayer[i].OverAllGiveAwayMoney := fPLayer[i].OverAllGiveAwayMoney + Count;
      break;
    End;
  End;
  // 2. gut schreiben
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = DestPlayer Then Begin
      fPLayer[i].Cash := fPLayer[i].Cash + Count;
      fPLayer[i].OverAllEarned := fPLayer[i].OverAllEarned + Count;
      fPLayer[i].gMoneyEarnded.Counter := fPLayer[i].gMoneyEarnded.Counter + Count;
      fPLayer[i].OverAllMoneyForFree := fPLayer[i].OverAllMoneyForFree + Count;
      break;
    End;
  End;
  RefreshAllPlayerStats;
  LogLeave;
End;

Procedure TServer.HandleBuildingStrategyChange(Owner: integer;
  Const Stream: TStream);
Var
  b: Boolean;
  bs: TBuildingStrategy;
  x, y: integer;
  index, streamsize: int64;
Begin
  log('TServer.HandleBuildingStrategyChange', llTrace);
  fPLayer[Owner].gActions.Counter := fPLayer[Owner].gActions.Counter + 1;
  index := stream.Position;
  streamsize := stream.Size;
  While index < streamsize Do Begin
    x := -1;
    Stream.Read(x, sizeof(x));
    y := -1;
    Stream.Read(y, sizeof(y));
    bs := bsFirst;
    b := true;
    Stream.Read(b, sizeof(b));
    Stream.Read(bs, sizeof(bs));
    fmap.ChangeBuildingStrategy(x, y, Owner, bs, b);
    inc(index, 2 * sizeof(integer));
    inc(index, sizeof(b));
    inc(index, sizeof(bs));
  End;
  LogLeave;
End;

Procedure TServer.HandleHeroStrategyChange(Owner: integer; Const Stream: TStream
  );
Var
  b: Boolean;
  bs: TBuildingStrategy;
  HeroIndex: integer;
  index, streamsize: int64;
Begin
  log('TServer.HandleBuildingStrategyChange', llTrace);
  fPLayer[Owner].gActions.Counter := fPLayer[Owner].gActions.Counter + 1;
  index := stream.Position;
  streamsize := stream.Size;
  While index < streamsize Do Begin
    HeroIndex := -1;
    Stream.Read(HeroIndex, sizeof(HeroIndex));
    bs := bsFirst;
    b := true;
    Stream.Read(b, sizeof(b));
    Stream.Read(bs, sizeof(bs));
    fmap.ChangeHeroStrategy(HeroIndex, Owner, bs, b);
    inc(index, sizeof(integer));
    inc(index, sizeof(b));
    inc(index, sizeof(bs));
  End;
  LogLeave;
End;

Procedure TServer.HandleSetHeroTargets(Owner: integer; Const Stream: TStream);
Var
  HeroIndex: uInt16;
  index, streamsize: int64;
  xs, ys: Single;
Begin
  log('TServer.HandleBuildingStrategyChange', llTrace);
  fPLayer[Owner].gActions.Counter := fPLayer[Owner].gActions.Counter + 1;
  xs := -1;
  ys := -1;
  stream.Read(xs, sizeof(xs));
  stream.Read(ys, sizeof(ys));
  index := stream.Position;
  streamsize := stream.Size;
  While index < streamsize Do Begin
    HeroIndex := 65535;
    Stream.Read(HeroIndex, sizeof(HeroIndex));
    fmap.SetheroTarget(HeroIndex, Owner, xs, ys);
    inc(index, sizeof(HeroIndex));
  End;
  LogLeave;
End;

Procedure TServer.HandleTransferCompleteMapTerrain(Const Stream: TStream);
Var
  i, j, c: integer;
Begin
  i := 0;
  j := 0;
  stream.Read(i, sizeof(i));
  stream.Read(j, sizeof(j));
  fmap.Resize(i, j);
  For i := 0 To fmap.Width - 1 Do Begin
    For j := 0 To fmap.Height - 1 Do Begin
      c := 0;
      stream.read(c, sizeof(c));
      fmap.fTerrain[i, j].data := c;
    End;
  End;
  //fMap.Save(MapName); -- Wird Automatisch gemacht
  HandleRequestMap(0, 0);
End;

Procedure TServer.HandleSendFile(MapName_, FileName: String; ResponceID,
  UID: integer);
Var
  s: String;
  f: TFileStream;
  m: TMemoryStream;
Begin
  log(format('TServer.HandleSendFile : %d, %s, %s', [uid, MapName_, FileName]), llTrace);
  s := MapFolder + MapName + PathDelim + FileName;
  f := TFileStream.Create(s, fmOpenRead);
  m := TMemoryStream.Create;
  m.WriteAnsiString(MapName_);
  m.WriteAnsiString(FileName);
  m.CopyFrom(f, f.Size);
  f.free;
  SendChunk(miFileTransfer Or ResponceID, m, uid);
  LogLeave;
End;

Procedure TServer.HandleRequestPingtimes(UID: integer);
Var
  b: Boolean;
  i: Integer;
  n: int64;
Begin
  b := false;
  For i := 0 To high(fPLayer) Do Begin
    // Merken ob schon eine Pinganfrage am laufen ist
    If fPLayer[i].RequestPing Then Begin
      b := true;
    End;
    // Merken, dass wir die Antwort haben wollen
    If fPLayer[i].UID = UID Then Begin
      fPLayer[i].RequestPing := true;
    End;
  End;
  If Not b Then Begin // Es läuft gerade keine Ping Anfrage, also eine neue starten
    // Alle Zeitbasen neu starten
    n := GetTick;
    For i := 0 To high(fPLayer) Do Begin
      fPLayer[i].PingStartTimestamp := n;
    End;
    SendChunk(miPing, Nil, 0); // Alle Anpingen
  End;
End;

Procedure TServer.HandlePing(UID: integer);
Var
  i: integer;
  n: int64;
  b: Boolean;
  s: String;
  m: TMemoryStream;
Begin
  log('TServer.HandlePing : ' + inttostr(UID), llTrace);
  n := GetTick;
  b := true;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = UID Then Begin
      fPLayer[i].PingArrived := true;
      fPLayer[i].PingStartTimestamp := n - fPLayer[i].PingStartTimestamp; // Das Delta Speichern
    End;
    If Not fPLayer[i].PingArrived Then b := false;
  End;
  If b Then Begin // Alle Mitspieler haben geantwortet, versenden der Antwort an alle die gefragt hatten
    // Den Report Aufbauen
    s := 'Ping protocol created ' + FormatDateTime('YYYY.MM.DD, HH:MM:SS', Now) + LineEnding;
    For i := 0 To high(fPLayer) Do Begin
      fPLayer[i].PingArrived := false; // Das Ping Arrived zurücksetzen, dass eine erneute Anfrage gestellt werden kann.
      s := s + format(' Server -> %s -> Server = %dms', [fPLayer[i].UserName, fPLayer[i].PingStartTimestamp]);
      If i <> High(fPLayer) Then s := s + LineEnding;
    End;
    s := Serialize(s);
    // Den Report senden, an alle die ihn wollten
    For i := 0 To high(fPLayer) Do Begin
      If fPLayer[i].RequestPing Then Begin
        fPLayer[i].RequestPing := false;
        m := TMemoryStream.Create;
        m.WriteAnsiString(s);
        SendChunk(miChatMessage, m, fPLayer[i].UID);
      End;
    End;
  End;
  LogLeave;
End;

Procedure TServer.HandleGetPlayerStats(uid: integer);
Begin
  RefreshAllPlayerStats(uid);
End;

Procedure TServer.HandleRequestPlayerPosChange(Playerindex: integer; Up: Boolean
  );
Var
  tmpP: TPlayer;
Begin
  If up Then Begin
    If (Playerindex > 0) And (Playerindex <= high(fPLayer)) Then Begin
      tmpP := fPLayer[Playerindex - 1];
      fPLayer[Playerindex - 1] := fPLayer[Playerindex];
      fPLayer[Playerindex] := tmpP;
    End;
  End
  Else Begin
    If (Playerindex >= 0) And (Playerindex < high(fPLayer)) Then Begin
      tmpP := fPLayer[Playerindex + 1];
      fPLayer[Playerindex + 1] := fPLayer[Playerindex];
      fPLayer[Playerindex] := tmpP;
    End;
  End;
  RefreshAllPlayerStats(0);
End;

Procedure TServer.HandleRequestMap(ResponceID, Uid: integer);
Var
  sl: TStringlist;
  s: String;
  i: Integer;
  m: TMemoryStream;
Begin
  log(format('TServer.HandleRequestMap : %d', [uid]), llTrace);
  If MapName <> '' Then Begin
    log(format('Player %s requests map "%s"', [UidToUsername(Uid), MapName]), llInfo);
    fMap.Save(MapName); // Speichern, damit der User auf jeden Fall die Aktuellste Version der Karte bekommt.
    fGameState := gs_EditMode;
    // 1. Alle Dateien in dem Karten Verzeichniss suchen und übertragungsanfrage an uid senden
    s := MapFolder + MapName;
    sl := findallfiles(s, '', false);
    m := TMemoryStream.Create;
    // Wir Informieren den Client über die Anzahl der zu ladenden Dateien, dass dieser einen ladebalken anzeigen kann
    i := sl.Count;
    m.Write(i, SizeOf(i));
    SendChunk(miFilesToTransmitCount Or ResponceId, m, Uid);
    For i := 0 To sl.count - 1 Do Begin
      CreateFileSendRequest(MapName, ExtractFileName(sl[i]), UID);
    End;
    sl.free;
    // Die Karte ist nun übertragen -> Starten der Karte bewirken
    m := TMemoryStream.Create;
    m.WriteAnsiString(MapName);
    SendChunk(miLoadMap Or ResponceId, m, uid);
  End;
  LogLeave;
End;

Procedure TServer.HandleRequestMapPreviewInfo(MapName_: String; ResponceID,
  Uid: integer);
Var
  m: TMemoryStream;
  s: String;
  p: TPoint;
  b: tbitmap;
  ini: TIniFile;
  i, j: integer;
  c: Char;
  LazIntfImage: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  log(format('TServer.HandleRequestMapPreviewInfo : %d, %s', [uid, MapName_]), llTrace);
  m := TMemoryStream.Create;
  ini := TIniFile.Create(MapFolder + MapName_ + PathDelim + MapRootName);
  s := DeSerialize(ini.ReadString('map', 'description', ''));
  m.WriteAnsiString(s);
  p := StringToPos(ini.ReadString('map', 'size', '0x0'));
  b := TBitmap.create;
  If (p.x <> 0) And (p.y <> 0) Then Begin
    b.Width := p.x;
    b.Height := p.y;
    LazIntfImage := TLazIntfImage.Create(0, 0);
    LazIntfImage.LoadFromBitmap(b.Handle, b.MaskHandle);
    For j := 0 To p.y - 1 Do Begin
      s := ini.ReadString('data', 'row' + inttostr(j), '');
      For i := 0 To p.x - 1 Do Begin
        If i + 1 <= length(s) Then Begin
          c := s[i + 1];
        End
        Else Begin
          c := '0';
        End;
        Case c Of
          '0', '4': LazIntfImage.Colors[i, j] := ColorToFPColor(Nichts_Col);
          '1', '5': LazIntfImage.Colors[i, j] := ColorToFPColor(Begehbar_col);
          '2', '6': LazIntfImage.Colors[i, j] := ColorToFPColor(Bebaubar_col);
          '3', '7': LazIntfImage.Colors[i, j] := ColorToFPColor(Beides_Col);
        Else Begin
            Log('Error invalid symbol rowdata ' + s, llCritical);
            LazIntfImage.Colors[i, j] := ColorToFPColor(Nichts_Col);
          End;
        End;
      End;
    End;
    LazIntfImage.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
    b.Handle := ImgHandle;
    b.MaskHandle := ImgMaskHandle;
    LazIntfImage.free;
  End;
  ini.free;
  b.SaveToStream(m);
  b.free;
  SendChunk(miRequestMapPreviewInfoResult Or ResponceID, m, Uid);
  LogLeave;
End;

Procedure TServer.HandleRequestLoadMap(MapName_: String; ResponceId,
  Uid: Integer);
Begin
  log(format('TServer.HandleRequestLoadMap : %d, %s', [Uid, MapName_]), llTrace);
  // Laden der Karte, dann Schicken an Alle
  If MapName = MapName_ Then Begin
    // Die Karte ist schon geladen, der Client macht nur nen "Refresh"
    HandleRequestMap(ResponceId, uid);
  End
  Else Begin
    If assigned(fmap) Then fmap.free;
    fmap := Tmap.create;
    fmap.Load(MapName_);
    // Wir laden eine Komplett neue Karte, also schmeißen wir alle raus
    HandleRequestMap(0, 0);
  End;
  LogLeave;
End;

Procedure TServer.HandleUpdateMapProperty(MapProperty: integer;
  Const Data: TStream; UID: integer);
Var
  s: String;
  i, j, c: integer;
  tmp: Tpoint;
  m: TMemorystream;
  bk: TBuyAbleKind;
Begin
  (*
   * Wir führen die Änderung bei uns Lokal nach und benachrichtigen Alle, dass sie dies Ändern sollen
   *)
  log(format('TServer.HandleUpdateMapeProperty : %d, %s', [UID, MessageMapPropertyToString(MapProperty)]), llTrace);
  (*
   * Der Code in HandleUpdateMapProperty.inc übernimmt die Informationen aus Data und schreibt sie in FMap
   * dies ist bei Server und Client Identisch, zur Reduktion von Redundantem
   * Code ist aus diesem Grund der Folgende Code in das .inc ausgelagert.
   *)
{$I ../client/HandleUpdateMapProperty.inc}
  If MapProperty = mpSaveMap Then Begin
    fMap.Save(MapName);
  End;
  // Da Data aus HandleReceived kommt, ist der Stream nachher ungültig, also muss hier eine Kopie übergeben werden.
  m := TMemoryStream.Create;
  data.Position := 0;
  m.CopyFrom(data, data.Size);
  SendChunk(miUpdateMapProperty, m, 0); // was wir bekommen haben schicken wir an alle
  LogLeave;
End;

Procedure TServer.HandleChatMessage(Text: String; Uid: integer);
Var
  w: Integer;
  m: TMemoryStream;
Begin
  log('TServer.HandleChatMessage : ' + text, llTrace);
  For w := 0 To high(fPLayer) Do
    If fPLayer[w].UID = UID Then Begin
      text := fPLayer[w].UserName + ' : ' + text;
      break;
    End;
  m := TMemoryStream.Create;
  m.WriteAnsiString(text);
  SendChunk(miChatMessage, m, 0);
  LogLeave;
End;

Procedure TServer.HandleKickPlayerOut(PlayerName: String; Uid: integer);
Var
  Player_Uid, i, j: Integer;
Begin
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UserName = PlayerName Then Begin
      (*
       * Info an Alle
       *)
      For j := 0 To high(fPLayer) Do Begin
        If fPLayer[j].UID = Uid Then Begin
          SendSplashMessage('Player: ' + fPLayer[j].UserName + ' kicked player ' + PlayerName + ' out of the game.', DefaultSplashHintDelay, DefaultSplashHintColor, -fPLayer[i].UID);
          log('Player: ' + fPLayer[j].UserName + ' kicked player ' + PlayerName + ' out of the game.', llInfo);
          break;
        End;
      End;
      (*
       * Der Eigentliche Raus wurf
       *)
      Player_Uid := fPLayer[i].UID;
      PlayerLeaves(fPLayer[i].UID);
      (*
       * Hier nehmen wir Absichtlich nicht das SendChunk, damit es keine Fehlermeldung gibt, sollte der Spieler das Spiel bereits verlassen haben
       *)
      fChunkManager.SendChunk(miYouWereKickedOut, Nil, Player_Uid);
      break;
    End;
  End;
End;

Procedure TServer.HandleBuyBuilding(x, y: integer; Bname: String; Uid: integer);
Var
  bool: Boolean;
  b: TBuilding;
  PlayerIndex, i: integer;
  m: TMemoryStream;
Begin
  log(format('TServer.HandleBuyBuilding : %d, %d/%d %s', [Uid, x, y, Bname]), llTrace);
  PlayerIndex := -1;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = Uid Then Begin
      PlayerIndex := i;
      break;
    End;
  End;
  If PlayerIndex = -1 Then Begin
    log('Could not find uid in playerlist.', llCritical);
    logleave;
    exit;
  End;
  fPLayer[PlayerIndex].gActions.Counter := fPLayer[PlayerIndex].gActions.Counter + 1;
  b := TBuilding.create();
  b.LoadFromFile(MapFolder + MapName + PathDelim + Bname);
  b.Owner := PlayerIndex;
  If fPLayer[PlayerIndex].Cash - b.Stages[0].Cost >= 0 Then Begin
    If fmap.AddBuilding(x, y, b) Then Begin
      fPLayer[PlayerIndex].Cash := fPLayer[PlayerIndex].Cash - b.Stages[0].Cost;
      fPLayer[PlayerIndex].OverAllMoneySpend := fPLayer[PlayerIndex].OverAllMoneySpend + b.Stages[0].Cost;
      fPLayer[PlayerIndex].gMoneySpend.Counter := fPLayer[PlayerIndex].gMoneySpend.Counter + b.Stages[0].Cost;
      fPLayer[PlayerIndex].OverAllBuildingsBuild := fPLayer[PlayerIndex].OverAllBuildingsBuild + 1;
      // Tracken das der Spieler dieses Gebäude gekauft hat
      bool := false;
      For i := 0 To high(fPLayer[PlayerIndex].BuildingsBuild) Do Begin
        If (fPLayer[PlayerIndex].BuildingsBuild[i].BildingFilename = Bname) Then Begin
          fPLayer[PlayerIndex].BuildingsBuild[i].Count := fPLayer[PlayerIndex].BuildingsBuild[i].Count + 1;
          bool := true;
          break;
        End;
      End;
      // Der Spieler Baut dieses Gebäude zum ersten Mal, also legen wir es an.
      If Not bool Then Begin
        setlength(fPLayer[PlayerIndex].BuildingsBuild, high(fPLayer[PlayerIndex].BuildingsBuild) + 2);
        fPLayer[PlayerIndex].BuildingsBuild[high(fPLayer[PlayerIndex].BuildingsBuild)].Count := 1;
        fPLayer[PlayerIndex].BuildingsBuild[high(fPLayer[PlayerIndex].BuildingsBuild)].BildingFilename := Bname;
        fPLayer[PlayerIndex].BuildingsBuild[high(fPLayer[PlayerIndex].BuildingsBuild)].BuildingName := b.name;
      End;
      // Nachricht An Alle, dass es ein Gebäude mehr auf der Karte gibt
      m := TMemoryStream.Create;
      m.WriteAnsiString(Bname);
      m.Write(x, SizeOf(x));
      m.Write(y, SizeOf(y));
      m.Write(PlayerIndex, SizeOf(PlayerIndex)); // Damit die Hints später auch den Richtigen Besitzer anzeigen können, und wegen der Verkauferei
      SendChunk(miAddBuilding, m, 0);
      RefreshAllPlayerStats; // Die Kohle Updaten
    End
    Else Begin
      // Nur der Bauende Spieler bekommt die Absage
      SendSplashMessage('Error, not allowed to place building here.', DefaultSplashHintDelay, DefaultSplashHintColor, Uid);
      b.free;
    End;
  End
  Else Begin
    // !! ACHTUNG !! Diese Fehlermeldung sollte Identisch zu der im Client sein.
    SendSplashMessage('Error, not enough money to buy ' + B.name, DefaultSplashHintDelay, DefaultSplashHintColor, Uid);
    b.free;
  End;
  LogLeave;
End;

Procedure TServer.HandleBuyHero(x, y: integer; Bname: String; Uid: integer);
Var
  h: Thero;
  PlayerIndex, i: integer;
  m: TMemoryStream;
Begin
  log(format('TServer.HandleBuyHero : %d, %d/%d %s', [Uid, x, y, Bname]), llTrace);
  PlayerIndex := -1;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = Uid Then Begin
      PlayerIndex := i;
      break;
    End;
  End;
  If PlayerIndex = -1 Then Begin
    log('Could not find uid in playerlist.', llCritical);
    logleave;
    exit;
  End;
  fPLayer[PlayerIndex].gActions.Counter := fPLayer[PlayerIndex].gActions.Counter + 1;
  h := THero.create();
  h.LoadFromFile(MapFolder + MapName + PathDelim + Bname);
  h.Owner := PlayerIndex;
  If fPLayer[PlayerIndex].Cash - h.Cost >= 0 Then Begin
    If fmap.AddHero(x, y, h) Then Begin
      fPLayer[PlayerIndex].Cash := fPLayer[PlayerIndex].Cash - h.Cost;
      fPLayer[PlayerIndex].OverAllMoneySpend := fPLayer[PlayerIndex].OverAllMoneySpend + h.Cost;
      fPLayer[PlayerIndex].gMoneySpend.Counter := fPLayer[PlayerIndex].gMoneySpend.Counter + h.Cost;
      //      fPLayer[PlayerIndex].OverAllBuildingsBuild := fPLayer[PlayerIndex].OverAllBuildingsBuild + 1;
      // Tracken das der Spieler dieses Gebäude gekauft hat
      //      bool := false;
      //      For i := 0 To high(fPLayer[PlayerIndex].BuildingsBuild) Do Begin
      //        If (fPLayer[PlayerIndex].BuildingsBuild[i].BildingFilename = Bname) Then Begin
      //          fPLayer[PlayerIndex].BuildingsBuild[i].Count := fPLayer[PlayerIndex].BuildingsBuild[i].Count + 1;
      //          bool := true;
      //          break;
      //        End;
      //      End;
      //      // Der Spieler Baut dieses Gebäude zum ersten Mal, also legen wir es an.
      //      If Not bool Then Begin
      //        setlength(fPLayer[PlayerIndex].BuildingsBuild, high(fPLayer[PlayerIndex].BuildingsBuild) + 2);
      //        fPLayer[PlayerIndex].BuildingsBuild[high(fPLayer[PlayerIndex].BuildingsBuild)].Count := 1;
      //        fPLayer[PlayerIndex].BuildingsBuild[high(fPLayer[PlayerIndex].BuildingsBuild)].BildingFilename := Bname;
      //        fPLayer[PlayerIndex].BuildingsBuild[high(fPLayer[PlayerIndex].BuildingsBuild)].BuildingName := h.name;
      //      End;
     // Nachricht An Alle, dass es ein Gebäude mehr auf der Karte gibt
      m := TMemoryStream.Create;
      m.WriteAnsiString(Bname);
      m.Write(x, SizeOf(x));
      m.Write(y, SizeOf(y));
      m.Write(PlayerIndex, SizeOf(PlayerIndex)); // Damit die Hints später auch den Richtigen Besitzer anzeigen können, und wegen der Verkauferei
      SendChunk(miAddHero, m, 0);
      RefreshAllPlayerStats; // Die Kohle Updaten
    End
    Else Begin
      // Nur der Bauende Spieler bekommt die Absage
      SendSplashMessage('Error, not allowed to place hero here.', DefaultSplashHintDelay, DefaultSplashHintColor, Uid);
      h.free;
    End;
  End
  Else Begin
    // !! ACHTUNG !! Diese Fehlermeldung sollte Identisch zu der im Client sein.
    SendSplashMessage('Error, not enough money to buy ' + h.name, DefaultSplashHintDelay, DefaultSplashHintColor, Uid);
    h.free;
  End;
  LogLeave;
End;

Procedure TServer.HandleSellBuilding(Owner: integer; Const Stream: TStream);
Var
  b: tctd_mapopbject;
  invests, i: Integer;
  m: TMemoryStream;
  x, y: integer;
  index, streamsize: int64;
Begin
  log('TServer.HandleSellBuilding', llTrace);
  fPLayer[Owner].gActions.Counter := fPLayer[Owner].gActions.Counter + 1;
  index := stream.Position;
  streamsize := stream.Size;
  m := TMemoryStream.Create;
  While index < streamsize Do Begin
    x := -1;
    y := -1;
    stream.Read(x, sizeof(x));
    stream.Read(y, sizeof(y));

    b := fmap.GetObjUnderCursor(x * MapBlockSize, y * MapBlockSize);
    If (Not assigned(b)) Or (Not (b Is TBuilding)) Then Begin
      log(format('Could not locate a building on %d/%d', [x, y]), llWarning);
      LogLeave;
      exit;
    End;
    // Was hat uns der Turm bisher gekostet
    invests := 0;
    For i := 0 To TBuilding(b).Stage Do Begin
      invests := invests + TBuilding(b).Stages[i].Cost;
    End;
    invests := max(0, round(invests * TBuilding(b).Refund[fMap.Difficulty] / 100)); // Verkaufen gibt bestenfalls kein Geld, aber nie Schulden
    If fMap.RemoveBuilding(x, y, Owner) Then Begin
      fPLayer[Owner].Cash := fPLayer[Owner].Cash + invests;
      fPLayer[Owner].OverAllEarned := fPLayer[Owner].OverAllEarned + invests;
      fPLayer[Owner].gMoneyEarnded.Counter := fPLayer[Owner].gMoneyEarnded.Counter + invests;
      fPLayer[Owner].OverAllBuildingsSold := fPLayer[Owner].OverAllBuildingsSold + 1;
      m.Write(x, sizeof(x));
      m.Write(y, sizeof(y));
    End;
    inc(index, 2 * sizeof(integer));
  End;
  If m.Size <> 0 Then Begin
    SendChunk(miRemoveBuilding, m, 0);
    RefreshAllPlayerStats;
  End
  Else Begin
    m.free;
  End;
  LogLeave;
End;

Procedure TServer.HandleLevelUpBuilding(Owner: integer; Const Stream: TStream);
Var
  b: tctd_mapopbject;
  cost: integer;
  m: TMemoryStream;
  x, y: integer;
  index, streamsize: int64;
Begin
  log('TServer.HandleLevelUpBuilding', llTrace);
  fPLayer[Owner].gActions.Counter := fPLayer[Owner].gActions.Counter + 1;
  index := stream.Position;
  streamsize := stream.Size;
  m := TMemoryStream.Create;
  While index < streamsize Do Begin
    x := -1;
    y := -1;
    stream.Read(x, sizeof(x));
    stream.Read(y, sizeof(y));
    b := fmap.GetObjUnderCursor(x * MapBlockSize, y * MapBlockSize);
    If (Not assigned(b)) Or (Not (b Is TBuilding)) Then Begin
      log(format('Could not locate a building on %d/%d', [x, y]), llWarning);
      LogLeave;
      exit;
    End;
    If TBuilding(b).Stage < high(TBuilding(b).Stages) Then Begin // kann das Gebäude überhaupt noch geupt werden ?
      // Kann sich der Spieler das Update überhaupt leisten ?
      cost := TBuilding(b).Stages[TBuilding(b).Stage + 1].Cost;
      If fPLayer[Owner].Cash - Cost >= 0 Then Begin
        // Ja, na dann soll er Updaten
        If TBuilding(b).IncStage Then Begin
          log('Building Level Up granted.', llDebug);
          fPLayer[Owner].Cash := fPLayer[Owner].Cash - Cost;
          fPLayer[Owner].OverAllMoneySpend := fPLayer[Owner].OverAllMoneySpend + cost;
          fPLayer[Owner].gMoneySpend.Counter := fPLayer[Owner].gMoneySpend.Counter + cost;
          fPLayer[Owner].OverAllBuildingsLeveledUp := fPLayer[Owner].OverAllBuildingsLeveledUp + 1;
          m.Write(x, sizeof(x));
          m.Write(y, sizeof(y));
        End;
      End;
    End;
    inc(index, 2 * sizeof(integer));
  End;
  If m.Size <> 0 Then Begin
    SendChunk(miLevelUpBuilding, m, 0);
    RefreshAllPlayerStats;
  End
  Else Begin
    m.free;
  End;
  LogLeave;
End;

Procedure TServer.HandleReceiveHeartBeat(p: integer; t: int64);
Begin
  If fGameState <> gs_Gaming Then exit;
  If (p < 0) Or (p > high(fPLayer)) Then Begin
    log('Invalid player index for heartbeat : ' + IntToStr(p), llFatal);
    exit;
  End;
  fPLayer[p].LastSynchronTimeStamp := t;
End;

Procedure TServer.HandleSendLastWaveToAll;
(*
 * Der miUpdateMapProperty Stream ist wie folgt aufgebaut
 * <integer= MapProperty-ID>
 * <integer= Sender ID -> immer 0>
 * <integer= WaveNum [0-Bassiert]>
 * [Jeweilige WaveProp Nutzdaten]
 *)
  Function NewStream(ID: Integer; Data: Array Of integer): TMemoryStream;
  Var
    h, i: integer;
  Begin
    result := TMemoryStream.Create;
    result.Write(id, sizeof(id));
    id := 0;
    result.Write(id, sizeof(id));
    For i := 0 To high(data) Do Begin
      h := data[i];
      result.Write(h, sizeof(h));
    End;
  End;

Var
  m: TMemoryStream;
  n, i: Integer;
Begin
  // Die "Höchste" Wave der Karte muss an alle Verteilt werden
  If Not assigned(fMap) Then exit;
  n := high(fMap.Waves);
  // 1. Die WaveCount Nummer erhöhen
  m := NewStream(mpWaveCount, [n + 1]);
  SendChunk(miUpdateMapProperty, m, 0);

  m := NewStream(mpCashOnWaveStart, [n, fMap.Waves[n].ChashOnStart]);
  SendChunk(miUpdateMapProperty, m, 0);

  m := NewStream(mpWaveHint, [n]);
  m.WriteAnsiString(fMap.Waves[n].WaveHint);
  SendChunk(miUpdateMapProperty, m, 0);

  m := NewStream(mpWaveOpponents, [n, length(fMap.Waves[n].Opponents)]);
  SendChunk(miUpdateMapProperty, m, 0);

  For i := 0 To high(fMap.Waves[n].Opponents) Do Begin
    m := NewStream(mpWaveOpponent, [n, i]);
    m.WriteAnsiString(fMap.Waves[n].Opponents[i].opponent);
    SendChunk(miUpdateMapProperty, m, 0);

    m := NewStream(mpWaveOpponentCount, [n, i, fMap.Waves[n].Opponents[i].Count]);
    SendChunk(miUpdateMapProperty, m, 0);

    m := NewStream(mpWaveOpponentCashPerUnit, [n, i, fMap.Waves[n].Opponents[i].refund]);
    SendChunk(miUpdateMapProperty, m, 0);

    m := NewStream(mpWaveOpponentUnitsPerSpawn, [n, i, fMap.Waves[n].Opponents[i].UnitsPerSpawn]);
    SendChunk(miUpdateMapProperty, m, 0);

    m := NewStream(mpWaveOpponentSpawnDelta, [n, i, fMap.Waves[n].Opponents[i].Spawndelta]);
    SendChunk(miUpdateMapProperty, m, 0);

    m := NewStream(mpWaveOpponentSpawnDelay, [n, i, fMap.Waves[n].Opponents[i].SpawnDelay]);
    SendChunk(miUpdateMapProperty, m, 0);
  End;
End;

Procedure TServer.HandleStartRound;
Var
  n: int64;
  i: integer;
  m: TMemoryStream;
Begin
  log('TServer.HandleStartRound', llTrace);
  // Den Clients mitteilen dass es nun Los geht
  SendChunk(miStart, Nil, 0);
  // Umschalten in den Spiel Modus
  fGameState := gs_Gaming;
  // Evtl. Die Wave hint Anzeigen
  If fMap.Waves[fAktualRound].WaveHint <> '' Then Begin
    SendSplashMessage(fMap.Waves[fAktualRound].WaveHint, min(20000, length(fMap.Waves[fAktualRound].WaveHint) * 250), DefaultSplashHintColor, 0);
  End;
  // Alle Zeitbasen Starten die es so zu starten gibt, gleichzeitig werden "alte" Pausen genullt
  fpausing := false;
  fSyncPause := false;
  foldpausevalue := false;
  n := GetTick;
  If Speedup <> 1 Then Begin
    Speedup := 1;
    m := TMemoryStream.Create;
    m.Write(Speedup, sizeof(Speedup));
    SendChunk(miSetSpeedUp, m, 0);
  End;
  For i := 0 To high(fplayer) Do Begin
    fplayer[i].LastSynchronTimeStamp := n;
  End;
  FLastFrameTimestamp := n;
  fLastClientUpdateTimestamp := n;
  fLastHeartbeatTimestamp := n;
  fmap.Pause(false);
  fmap.Start();
  fSpawnModul.Pause(false);
  fSpawnModul.Start();
  fFrameLog.TimeStamp := n;
  fFrameLog.AccumulatedSize := 0;
  fFrameLog.Count := 0;
  LogLeave;
End;

Procedure TServer.RefreshAllPlayerStats(Uid: integer);
Var
  m: TMemoryStream;
  j, i: Integer;
Begin
  log('TServer.RefreshAllPlayerStats', llTrace);
  m := TMemoryStream.Create;
  // Einfügen aller Spielerinformationen, dass diese übernommen werden können (z.B. nach Load Game)
  j := length(fPLayer);
  m.write(j, sizeof(j));
  For i := 0 To high(fPLayer) Do Begin
    j := fPLayer[i].UID;
    m.write(j, sizeof(j));
    j := fPLayer[i].Cash;
    m.write(j, sizeof(j));
    j := fPLayer[i].Lives;
    m.write(j, sizeof(j));
    j := fPLayer[i].Kills;
    m.write(j, sizeof(j));
    j := fPLayer[i].BonusFinisher;
    m.write(j, sizeof(j));
    m.WriteAnsiString(fPLayer[i].UserName);
  End;
  SendChunk(miRefreshPlayerStats, m, UID);
  LogLeave;
End;

Procedure TServer.HandleInitiateNewRound(Difficulty, Round, UID: integer);

  Function DifficultyToString(Level: Integer): String;
  Begin
    result := 'Unknown level';
    Case Level Of
      0: result := 'Easy';
      1: result := 'Normal';
      2: result := 'Hard';
    End;
  End;

Var
  i: integer;
  m: TMemoryStream;
  r: TCheckResult;
Begin
  log(format('TServer.HandleInitiateNewRound : %d, %d, %d', [UID, Difficulty, Round]), llTrace);
  // 1. Speichern
  fMap.Save(MapName);
  // 2.1 Alles Prüfen
  r := fMap.CheckForErrors(false);
  If r.Errors Then Begin
    SendSplashMessage('Map contains errors, could not start.', DefaultSplashHintDelay, v3(1, 0, 0), uid);
    fGameState := gs_EditMode;
    SendChunk(miForceEditMode, Nil, 0);
    LogLeave;
    exit;
  End;
  If round > high(fMap.Waves) Then Begin
    SendSplashMessage(format('Wave %d does not exist in map.', [Round + 1]), DefaultSplashHintDelay, v3(1, 0, 0), uid);
    fGameState := gs_EditMode;
    SendChunk(miForceEditMode, Nil, 0);
    LogLeave;
    exit;
  End;
  // 2.2 Prüfen ob auch die Passende Anzahl an Mitspielern mit spielt
  If length(fPLayer) > fmap.MaxPlayer Then Begin
    SendSplashMessage(format('Map does not Support %d player', [length(fPLayer)]), DefaultSplashHintDelay, v3(1, 0, 0), uid);
    fGameState := gs_EditMode;
    SendChunk(miForceEditMode, Nil, 0);
    LogLeave;
    exit;
  End;

  // 4. Spiel Starten
  log(format('Start round : %d on map %s', [round + 1, MapName]), llInfo);
  fAktualRound := round;
  fRestartWave.clear;
  If fAktualRound <> 0 Then Begin
    SaveGame(fRestartWave); // das Vorherige Spiel
  End
  Else Begin
    fLastSaveable.clear;
  End;

  If fAktualRound = 0 Then Begin
    // Wir starten ein Komplett neues Spiel
    fOverAllGameingTime := 0;
    fOverAllRealGameingTime := 0;
    fOverAllPausingTime := 0;
    fCounterfor10s := 0;
    HandleResetPlayerStatisikAndValues(Difficulty, fmap.MapType);
    Handle10sStatistikEvaluation(); // Alles mit "0" Initialisieren
  End;
  For i := 0 To high(fPLayer) Do Begin
    If fMap.waves[fAktualRound].ChashOnStart >= 0 Then Begin
      fPLayer[i].OverAllEarned := fPLayer[i].OverAllEarned + fMap.waves[fAktualRound].ChashOnStart;
      fPLayer[i].gMoneyEarnded.Counter := fPLayer[i].gMoneyEarnded.Counter + fMap.waves[fAktualRound].ChashOnStart;
    End
    Else Begin
      If fPLayer[i].Cash > abs(fMap.waves[fAktualRound].ChashOnStart) Then Begin
        fPLayer[i].OverAllMoneySpend := fPLayer[i].OverAllMoneySpend + abs(fMap.waves[fAktualRound].ChashOnStart);
        fPLayer[i].gMoneySpend.Counter := fPLayer[i].gMoneySpend.Counter + abs(fMap.waves[fAktualRound].ChashOnStart);
      End
      Else Begin
        fPLayer[i].OverAllMoneySpend := fPLayer[i].OverAllMoneySpend + fPLayer[i].Cash;
        fPLayer[i].gMoneySpend.Counter := fPLayer[i].gMoneySpend.Counter + fPLayer[i].Cash;
      End;
    End;
    fPLayer[i].Cash := max(0, fPLayer[i].Cash + fMap.waves[fAktualRound].ChashOnStart); // Sollte der Kartenersteller Geld Apziehen, kann dies maximal bis 0 abgezogen werden.
  End;
  fGameState := gs_WaitToStart;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = UID Then Begin
      If fAktualRound = 0 Then Begin
        SendSplashMessage(format('%s has started a new game with difficulty level: "%s", please wait until loading is finished.', [fPLayer[i].UserName, DifficultyToString(Difficulty)]), DefaultSplashHintDelay, v3(1, 1, 1), 0);
      End
      Else Begin
        SendSplashMessage(format('%s has started wave %d, please wait until loading is finished.', [fPLayer[i].UserName, fAktualRound + 1]), DefaultSplashHintDelay, v3(1, 1, 1), 0);
      End;
      break;
    End;
  End;
  fLastUpdateDataSend := false;
  // 4.1 Allen Teilnehmern Mitteilen dass sie die Karte Laden sollen und auf den Spielbegin warten sollen
  m := TMemoryStream.Create;
  m.Write(fAktualRound, sizeof(fAktualRound));
  m.Write(Difficulty, SizeOf(Difficulty)); // Allen Clients mitteilen auf welchem Difficulty die Runde gestartet wurde..
  // 4.2 Initialisieren des Wartens auf Ack von Allen Teilnehmern
  // Todo : Einbaun eines Timeouts, sonst sind wir hier in einer Endlosschleife
  For i := 0 To high(fplayer) Do Begin // Init des Warten auf Alle Spieler
    fPLayer[i].ready := false;
  End;
  RefreshAllPlayerStats; // Zuerst alle Statistiken Aktualisieren,
  SendChunk(miStartRound, m, 0); // Allen Spielern Mitteilen das es los geht
  // 4.3 Alles Laden was es zu laden gibt
  fmap.Difficulty := Difficulty;
  If Round = 0 Then Begin
    fmap.ResetAllBuyedObjects;
  End;
  fmap.ResetAllMovingObjects;
  fmap.CalcOpponentPaths;
  fmap.CreateMovableObjectList(fAktualRound);
  fSpawnModul.Init(fmap, fAktualRound, length(Fplayer));
  LogLeave;
End;

Procedure TServer.HandleOnEndRound(Succeed: Boolean);
Var
  m: TMemoryStream;
Begin
  log('TServer.HandleOnEndRound : ' + inttostr(ord(Succeed)), llTrace);
  m := TMemoryStream.Create;
  m.Write(Succeed, SizeOf(Succeed));
  m.Write(fAktualRound, SizeOf(fAktualRound));
  If (fAktualRound >= high(fMap.Waves)) Or (Not Succeed) Then Begin
    // Die Letzte Laufende Zeitscheibe nehmen wir noch mit ;-)
    If fCounterfor10s <> 0 Then Begin
      Handle10sStatistikEvaluation();
      fCounterfor10s := 0;
    End;
    AppendGameStatistics(m);
  End;
  SendChunk(miEndRound, m, 0);
  If Succeed Then Begin
    // Die Runde war erfolgreich, also abspeichern in Saveable
    fLastSaveable.Clear;
    SaveGame(fLastSaveable); // Die Savegames werden ja via Continue weitergespielt, also darf hier noch keine Wave hochgezählt werden.
    BakupWave();
    fAktualRound := fAktualRound + 1;
    If fAktualRound <= high(fMap.Waves) Then Begin
      HandleInitiateNewRound(fMap.Difficulty, fAktualRound, 0); // Automatisch die Nächste Runde
    End
    Else Begin
      fAktualRound := fAktualRound - 1; // Es gibt ja keine weitere Wave mehr, und das Continue zählt wieder eins hoch
      HandleOnEndGame();
      SendSplashMessage('All waves played successfully.', DefaultSplashHintDelay * 3, v3(1, 1, 0), 0);
      fGameState := gs_EditMode;
      SendChunk(miForceEditMode, Nil, 0);
    End;
  End
  Else Begin
    HandleOnEndGame();
    fGameState := gs_EditMode;
  End;
  LogLeave;
End;

Procedure TServer.HandleOnEndGame;
Var
  PlayerNames: Array Of String;

  Procedure QuickNames(li, re: integer);
  Var
    l, r: Integer;
    h, p: String;
  Begin
    If Li < Re Then Begin
      p := PlayerNames[Trunc((li + re) / 2)]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(PlayerNames[l], p) < 0 Do
          inc(l);
        While CompareStr(PlayerNames[r], p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := PlayerNames[l];
          PlayerNames[l] := PlayerNames[r];
          PlayerNames[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      QuickNames(li, r);
      QuickNames(l, re);
    End;
  End;

Var
  i: Integer;
  pts: Int64;
  ts: TDateTime;
  pn: String;
Begin
  // Aktualisieren der Highscore der Karte
  ts := Now();
  If (fMap.MapType = mtCoop) Or (fMap.MapType = mtCoopMaze) Then Begin
    // Sortieren aller Spielernamen
    PlayerNames := Nil;
    setlength(PlayerNames, length(fPLayer));
    For i := 0 To high(fPLayer) Do Begin
      PlayerNames[i] := fPLayer[i].UserName;
    End;
    QuickNames(0, high(PlayerNames));
    pn := '';
    For i := 0 To high(PlayerNames) Do Begin
      pn := pn + PlayerNames[i];
      If i <> high(PlayerNames) Then Begin
        pn := pn + ', ';
      End;
    End;
    pts := CalcHighScoreForPlayer(0); // Im Coop haben alle die gleichen Punkte
    fMap.AddHighScore(pn, ts, pts, fOverAllGameingTime Div 1000, fAktualRound + 1, fMap.Difficulty, length(fPLayer), fPLayer[0].LivesLost);
  End
  Else Begin
    For i := 0 To high(fPLayer) Do Begin
      pts := CalcHighScoreForPlayer(i);
      fMap.AddHighScore(fPLayer[i].UserName, ts, pts, fOverAllGameingTime Div 1000, fAktualRound + 1, fMap.Difficulty, length(fPLayer), fPLayer[i].LivesLost);
    End;
  End;
  // Die Karte muss natürlich auch gespeichert werden ;)
  fMap.Save(MapName);
End;

Procedure TServer.HandleAbortWave(UID: integer);
Var
  m: TMemoryStream;
Begin
  log('TServer.HandleAbortWave', llTrace);
  SendSplashMessage(format('Player %s canceled wave.', [UidToUsername(uid)]), DefaultSplashHintDelay, v3(1, 0, 0), uid);
  fGameState := gs_EditMode;
  fAktualRound := fAktualRound - 1; // Sonst stimmt Continue nicht
  // Ist noch irgend ein Gebäude am Bauen, dann Brechen wir alle erst mal Ab
  m := fmap.ForceBuildingsBuildReady;
  If m.Size > 0 Then Begin
    SendChunk(miSetBuildingsToStage, m, 0);
  End
  Else Begin
    m.free;
  End;
  // Ist noch ein Held am Bauen, dann brechen wir alle erst mal ab
  m := fmap.ForceHerosReady;
  If m.Size > 0 Then Begin
    SendChunk(miSetHerosToLevel, m, 0);
  End
  Else Begin
    m.free;
  End;
  SendChunk(miForceEditMode, Nil, 0);
  LogLeave;
End;

Procedure TServer.HandleRestartLastWave(UID: integer);
Begin
  If fRestartWave.Size <> 0 Then Begin
    fRestartWave.Position := 0;
    If LoadGame(fRestartWave, UID) Then Begin
      HandleInitiateNewRound(fMap.Difficulty, fAktualRound, UID);
    End;
  End;
End;

Procedure TServer.Handle10sStatistikEvaluation;
Var
  i: Integer;
Begin
  For i := 0 To high(fPLayer) Do Begin
    fPLayer[i].gActions.Data[fPLayer[i].gActions.Ptr] := fPLayer[i].gActions.Counter;
    fPLayer[i].gActions.Counter := 0;
    fPLayer[i].gActions.Ptr := fPLayer[i].gActions.Ptr + 1;
    If fPLayer[i].gActions.Ptr > high(fPLayer[i].gActions.Data) Then Begin
      setlength(fPLayer[i].gActions.Data, length(fPLayer[i].gActions.Data) + 1024);
    End;

    fPLayer[i].gKills.Data[fPLayer[i].gKills.Ptr] := fPLayer[i].gKills.Counter;
    fPLayer[i].gKills.Counter := 0;
    fPLayer[i].gKills.Ptr := fPLayer[i].gKills.Ptr + 1;
    If fPLayer[i].gKills.Ptr > high(fPLayer[i].gKills.Data) Then Begin
      setlength(fPLayer[i].gKills.Data, length(fPLayer[i].gKills.Data) + 1024);
    End;

    fPLayer[i].gMoneyEarnded.Data[fPLayer[i].gMoneyEarnded.Ptr] := fPLayer[i].gMoneyEarnded.Counter;
    //fPLayer[i].gMoneyEarnded.Counter := 0;
    fPLayer[i].gMoneyEarnded.Ptr := fPLayer[i].gMoneyEarnded.Ptr + 1;
    If fPLayer[i].gMoneyEarnded.Ptr > high(fPLayer[i].gMoneyEarnded.Data) Then Begin
      setlength(fPLayer[i].gMoneyEarnded.Data, length(fPLayer[i].gMoneyEarnded.Data) + 1024);
    End;

    fPLayer[i].gMoneySpend.Data[fPLayer[i].gMoneySpend.Ptr] := fPLayer[i].gMoneySpend.Counter;
    //fPLayer[i].gMoneySpend.Counter := 0;
    fPLayer[i].gMoneySpend.Ptr := fPLayer[i].gMoneySpend.Ptr + 1;
    If fPLayer[i].gMoneySpend.Ptr > high(fPLayer[i].gMoneySpend.Data) Then Begin
      setlength(fPLayer[i].gMoneySpend.Data, length(fPLayer[i].gMoneySpend.Data) + 1024);
    End;

  End;
End;

Procedure TServer.HandleResetPlayerStatisikAndValues(Difficulty: integer;
  Maptype: TMapType);
Var
  i: Integer;
Begin
  For i := 0 To high(fPLayer) Do Begin
    fPLayer[i].OverAllEarned := 0;
    fPLayer[i].OverAllMoneySpend := 0;
    fPLayer[i].OverAllMoneyForFree := 0;
    fPLayer[i].OverAllGiveAwayMoney := 0;
    fPLayer[i].OverAllBuildingsBuild := 0;
    fPLayer[i].OverAllBuildingsSold := 0;
    fPLayer[i].OverAllBuildingsLeveledUp := 0;
    fPLayer[i].DamageDealt[0] := 0;
    fPLayer[i].DamageDealt[1] := 0;
    fPLayer[i].DamageDealt[2] := 0;
    fPLayer[i].DamageDealt[3] := 0;
    fPLayer[i].Cash := 0;
    fPLayer[i].Kills := 0;
    fPLayer[i].BonusFinisher := 0;
    fPLayer[i].LivesLost := 0;
    setlength(fPLayer[i].BuildingsBuild, 0);
    // Im Singleplayer Modus hat jeder seine Leben
    If (MapType = mtSingle) Or (MapType = mtSingleMaze) Then Begin
      fPLayer[i].Lives := fmap.Lives[Difficulty];
    End
    Else Begin
      // Im Multiplayer Modus haben alle gemeinsam Leben
      fPLayer[i].Lives := fmap.Lives[Difficulty] * length(fPLayer);
    End;
    // Die Dynamischen Statistik Sachen ..
    fPLayer[i].gActions.Counter := 0;
    fPLayer[i].gActions.Ptr := 0;
    setlength(fPLayer[i].gActions.Data, 1024);
    fPLayer[i].gKills.Counter := 0;
    fPLayer[i].gKills.Ptr := 0;
    setlength(fPLayer[i].gKills.Data, 1024);
    fPLayer[i].gMoneyEarnded.Counter := 0;
    fPLayer[i].gMoneyEarnded.Ptr := 0;
    setlength(fPLayer[i].gMoneyEarnded.Data, 1024);
    fPLayer[i].gMoneySpend.Counter := 0;
    fPLayer[i].gMoneySpend.Ptr := 0;
    setlength(fPLayer[i].gMoneySpend.Data, 1024);
  End;
End;

Procedure TServer.ApplyPause(Value: Boolean);
Var
  t: int64;
  i: integer;
Begin
  value := value Or fSyncPause;
  fmap.Pause(Value);
  fSpawnModul.Pause(Value);
  If Value Then Begin // Wir Starten die Pause
    If Not foldpausevalue Then Begin
      fPauseTimestamp := GetTick();
    End;
  End
  Else Begin //Wir Beenden die Pause, nun müssen alle Zeitbasen passend Verschoben werden
    If foldpausevalue Then Begin
      t := GetTick() - fPauseTimestamp; // T = Zeit in ms wie lange die Pause gedauert hat
      FLastFrameTimestamp := FLastFrameTimestamp + t;
      fLastClientUpdateTimestamp := fLastClientUpdateTimestamp + t;
      // fLastActiveTickTimestamp := fLastActiveTickTimestamp + t; -- Egal ob das gemacht wird oder nicht ..
      (*
       * Verschieben aller Zeitbasen für die Bandbreitenlimitierung
       *)
      fLastHeartbeatTimestamp := fLastHeartbeatTimestamp + t;
      For i := 0 To high(fPLayer) Do Begin
        fPLayer[i].LastSynchronTimeStamp := fPLayer[i].LastSynchronTimeStamp + t;
      End;
      // fFrameLog wird nicht gesetzt, läuft paralell weiter auch während einer Pause.
    End;
  End;
  foldpausevalue := value;
End;

Procedure TServer.SaveGame(Const Stream: Tstream);
Var
  i, j, k: Integer;
  ver: uint32;
  Flag: Byte;
Begin
  (*
   * Alle Savegames werden erst mal als Bakup markiert, da es viel häufiger vorkommt
   * das die Streams als Bakup gespeichert werden
   * Lediglich beim "echten" Speichern wird aus Bakup dann Normal gemacht..
   *
   * das Flag kommt als aller erstes Byte weil es sich so leichter überschreiben lässt und
   * das lesen in HandleRequestSavegames am einfachsten ist.
   *)
  Flag := 1; // [0 = Normal, 1 = Bakup]
  stream.Write(flag, sizeof(flag));
  // Speichern der Versionsnummer beim Speichern
  ver := Version;
  stream.Write(ver, sizeof(ver));
  i := fMap.Difficulty;
  stream.Write(i, sizeof(i));
  i := Length(fPLayer);
  stream.Write(i, sizeof(i));
  i := fAktualRound;
  stream.Write(i, sizeof(i));
  stream.WriteAnsiString(MapName);
  stream.Write(fOverAllGameingTime, sizeof(fOverAllGameingTime));
  stream.Write(fOverAllRealGameingTime, sizeof(fOverAllRealGameingTime));
  stream.Write(fOverAllPausingTime, sizeof(fOverAllPausingTime));
  For i := 0 To high(fPLayer) Do Begin
    Stream.WriteAnsiString(fPLayer[i].UserName);
    j := fPLayer[i].Cash;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].Lives;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].LivesLost;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].Kills;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].BonusFinisher;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllEarned;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllMoneySpend;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllMoneyForFree;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllGiveAwayMoney;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllBuildingsBuild;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllBuildingsSold;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].OverAllBuildingsLeveledUp;
    stream.Write(j, sizeof(j));
    stream.Write(fPLayer[i].DamageDealt, SizeOf(fPLayer[i].DamageDealt));
    j := length(fPLayer[i].BuildingsBuild);
    stream.Write(j, sizeof(j));
    For k := 0 To high(fPLayer[i].BuildingsBuild) Do Begin
      stream.WriteAnsiString(fPLayer[i].BuildingsBuild[k].BuildingName);
      j := fPLayer[i].BuildingsBuild[k].Count;
      stream.Write(j, sizeof(j));
    End;
    // Speichern der TAChart Historien..
    j := fPLayer[i].gActions.Counter;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].gActions.Ptr;
    stream.Write(j, sizeof(j));
    For j := 0 To fPLayer[i].gActions.Ptr - 1 Do Begin
      k := fPLayer[i].gActions.Data[j];
      stream.Write(k, sizeof(k));
    End;
    j := fPLayer[i].gKills.Counter;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].gKills.Ptr;
    stream.Write(j, sizeof(j));
    For j := 0 To fPLayer[i].gKills.Ptr - 1 Do Begin
      k := fPLayer[i].gKills.Data[j];
      stream.Write(k, sizeof(k));
    End;
    j := fPLayer[i].gMoneyEarnded.Counter;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].gMoneyEarnded.Ptr;
    stream.Write(j, sizeof(j));
    For j := 0 To fPLayer[i].gMoneyEarnded.Ptr - 1 Do Begin
      k := fPLayer[i].gMoneyEarnded.Data[j];
      stream.Write(k, sizeof(k));
    End;
    j := fPLayer[i].gMoneySpend.Counter;
    stream.Write(j, sizeof(j));
    j := fPLayer[i].gMoneySpend.Ptr;
    stream.Write(j, sizeof(j));
    For j := 0 To fPLayer[i].gMoneySpend.Ptr - 1 Do Begin
      k := fPLayer[i].gMoneySpend.Data[j];
      stream.Write(k, sizeof(k));
    End;
  End;
  fmap.saveGameingData(Stream);
End;

Function TServer.LoadGame(Const Stream: TStream; uid: integer): boolean;
Var
  difficulty: integer;
  PLayerCount: integer;
  aRound, i, j, k: integer;
  mn: String;
  DummyPlayers, dp: Array Of TPlayer;
  m: TMemoryStream;
  PermutList: Array Of Integer;
  ver: uint32;
  flag: Byte;
Begin
  log('TServer.LoadGame', llTrace);
  result := false;
  flag := 0;
  stream.read(flag, sizeof(flag)); // Ob Bakup oder Savegame ist uns egal
  ver := $FFFFFFFF;
  stream.Read(ver, SizeOf(ver)); // Auslesen der Versionsnummer des Spieles, als der Spielstand gespeichert wurde
  If ver <> Version Then Begin
    SendSplashMessage(format('Unable to load game, invalid file version %d need %d', [ver, Version]), DefaultSplashHintDelay, v3(1, 0, 0), uid);
    LogLeave;
    exit;
  End;
  difficulty := -1;
  stream.Read(difficulty, SizeOf(difficulty));
  PLayerCount := -1;
  stream.Read(PLayerCount, SizeOf(PLayerCount));
  If length(fplayer) <> PLayerCount Then Begin
    SendSplashMessage(format('Unable to load game, invalid playercount, need %d', [PLayerCount]), DefaultSplashHintDelay, v3(1, 0, 0), uid);
    LogLeave;
    exit;
  End;
  aRound := -1;
  stream.Read(aRound, SizeOf(aRound));
  mn := Stream.ReadAnsiString;
  stream.Read(fOverAllGameingTime, sizeof(fOverAllGameingTime));
  stream.Read(fOverAllRealGameingTime, sizeof(fOverAllRealGameingTime));
  stream.Read(fOverAllPausingTime, sizeof(fOverAllPausingTime));
  DummyPlayers := Nil;
  setlength(DummyPlayers, length(fPLayer));
  For i := 0 To high(DummyPlayers) Do Begin
    DummyPlayers[i].UserName := stream.ReadAnsiString;
    DummyPlayers[i].Cash := 0;
    stream.Read(DummyPlayers[i].Cash, sizeof(DummyPlayers[i].Cash));
    DummyPlayers[i].Lives := 0;
    stream.Read(DummyPlayers[i].Lives, sizeof(DummyPlayers[i].Lives));
    DummyPlayers[i].LivesLost := 0;
    stream.Read(DummyPlayers[i].LivesLost, sizeof(DummyPlayers[i].LivesLost));
    DummyPlayers[i].Kills := 0;
    stream.Read(DummyPlayers[i].Kills, sizeof(DummyPlayers[i].Kills));
    DummyPlayers[i].BonusFinisher := 0;
    stream.Read(DummyPlayers[i].BonusFinisher, sizeof(DummyPlayers[i].BonusFinisher));
    DummyPlayers[i].OverAllEarned := 0;
    stream.Read(DummyPlayers[i].OverAllEarned, sizeof(DummyPlayers[i].OverAllEarned));
    DummyPlayers[i].OverAllMoneySpend := 0;
    stream.Read(DummyPlayers[i].OverAllMoneySpend, sizeof(DummyPlayers[i].OverAllMoneySpend));
    DummyPlayers[i].OverAllMoneyForFree := 0;
    stream.Read(DummyPlayers[i].OverAllMoneyForFree, sizeof(DummyPlayers[i].OverAllMoneyForFree));
    DummyPlayers[i].OverAllGiveAwayMoney := 0;
    stream.Read(DummyPlayers[i].OverAllGiveAwayMoney, sizeof(DummyPlayers[i].OverAllGiveAwayMoney));
    DummyPlayers[i].OverAllBuildingsBuild := 0;
    stream.Read(DummyPlayers[i].OverAllBuildingsBuild, sizeof(DummyPlayers[i].OverAllBuildingsBuild));
    DummyPlayers[i].OverAllBuildingsSold := 0;
    stream.Read(DummyPlayers[i].OverAllBuildingsSold, sizeof(DummyPlayers[i].OverAllBuildingsSold));
    DummyPlayers[i].OverAllBuildingsLeveledUp := 0;
    stream.Read(DummyPlayers[i].OverAllBuildingsLeveledUp, sizeof(DummyPlayers[i].OverAllBuildingsLeveledUp));
    fPLayer[i].DamageDealt[0] := 0;
    fPLayer[i].DamageDealt[1] := 0;
    fPLayer[i].DamageDealt[2] := 0;
    fPLayer[i].DamageDealt[3] := 0;
    stream.Read(fPLayer[i].DamageDealt, SizeOf(fPLayer[i].DamageDealt));
    k := 0;
    stream.Read(k, sizeof(k));
    setlength(DummyPlayers[i].BuildingsBuild, k);
    For k := 0 To high(DummyPlayers[i].BuildingsBuild) Do Begin
      DummyPlayers[i].BuildingsBuild[k].BuildingName := Stream.ReadAnsiString;
      DummyPlayers[i].BuildingsBuild[k].Count := 0;
      Stream.Read(DummyPlayers[i].BuildingsBuild[k].Count, sizeof(DummyPlayers[i].BuildingsBuild[k].Count));
    End;

    // Laden der TAChart Historien..
    stream.read(DummyPlayers[i].gActions.Counter, sizeof(DummyPlayers[i].gActions.Counter));
    stream.Read(DummyPlayers[i].gActions.Ptr, sizeof(DummyPlayers[i].gActions.Ptr));
    k := 1024;
    While k < DummyPlayers[i].gActions.Ptr Do
      k := k + 1024;
    setlength(DummyPlayers[i].gActions.Data, k);
    For j := 0 To DummyPlayers[i].gActions.Ptr - 1 Do Begin
      stream.Read(k, sizeof(k));
      DummyPlayers[i].gActions.Data[j] := k;
    End;
    stream.read(DummyPlayers[i].gKills.Counter, sizeof(DummyPlayers[i].gKills.Counter));
    stream.Read(DummyPlayers[i].gKills.Ptr, sizeof(DummyPlayers[i].gKills.Ptr));
    k := 1024;
    While k < DummyPlayers[i].gKills.Ptr Do
      k := k + 1024;
    setlength(DummyPlayers[i].gKills.Data, k);
    For j := 0 To DummyPlayers[i].gKills.Ptr - 1 Do Begin
      stream.Read(k, sizeof(k));
      DummyPlayers[i].gKills.Data[j] := k;
    End;
    stream.read(DummyPlayers[i].gMoneyEarnded.Counter, sizeof(DummyPlayers[i].gMoneyEarnded.Counter));
    stream.Read(DummyPlayers[i].gMoneyEarnded.Ptr, sizeof(DummyPlayers[i].gMoneyEarnded.Ptr));
    k := 1024;
    While k < DummyPlayers[i].gMoneyEarnded.Ptr Do
      k := k + 1024;
    setlength(DummyPlayers[i].gMoneyEarnded.Data, k);
    For j := 0 To DummyPlayers[i].gMoneyEarnded.Ptr - 1 Do Begin
      stream.Read(k, sizeof(k));
      DummyPlayers[i].gMoneyEarnded.Data[j] := k;
    End;
    stream.read(DummyPlayers[i].gMoneySpend.Counter, sizeof(DummyPlayers[i].gMoneySpend.Counter));
    stream.Read(DummyPlayers[i].gMoneySpend.Ptr, sizeof(DummyPlayers[i].gMoneySpend.Ptr));
    k := 1024;
    While k < DummyPlayers[i].gMoneySpend.Ptr Do
      k := k + 1024;
    setlength(DummyPlayers[i].gMoneySpend.Data, k);
    For j := 0 To DummyPlayers[i].gMoneySpend.Ptr - 1 Do Begin
      stream.Read(k, sizeof(k));
      DummyPlayers[i].gMoneySpend.Data[j] := k;
    End;
  End;
  // Bestimmen, wie die Spieler Permutiert werden müssen, damit es mit dem Savegame 1:1 ist, sonst stimmen nachher die Owner nicht
  PermutList := Nil;
  setlength(PermutList, length(fPLayer));
  For i := 0 To high(DummyPlayers) Do Begin // Alle Auf Ungültig setzen
    PermutList[i] := -1;
  End;
  For i := 0 To high(fPlayer) Do Begin // Alle Suchen und Permutation bestimmen
    For j := 0 To high(fPLayer) Do Begin
      If DummyPlayers[i].UserName = fPLayer[j].UserName Then Begin
        PermutList[j] := i;
        break;
      End;
    End;
  End;
  // Kopie der Originalspieler erstellen
  For i := 0 To high(DummyPlayers) Do Begin // Konnten alle gefunden werden ?
    If (PermutList[i] = -1) Then Begin
      If (high(PermutList) = 0) Then Begin
        // Spielt nur ein einziger Spieler, dann wird der Spielername ignoriert
        PermutList[0] := 0;
      End
      Else Begin
        SendSplashMessage(format('Unable to load game, could not match player %s', [DummyPlayers[i].UserName]), DefaultSplashHintDelay, v3(1, 0, 0), uid);
        LogLeave;
        exit;
      End;
    End;
  End;
  dp := Nil;
  setlength(dp, length(fPLayer));
  For i := 0 To high(fPLayer) Do Begin
    dp[i] := fPLayer[i];
  End;
  // Originalspieler mit Permutation zurückschreiben
  For i := 0 To high(fPLayer) Do Begin
    fplayer[PermutList[i]] := dp[i];
  End;
  // Nachdem die Spielerreihenfolge nun stimmt, werden nun die "Verbrauchsdaten" Aktualisiert
  For i := 0 To high(DummyPlayers) Do Begin
    If fPLayer[i].UserName <> DummyPlayers[i].UserName Then Begin // Dieses If darf eigentlich nie kommen
      mn := '';
      For j := 0 To high(DummyPlayers) Do Begin
        mn := mn + '"' + fPLayer[j].UserName + '" == "' + DummyPlayers[j].UserName + '"' + LineEnding;
      End;
      Raise exception.Create('Verdammt die Permutationsauflösung hat nicht geklappt: ' + LineEnding + mn);
    End;
    fPLayer[i].Cash := DummyPlayers[i].Cash;
    fPLayer[i].Lives := DummyPlayers[i].Lives;
    fPLayer[i].LivesLost := DummyPlayers[i].LivesLost;
    fPLayer[i].Kills := DummyPlayers[i].Kills;
    fPLayer[i].BonusFinisher := DummyPlayers[i].BonusFinisher;
    fPLayer[i].OverAllEarned := DummyPlayers[i].OverAllEarned;
    fPLayer[i].OverAllMoneySpend := DummyPlayers[i].OverAllMoneySpend;
    fPLayer[i].OverAllMoneyForFree := DummyPlayers[i].OverAllMoneyForFree;
    fPLayer[i].OverAllGiveAwayMoney := DummyPlayers[i].OverAllGiveAwayMoney;
    fPLayer[i].OverAllBuildingsBuild := DummyPlayers[i].OverAllBuildingsBuild;
    fPLayer[i].OverAllBuildingsSold := DummyPlayers[i].OverAllBuildingsSold;
    fPLayer[i].OverAllBuildingsLeveledUp := DummyPlayers[i].OverAllBuildingsLeveledUp;
    fPLayer[i].BuildingsBuild := DummyPlayers[i].BuildingsBuild;
    fPLayer[i].gActions := DummyPlayers[i].gActions;
    fPLayer[i].gKills := DummyPlayers[i].gKills;
    fPLayer[i].gMoneyEarnded := DummyPlayers[i].gMoneyEarnded;
    fPLayer[i].gMoneySpend := DummyPlayers[i].gMoneySpend;
  End;
  // Alle Laden die Karte
  MapName := '';
  HandleRequestLoadMap(mn, 0, 0);
  m := TMemoryStream.Create;
  m.CopyFrom(Stream, Stream.Size - Stream.Position);
  m.Position := 0;
  fMap.LoadGameingData(m);
  fMap.Difficulty := difficulty;
  fAktualRound := aRound;
  SendChunk(miLoadGamingdata, m, 0);
  RefreshAllPlayerStats;
  result := true;
End;

Procedure TServer.BakupWave;
Var
  f: TFileStream;
  fTruncedMapfolder: String;
Begin
  If fLastSaveable.Size <> 0 Then Begin
    log('TServer.BakupWave', llTrace);
    fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
    While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
      delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
    End;
    fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
    If Not DirectoryExistsUTF8(fTruncedMapfolder) Then Begin
      If Not CreateDirUTF8(fTruncedMapfolder) Then Begin
        log('Could not create bakup directory : ' + fTruncedMapfolder, llError);
        LogLeave;
        exit;
      End;
    End;
    f := TFileStream.Create(fTruncedMapfolder + MapName + '_wave_' + IntToStr(fAktualRound + 1) + '_player_' + inttostr(length(fPLayer)) + '.sg', fmCreate Or fmOpenWrite);
    fLastSaveable.Position := 0;
    f.CopyFrom(fLastSaveable, fLastSaveable.Size);
    f.free;
    LogLeave;
  End;
End;

Procedure TServer.AppendGameStatistics(Const Stream: TStream);
Var
  i, j, ji, k: integer;
  i64: Int64;
Begin
  stream.Write(fOverAllGameingTime, sizeof(fOverAllGameingTime));
  stream.Write(fOverAllRealGameingTime, sizeof(fOverAllRealGameingTime));
  stream.Write(fOverAllPausingTime, sizeof(fOverAllPausingTime));
  i := fAktualRound + 1;
  stream.Write(i, SizeOf(i));
  i := fMap.Difficulty;
  stream.Write(i, SizeOf(i));
  For i := 0 To high(fPLayer) Do Begin
    stream.WriteAnsiString(fPLayer[i].UserName);

    stream.Write(fPLayer[i].Lives, sizeof(fPLayer[i].Lives));
    stream.Write(fPLayer[i].LivesLost, sizeof(fPLayer[i].LivesLost));
    stream.Write(fPLayer[i].Kills, sizeof(fPLayer[i].Kills));

    stream.Write(fPLayer[i].OverAllEarned, sizeof(fPLayer[i].OverAllEarned));
    stream.Write(fPLayer[i].OverAllMoneySpend, sizeof(fPLayer[i].OverAllMoneySpend));
    stream.Write(fPLayer[i].OverAllMoneyForFree, sizeof(fPLayer[i].OverAllMoneyForFree));
    stream.Write(fPLayer[i].OverAllGiveAwayMoney, sizeof(fPLayer[i].OverAllGiveAwayMoney));

    stream.Write(fPLayer[i].OverAllBuildingsBuild, sizeof(fPLayer[i].OverAllBuildingsBuild));
    stream.Write(fPLayer[i].OverAllBuildingsSold, sizeof(fPLayer[i].OverAllBuildingsSold));
    stream.Write(fPLayer[i].OverAllBuildingsLeveledUp, sizeof(fPLayer[i].OverAllBuildingsLeveledUp));
    // Suchen des Meist gebauten Gebäudes
    ji := 0;
    For j := 1 To high(fPLayer[i].BuildingsBuild) Do Begin
      If fPLayer[i].BuildingsBuild[j].Count > fPLayer[i].BuildingsBuild[ji].Count Then Begin
        ji := j;
      End;
    End;
    If assigned(fPLayer[i].BuildingsBuild) Then Begin
      stream.WriteAnsiString(fPLayer[i].BuildingsBuild[ji].BuildingName + ' (' + inttostr(fPLayer[i].BuildingsBuild[ji].Count) + ')');
    End
    Else Begin
      // Der Spieler hat gar kein Gebäude gebaut..
      stream.WriteAnsiString('n/a');
    End;

    stream.Write(fPLayer[i].DamageDealt[0], sizeof(fPLayer[i].DamageDealt[0]));
    stream.Write(fPLayer[i].DamageDealt[1], sizeof(fPLayer[i].DamageDealt[1]));
    stream.Write(fPLayer[i].DamageDealt[2], sizeof(fPLayer[i].DamageDealt[2]));
    stream.Write(fPLayer[i].DamageDealt[3], sizeof(fPLayer[i].DamageDealt[3]));

    i64 := CalcHighScoreForPlayer(i);
    stream.Write(i64, SizeOf(i64));
    (*
     * Es folgen die TaChart Kurven
     *)
    // Actions
    j := fPLayer[i].gActions.Ptr;
    stream.Write(j, SizeOf(j));
    For j := 0 To fPLayer[i].gActions.Ptr - 1 Do Begin
      k := fPLayer[i].gActions.Data[j];
      stream.Write(k, sizeof(k));
    End;
    // Kills
    j := fPLayer[i].gKills.Ptr;
    stream.Write(j, SizeOf(j));
    For j := 0 To fPLayer[i].gKills.Ptr - 1 Do Begin
      k := fPLayer[i].gKills.Data[j];
      stream.Write(k, sizeof(k));
    End;
    // Geld Eingenommen
    j := fPLayer[i].gMoneyEarnded.Ptr;
    stream.Write(j, SizeOf(j));
    For j := 0 To fPLayer[i].gMoneyEarnded.Ptr - 1 Do Begin
      k := fPLayer[i].gMoneyEarnded.Data[j];
      stream.Write(k, sizeof(k));
    End;
    // Geld Ausgegeben
    j := fPLayer[i].gMoneySpend.Ptr;
    stream.Write(j, SizeOf(j));
    For j := 0 To fPLayer[i].gMoneySpend.Ptr - 1 Do Begin
      k := fPLayer[i].gMoneySpend.Data[j];
      stream.Write(k, sizeof(k));
    End;
  End;
End;

Function TServer.CalcHighScoreForPlayer(PlayerIndex: integer): Int64;
Var
  WaveBonus, MoneyEarned, MoneySpent, Lives, Kills, i: integer;
Begin
  result := 0;
  Lives := fPLayer[PlayerIndex].Lives; //Im Koop haben alle Spieler immer die Gleiche Anzahl leben
  // Differenzierung nach (Koop / Single) / Anzahl Spieler
  If (fMap.MapType = mtCoop) Or (fMap.MapType = mtCoopMaze) Then Begin
    kills := 0;
    MoneyEarned := 0;
    MoneySpent := 0;
    For i := 0 To high(fPLayer) Do Begin
      Kills := Kills + fPLayer[i].Kills;
      MoneyEarned := MoneyEarned + fPLayer[i].OverAllEarned;
      MoneySpent := MoneySpent + fPLayer[i].OverAllMoneySpend;
    End;
  End
  Else Begin
    Kills := fPLayer[PlayerIndex].Kills;
    MoneyEarned := fPLayer[PlayerIndex].OverAllEarned;
    MoneySpent := fPLayer[PlayerIndex].OverAllMoneySpend;
  End;
  //1000 * Wavenum Am Ende + 100 * Anzahl Leben am Ende Übrig + 1 * Anzahl Kills - Simulated Time in sec + Money Earned - Money Spent
  WaveBonus := 0;
  Case fMap.Difficulty Of
    0: WaveBonus := 500; // Easy
    1: WaveBonus := 1000; // Normal
    2: WaveBonus := 1250; // Hard
  End;
  result :=
    WaveBonus * (fAktualRound + 1)
    + 100 * Lives
    + 1 * Kills
    - (fOverAllGameingTime Div 1000) // Umrechnen ms in S
  + MoneyEarned
    - MoneySpent
    ;
End;

Procedure TServer.HandleTogglePause;
Var
  m: TMemoryStream;

Begin
  log('TServer.HandleTogglePause', llTrace);
  If fGameState = gs_Gaming Then Begin
    fpausing := Not fpausing;
    ApplyPause(fPausing);
    m := TMemoryStream.Create;
    m.Write(fpausing, sizeof(fpausing));
    SendChunk(miTogglePause, m, 0);
  End;
  logleave;
End;

Function TServer.ExtractMapAttibutes(MapName: String): String;
Var
  ini: TIniFile;
  rc: integer;
Begin
  result := '';
  ini := TIniFile.Create(MapFolder + MapName + PathDelim + MapRootName);
  result := format('%d:%d', [
    ini.ReadInteger('map', 'type', -1),
      ini.ReadInteger('map', 'maxplayer', 0)
      ]);
  rc := ini.ReadInteger('Rating', 'Count', 0);
  If rc = 0 Then Begin
    result := result + ':0';
  End
  Else Begin
    result := result + ':' + inttostr(trunc(ini.ReadInteger('Rating', 'Sum', 0) / rc));
  End;
  ini.free;
End;

Procedure TServer.SendSplashMessage(Text: String; Delay: Integer;
  Color: tvector3; Uid: integer);
Var
  m: TMemoryStream;
  i: Integer;
  f: Single;
Begin
  log(format('TServer.SendSplashMessage : %d, %s', [uid, Text]), llTrace);
  m := TMemoryStream.Create;
  m.WriteAnsiString(Text);
  i := Delay;
  m.Write(i, sizeof(i));
  f := Color.x;
  m.Write(f, sizeof(f));
  f := Color.y;
  m.Write(f, sizeof(f));
  f := Color.z;
  m.Write(f, sizeof(f));
  SendChunk(miSplashHint, m, uid);
  LogLeave;
End;

Procedure TServer.CreateNewFrame;
Begin
  // Neue Einheiten Emirieren
  fSpawnModul.Update(); // Erzeugt ggf die Gegner, true, wenn keine Gegner mehr erzeigt werden sollen
  // Alle Einheiten Bewegen
  fmap.MoveAllOpponents(@UpdateEvent);
  // Alle Gebäude "Bauen"
  fmap.HandleAllBuildings();
  fmap.HandleAllHeros();
  // Alle Geschosse Bewegen
  fmap.HandleAllBullets(@UpdateEvent);
  // Ist das Spiel Vorbei ?
  EndGameCheck();
End;

Procedure TServer.UpdateAllClients;
Var
  m: TMemoryStream;
Begin
  // Sammel Aller Daten von Bewegten Objekten und Verteilen an alle Clients
  m := TMemoryStream.Create;
  fmap.GetMovingObjectsState(m);
  If m.Size <> 0 Then Begin
    fFrameLog.AccumulatedSize := fFrameLog.AccumulatedSize + m.Size;
    fFrameLog.Count := fFrameLog.Count + 1;
    SendChunk(miUpdateMoveables, m, 0);
    fLastUpdateDataSend := true;
  End
  Else Begin
    // Es Braucht nichts gesendet zu werden, da keine Beweglichen Objekte Sichtbar sind.
    If fLastUpdateDataSend Then Begin
      SendChunk(miUpdateMoveables, Nil, 0);
    End;
    fLastUpdateDataSend := false;
    m.free;
  End;
End;

Procedure TServer.UpdateEvent(Player: integer; Reason, ID2: integer);
Var
  i: Integer;
Begin
  log(format('TServer.UpdateEvent : %d %d %d', [Player, Reason, ID2]), llTrace);
  Case Reason Of
    UpdateIDBonusOpFinished: Begin
        // Mit Zählen der Bonusgegner die es aus der Karte Raus geschafft haben
        fPLayer[Player].BonusFinisher := fPLayer[Player].BonusFinisher + 1;
        RefreshAllPlayerStats();
      End;
    UpdateIdLifeLost: Begin
        If (fMap.MapType = mtSingle) Or (fMap.MapType = mtSingleMaze) Then Begin
          // Nur der Spieler, welcher sein Leben Verloren hat, bekommt den Abzug.
          fPLayer[Player].Lives := fPLayer[Player].Lives - 1;
          fPLayer[Player].LivesLost := fPLayer[Player].LivesLost + 1;
        End
        Else Begin
          // Alle Spieler haben ein Leben Verloren
          For i := 0 To high(fPLayer) Do Begin
            fPLayer[i].Lives := fPLayer[i].Lives - 1;
            fPLayer[i].LivesLost := fPLayer[i].LivesLost + 1;
          End;
        End;
        RefreshAllPlayerStats();
      End;
    UpdateIdOpponentKilled: Begin
        fPLayer[Player].Cash := fPLayer[Player].Cash + ID2;
        fPLayer[Player].gMoneyEarnded.Counter := fPLayer[Player].gMoneyEarnded.Counter + ID2;
        fPLayer[Player].OverAllEarned := fPLayer[Player].OverAllEarned + ID2;
        fPLayer[Player].Kills := fPLayer[Player].Kills + 1;
        fPLayer[Player].gKills.Counter := fPLayer[Player].gKills.Counter + 1;
        RefreshAllPlayerStats();
      End;
    UpdateIdBankIncrease: Begin
        fPLayer[Player].Cash := fPLayer[Player].Cash + ID2;
        fPLayer[Player].gMoneyEarnded.Counter := fPLayer[Player].gMoneyEarnded.Counter + ID2;
        fPLayer[Player].OverAllEarned := fPLayer[Player].OverAllEarned + ID2;
        RefreshAllPlayerStats();
      End;
    UpdateDamage_1,
      UpdateDamage_2,
      UpdateDamage_3,
      UpdateDamage_4: Begin
        fPLayer[Player].DamageDealt[Reason - UpdateDamage_1] := fPLayer[Player].DamageDealt[Reason - UpdateDamage_1] + ID2;
      End;
  End;
  LogLeave;
End;

Procedure TServer.EndGameCheck;
  Procedure ForceEndWave(Succeed: Boolean);
  Var
    m: TMemoryStream;
  Begin
    // Ist noch irgend ein Gebäude am Bauen, dann Brechen wir alle erst mal Ab
    m := fmap.ForceBuildingsBuildReady;
    If m.Size > 0 Then Begin
      SendChunk(miSetBuildingsToStage, m, 0);
    End
    Else Begin
      m.free;
    End;
    // Ist noch ein Held am Bauen, dann brechen wir alle erst mal ab
    m := fmap.ForceHerosReady;
    If m.Size > 0 Then Begin
      SendChunk(miSetHerosToLevel, m, 0);
    End
    Else Begin
      m.free;
    End;
    HandleOnEndRound(Succeed);
  End;

Var
  i: Integer;
  e: Boolean;
Begin
  // Wird eine Wave genau mit dem Ableben des letzten Gegners beendet, muss das zuerst erkannt werden.
  e := false;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].Lives <= 0 Then Begin
      e := true;
      break;
    End;
  End;
  If e Then Begin // Spiel Vorbei wir haben verloren, oder zumindest ein Spieler
    ForceEndWave(false);
    exit;
  End;
  // Eine Runde wird erfolgreich beendet
  If fSpawnModul.Finished And (fmap.OpponentCount = 0) Then Begin // Müssen noch Einheiten gesandt werden und auf der Karte sind auch keine Gegner mehr
    ForceEndWave(true);
    exit;
  End;
End;

Constructor TServer.create(Port, AutoTimeOut: Integer; Password: String);
Begin
  log('TServer.create', lltrace);
  Inherited create;
  fLastSaveable := TMemoryStream.Create;
  fRestartWave := TMemoryStream.Create;
  fGameState := gs_EditMode;
  fSpawnModul := TSpawnModul.create;
  MapName := '';
  fmap := Nil;
  fAutotimeout := AutoTimeOut;
  fPassword := Password;
  fTCP := TLTcp.Create(Nil);
  fTCP.ReuseAddress := true; // Bei Absturz kann so sofort wieder neu verbunden werden
  fTCP.OnAccept := @OnAccept;
  fTCP.OnError := @OnError;
  fTCP.OnDisconnect := @OnDisconnect;
  fUDP := TLUdp.Create(Nil);
  fUDP.OnReceive := @OnUDPReceiveEvent;
  If Not fUDP.Listen(UDPPingPort) Then Begin
    log('Error could not listen on port: ' + inttostr(UDPPingPort), llFatal);
    LogLeave;
    halt;
  End;

  fChunkManager := TChunkManager.create;
  fChunkManager.RegisterConnection(fTCP);
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;
  factive := true;
  fLastActiveTickTimestamp := gettick();
{$IFDEF WinXPMode}
  LogShow('Attention using WinXP mode make shure client is also compiled in WinXP mode.', llInfo);
{$ENDIF}
  fPLayer := Nil;
  fTCPPort := Port;
  If Not fChunkManager.Listen(Port) Then Begin
    log('Error could not listen on port: ' + inttostr(port), llFatal);
    LogLeave;
    halt;
  End;
  LogLeave;
End;

Destructor TServer.destroy;
Var
  i: Integer;
Begin
  log('TServer.destroy', lltrace);
  fLastSaveable.free;
  fRestartWave.free;
  fChunkManager.Disconnect(true);
  fChunkManager.free;
  fSpawnModul.free;
  ftcp.free;
  // Den UDP auch sauber platt machen
  If fUDP.Connected Then Begin
    fUDP.Disconnect(true);
    While fUDP.Connected Do Begin
      fUDP.CallAction;
    End;
  End;
  fUDP.free;
  fmap.free;
  fmap := Nil;
  For i := 0 To high(fPLayer) Do Begin
    setlength(fPLayer[i].gActions.Data, 0);
    setlength(fPLayer[i].gKills.Data, 0);
    setlength(fPLayer[i].gMoneyEarnded.Data, 0);
    setlength(fPLayer[i].gMoneySpend.Data, 0);
    setlength(fPLayer[i].BuildingsBuild, 0);
  End;
  setlength(fPLayer, 0);
  LogLeave;
End;

Procedure TServer.CheckSynchrons;
Var
  n: int64;
  i: integer;
  b: Boolean;
  s: String;
  m: TMemoryStream;
Begin
  If fpausing Then exit;
  n := gettick();
  // versenden der HeartBeat Aufforderung an die Clients
  If n - fLastHeartbeatTimestamp >= HeartBeatTime Then Begin
    fLastHeartbeatTimestamp := n;
    m := TMemoryStream.Create;
    m.Write(n, SizeOf(n));
    SendChunk(miHeartBeat, m, 0);
  End;
  // Prüfen der Empfangenen HeartBeat Aufforderungen
  s := '';
  b := false;
  For i := 0 To high(fplayer) Do Begin
    If n - fplayer[i].LastSynchronTimeStamp > SynchonizeTimeOut Then Begin
      s := fplayer[i].UserName;
      b := true;
      break;
    End;
  End;
  // Mindestens 1 Client ist asynchron, wir leiten eine Zwangspause ein / aus
  If b Then Begin
    If Not fSyncPause Then Begin // Positive Flanke der SyncPausierung
      log(format('Activate synchronising pause. %s is out of sync', [s]), llInfo);
      fSyncPause := true;
      ApplyPause(fpausing);
      LogLeave;
    End;
  End
  Else Begin
    If fSyncPause Then Begin // Negative Flanke der SyncPausierung
      log('Deactivate synchronising pause.', llInfo);
      fSyncPause := false;
      ApplyPause(fpausing);
      LogLeave;
    End;
  End;
End;

Procedure TServer.Execute;
Var
  pt, n: int64;
Begin
  log('TServer.Execute', lltrace);
  pt := GetTick; // Warnung nieder machen
  // Loop in einer Endlosschleife, so lange bis 1000ms lang kein Client mehr connected ist, dann raus
  While factive Do Begin
    fChunkManager.CallAction(); // Alle Aktuellen Aufgaben des TCP-Stacks Abbarbeiten
    fUDP.CallAction();
    If fGameState = gs_Gaming Then Begin // Im Spielmodus Frames und Updates der Clients Berechnen
      // alle 10 s Loggen wie Groß die Spieldaten waren, welche gesandt
      n := gettick();
      If fFrameLog.TimeStamp + 10000 <= n Then Begin
        fFrameLog.TimeStamp := n;
        If fFrameLog.Count <> 0 Then Begin
          log(Format('Send %d frames with avg size of %.1f Bytes.', [fFrameLog.Count, fFrameLog.AccumulatedSize / fFrameLog.Count]), lldebug);
        End
        Else Begin
          log('Send 0 frames.', lldebug);
        End;
        fFrameLog.Count := 0;
        fFrameLog.AccumulatedSize := 0;
      End;
      CheckSynchrons; //Sind alle Player noch Synchron, wenn nein Warten
      If fpausing Then Begin // Zählen der Pausenzeit
        n := GetTick;
        If n <> pt Then Begin
          fOverAllPausingTime := fOverAllPausingTime + n - pt;
          pt := n;
        End;
      End
      Else Begin
        pt := GetTick;
      End;
      If (Not (fpausing Or fSyncPause)) Then Begin
        n := gettick();
        If FLastFrameTimestamp + (FrameRate Div Speedup) <= n Then Begin
          fOverAllGameingTime := fOverAllGameingTime + FrameRate;
          // Durch das Aufaddieren der Zeit und nicht direkt setzen auf n könnte hier eine
          // Überpropertionale rechenlast erzeugt werden, besonders, wenn CreateNewFrame, länger dauert als (FrameRate Div Speedup)
          FLastFrameTimestamp := FLastFrameTimestamp + (FrameRate Div Speedup);
          CreateNewFrame;
        End;
        // Egal, welcher Speedup, das Spiel wird mit konstanter Rate Aktualisiert
        If fLastClientUpdateTimestamp + UpdateRate <= n Then Begin
          fLastClientUpdateTimestamp := n; // fLastClientUpdateTimestamp + UpdateRate; Verhindern von oben beschriebener Situation
          fOverAllRealGameingTime := fOverAllRealGameingTime + UpdateRate;
          UpdateAllClients;
          // Alle 10s werden alle Spielerstatistiken weg geschrieben ;)
          fCounterfor10s := fCounterfor10s + 1;
          If fCounterfor10s >= 10000 Div UpdateRate Then Begin
            fCounterfor10s := 0;
            Handle10sStatistikEvaluation();
          End;
        End;
      End;
    End;
    (*
     * Ab hier geht es nur noch darum zu erkennen ob die Anwendung beendet werden
     * soll.
     *)
    If KeyPressed Then Begin
      If ReadKey = #27 Then Begin
        Log('Close by user input.', llInfo);
        factive := false;
      End;
    End;
    // Beenden Bedingung erkennen
    If high(fPLayer) <> -1 Then Begin
      fLastActiveTickTimestamp := gettick();
    End
    Else Begin
      // 1000ms lang keine Activen Clients, dieser Server kann geschlossen werden
      If (fAutotimeout <> 0) Then Begin
        If (fLastActiveTickTimestamp + fAutotimeout < gettick()) Then Begin
          factive := false;
        End;
      End;
      {
      // Zwangsabbruch, weil der Server sich selbst geschlossen hat
      If Not fChunkManager.Connected Then Begin
        log('Chunkmanager lost connection.', llfatal);
        factive := false;
      End;
      }
    End;
    sleep(1);
  End;
  LogLeave;
End;

Procedure TServer.SaveWhatPossible;
Var
  f: TFileStream;
  fTruncedMapfolder: String;
Begin
  // Panik Save was auch immer gerade Aktiv war
  log('TServer.SaveWhatPossible', llTrace);
  If fLastSaveable.Size <> 0 Then Begin
    log('Server crashed, storing last game state as bakup savegame.', llInfo);
    fTruncedMapfolder := ExcludeTrailingPathDelimiter(MapFolder);
    While fTruncedMapfolder[length(fTruncedMapfolder)] <> PathDelim Do Begin
      delete(fTruncedMapfolder, length(fTruncedMapfolder), 1);
    End;
    fTruncedMapfolder := fTruncedMapfolder + 'savegames' + PathDelim;
    If Not DirectoryExistsUTF8(fTruncedMapfolder) Then Begin
      If Not CreateDirUTF8(fTruncedMapfolder) Then Begin
        log('Could not create savegames directory : ' + fTruncedMapfolder, llError);
        LogLeave;
        exit;
      End;
    End;
    f := TFileStream.Create(fTruncedMapfolder + 'crashed_game_' + FormatDateTime('YYYY_MM_DD HH_MM', now) + '.sg', fmCreate Or fmOpenWrite);
    fLastSaveable.Position := 0;
    f.CopyFrom(fLastSaveable, fLastSaveable.Size);
    f.free;
  End;
  LogLeave;
End;

End.


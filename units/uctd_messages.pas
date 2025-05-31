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
Unit uctd_messages;

{$MODE objfpc}{$H+}

Interface

Uses classes;

Const
  // MessageIdentifiers müssen Numerisch disjunkt sein !!!

  (*
   * Die Nachrichten ID's sind eine 32-Bit Integer zahlen
   *
   * ID's dürfen aber nur im Bereich [1 .. 65535] vergeben werden (also die unteren 2-Byte)
   *
   * die oberen 2 Byte werden für "interne" Zwecke des Clients verwendet und dürfen daher keine ID sein.
   *
   * Der Server Spiegelt die oberen 2-ID-Bytes in jede Anwort die er sendet, damit kann ein Client mehrere Gleiche Anfragen senden und diese
   * dennoch aus einander halten.
   *
   * Broadcast Nachrichten haben immer als Highword = 0
   *
   * => Achtung, das ist noch nicht optimal implementiert, eher so dass es funktioniert, im Zweifel also noch mal Prüfen !!
   *
   *)

  (*
   * Nachrichten, welche Server und Client Gleichermasen empfangen und senden
   *)
  miTogglePause = 1; // Ein Client will Pausieren, der Server Verteilt..
  miRequestFileTransfer = 2; // Soll A eine Datei an B senden ?
  miStartFileTransfer = 3; // Ja, A soll an B senden
  miFileTransfer = 4; // B Empfängt Datei
  miUpdateMapProperty = 5; // Client -> Server (Update der Map Eigenschaft), Server -> Client Broadcast an alle über das Update
  miChatMessage = 6; // Eine Chat Nachricht wird verteilt
  miHeartBeat = 7;
  miPing = 8;
  miCreateSplashMark = 9; // Der Client weist den Server an eine Markierung auf der Karte An zu Zeigen
  miRefreshPlayerStats = 10; // Client -> Server Gib mal info, Server -> Client Antwort
  miCloneMapWave = 11; // "Clone die Karten Wave x auf die Karten Wave y
  miDelOpponent = 12; // Der Eine will das der andere einen Opponent Löscht
  miDelBuilding = 13; // Der Eine will das der andere ein Gebäude Löscht
  miDelHero = 14; // Der Eine will das der andere einen Helden Löscht
  miCleanupUnusedOpponents = 15; // Ein Client triggert es, der Server verteilt es an alle und alle "löschen"

  (*
   * Nachrichten vom Client an den Server
   *)
  miStartRoundResult = 100; // Antwort, dass der Client soweit ist.
  miRequestLogin = 101; // Der CLient sendet sein Benutzername/ LoginPasswort und will mit machen
  miRequestMap = 102; // Der Client fordert die Komplette Karte sammt aller Zusatzdaten an (macht er eigentlich nur nach erfolgreichem Login)
  miRequestMapList = 103; // Der Client fordert die Mapliste an (Nur Namen)
  miRequestMapPreviewInfo = 104; // Der Client will im Map Laden Dialog die Karteninformationen anzeigen.
  miRequestLoadMap = 105; // Der Client will eine neue Karte Laden, das bewirkt, dass alle diese Karte laden.
  miNewMap = 106; // Der Client hat eine neue Karte erstellt, der Server soll diese Anlegen
  miInitiateNewGame = 107; // Ein Client will ein neues Spiel starten, der Server Checkt alles fragt ggf den Client Nr.1 nach dem Schrierigkeitsgrad und dann los.
  miBuyBuilding = 108; // Ein Client will ein Gebäude kaufen
  miSellBuilding = 109; // Ein Client verkauft ein Gebäude
  miRequestFileList = 110; // Ein Client will alle Dateien mit der Maske aus dem Mapverzeichnis
  miRequestPingTimes = 111; // Ein Client will alle Ping zeiten zu allen Clients wissen
  miInitiateRound = 112;
  miContinue = 113; // Ein Client will nach der Letzten Runde weiter Spielen
  miRestartLastWave = 114;
  miWantLevelUpBuilding = 115;
  miRequestSavegames = 116;
  miAbortWave = 117;
  miTransferCash = 118;
  miChangeBuildingStrategy = 119;
  miTransferCompleteMapTerrain = 120;
  midecSpeed = 121;
  miincSpeed = 122;
  midelSaveGame = 123; // Client, will, dass der Server 1 Savegame löscht
  midelSaveGames = 124; // Client, will das der Server alle Savegames löscht
  miRequestPlayerPosChange = 125; // Client, will das der Server die Spielerposition eines spielers "Tauscht"
  miRequestMapHighscoresAndRating = 126; // Der Client will alle Highscores der Karte haben
  miRequestClearMapHighscores = 127; // Der Client will, dass der Server alle Highscores der Karte löscht
  miSendMapRating = 128; // Der User hat eine Karte gespielt und bewertet diese
  miAddRandomWave = 129; // Der Server erzeugt eine neue "zufällige" wave und verteilt diese dann.
  miBuyHero = 130; // Ein Client will einen Helden kaufen
  miChangeHeroStrategy = 131;
  miSetHeroTargets = 132;
  miKickPlayerOut = 133; // Der Server soll den Spieler xy raus werfen (zum Retten wenn ein Spieler aus dem Netzwerk geflogen ist.
  miExchangeWaves = 134;

  (*
   * Nachrichten vom Server an den Client
   *)
  miRequestLoginResult = 200;
  miRequestMapListResult = 201;
  miRequestMapPreviewInfoResult = 202;
  miStartRound = 203; // Der Server startet ein Spiel mit der Aktuellen Karte, der Client Schaltet auf Warten um und bestätigt
  miLoadMap = 204; // Der Server hat alle Dateien die zur Karte gehören Versand, wenn der Client keine offenen Filetransfers mehr hat, dann Laden.
  miSplashHint = 205; // Der Server will beim Client eine Splashnachricht anzeigen
  miStart = 206; // Die Runde geht Los
  miUpdateMoveables = 207; // Der Server Aktualisiert alle bewegten Objecte
  miAddBuilding = 208; // Der Server hat das Bauen eines Gebäudes Akzeptiert und fordert die Clients auf es auf ihrer Karte (ungeprüft) zu adden.
  miRemoveBuilding = 209; // Der Server weist uns an ein Gebäude zu entfernen
  miRequestFileListResult = 210;
  miEndRound = 211; // Eine Runde ist Vorbei, wir gehen in den Editiermodus
  miForceEditMode = 212;
  miLevelUpBuilding = 213; // Der Server weist uns an ein Gebäude zu Upleveln
  miLoadGamingdata = 214; // Der CLient soll alle Spieldaten nachladen
  miLoadGame = 215;
  miSaveGame = 216;
  miRequestSavegamesResult = 217;
  miSetSpeedUp = 218; // Server Informiert Clients über die Aktuelle ZeitSpeedup
  miSetBuildingsToStage = 219; // Ersetzt miForceBuildReady
  miFilesToTransmitCount = 220; // Anzahl an Dateien für die der Server gleich einen Request senden wird -> für den lade Fortschrittsbalken
  miRequestMapHighscoresAndRatingResult = 221; // Der Server sendet dem Client alle Hichscores der Karte
  miAddHero = 222; // der Server hat das bauen eines Heroes Akzeptiert und fordert die Clients auf es auf ihrer Karte (ungeprüft) zu adden.
  miSetHeroesToLevel = 223;
  miYouWereKickedOut = 224; // Der Server hat uns Raus geworfen
  miCloseMap = 225; // Der Server teilt dem Client mit seine Karte zu schließen, das macht er nur weil er gleich eine neue sendet

  (*
   * MapProperties, werden mittels der miUpdateMapProperty Nachricht versand, müssen Disjunkt sein.
   * -1 = Fehler
   *)
  mpSaveMap = 0; // Eigentlich keine Geändete Eigenschaft, zwingt aber den Server dazu die Karte zu Speichern (Mehr als Bakup gedacht, passiert immer bei Checkmap)
  mpDescription = 1;
  mpMapType = 2;
  mpMaxPlayer = 3;
  mpLives = 4;
  mpResize = 5;
  mpCoord = 6;
  mpAddWayPoint = 7;
  mpDelWayPoint = 8;
  mpDecPointOrder = 9;
  mpDelPlacement = 10;
  mpAddPlacement = 11;
  mpAddBuyable = 12;
  // mpDelBuyable = 13; -- Das gibt es nicht mehr
  mpUpdateBuyable = 14;
  mpWaveCount = 15;
  mpCashOnWaveStart = 16;
  mpWaveHint = 17;
  mpWaveOpponents = 18; // anzahl der Opponents Pro Wave
  mpWaveOpponent = 19; // der Opponent der Wave
  mpWaveOpponentCount = 20;
  mpWaveOpponentCashPerUnit = 21;
  mpWaveOpponentUnitsPerSpawn = 22;
  mpWaveOpponentSpawnDelay = 23;
  mpWaveOpponentSpawnDelta = 24;
  mpBackTex = 25;
  mpDC1Tex = 26;
  mpDC2Tex = 27;
  mpDC3Tex = 28;
  mpDC4Tex = 29;
  mpDelOppInWave = 30; // Löschen eines Bestimmten Opponents in einer Wave
  mpWCoord = 31; // Setzten oder Löschen einer Wegpunkt Fläche
  mpCoordData = 32; // Alle Coord Daten als Stream auf einen Schlag

Function MessageIdentifierToString(value: integer): String;

Function MessageMapPropertyToString(Value: integer): String;

Implementation

Uses sysutils;

Function MessageIdentifierToString(value: integer): String;
Begin
  value := value And $FFFF; // Abschneiden der oberen beiden Byte
  result := inttostr(value);
  Case value Of
    miTogglePause: result := 'TogglePause';
    miRequestFileTransfer: result := 'RequestFileTransfer';
    miStartFileTransfer: result := 'StartFileTransfer';
    miFileTransfer: result := 'FileTransfer';
    miUpdateMapProperty: result := 'UpdateMapProperty';
    miChatMessage: result := 'ChatMessage';
    miHeartBeat: result := 'HeartBeat';
    miPing: result := 'Ping';
    miCreateSplashMark: result := 'CreateSplashMark';
    miRefreshPlayerStats: result := 'RefreshPlayerStats';
    miCloneMapWave: result := 'CloneMapWave';
    miDelOpponent: result := 'DelOpponent';
    miDelBuilding: result := 'DelBuilding';
    miDelHero: result := 'DelHero';
    miCleanupUnusedOpponents: result := 'CleanupUnusedOpponents';

    miStartRoundResult: result := 'StartRoundResult';
    miRequestLogin: result := 'RequestLogin';
    miRequestMap: result := 'RequestMap';
    miRequestMapList: result := 'RequestMapList';
    miRequestMapPreviewInfo: result := 'RequestMapPreviewInfo';
    miRequestLoadMap: result := 'RequestLoadMap';
    miNewMap: result := 'NewMap';
    miInitiateNewGame: result := 'InitiateNewGame';
    miBuyBuilding: result := 'BuyBuilding';
    miSellBuilding: result := 'SellBuilding';
    miRequestFileList: result := 'RequestFileList';
    miRequestPingTimes: result := 'RequestPingTimes';
    miInitiateRound: result := 'InitiateRound';
    miContinue: result := 'Continue';
    miRestartLastWave: result := 'RestartLastWave';
    miWantLevelUpBuilding: result := 'WantLevelUpBuilding';
    miRequestSavegames: result := 'RequestSavegames';
    miAbortWave: result := 'AbortWave';
    miTransferCash: result := 'TransferCash';
    miChangeBuildingStrategy: result := 'ChangeBuildingStrategy';
    miTransferCompleteMapTerrain: result := 'TransferCompleteMapTerrain';
    midecSpeed: result := 'DecSpeed';
    miincSpeed: result := 'IncSpeed';
    midelSaveGame: result := 'del savegame';
    midelSaveGames: result := 'del savegames';
    miRequestPlayerPosChange: result := 'Request player pos change';
    miRequestMapHighscoresAndRating: result := 'Request map highscores and rating';
    miRequestClearMapHighscores: result := 'Request clear map highscores';
    miSendMapRating: result := 'Send map rating';
    miAddRandomWave: result := 'Add random wave';
    miBuyHero: result := 'Buy hero';
    miChangeHeroStrategy: result := 'Change hero strategy';
    miSetHeroTargets: result := 'Set hero targets';
    miKickPlayerOut: result := 'Kick player out';

    miRequestLoginResult: result := 'RequestLoginResult';
    miRequestMapListResult: result := 'RequestMapListResult';
    miRequestMapPreviewInfoResult: result := 'RequestMapPreviewInfoResult';
    miStartRound: result := 'StartRound';
    miLoadMap: result := 'LoadMap';
    miSplashHint: result := 'SplashHint';
    miStart: result := 'Start';
    miUpdateMoveables: result := 'UpdateMoveables';
    miAddBuilding: result := 'AddBuilding';
    miRemoveBuilding: result := 'RemoveBuilding';
    miRequestFileListResult: result := 'RequestFileListResult';
    miEndRound: result := 'EndRound';
    miForceEditMode: result := 'ForceEditMode';
    miLevelUpBuilding: result := 'LevelUpBuilding';
    miLoadGamingdata: result := 'LoadGamingdata';
    miLoadGame: result := 'LoadGame';
    miSaveGame: result := 'SaveGame';
    miRequestSavegamesResult: result := 'RequestSavegamesResult';
    miSetBuildingsToStage: result := 'SetBuildingsToStage';
    miFilesToTransmitCount: result := 'Files to transmit count';
    miRequestMapHighscoresAndRatingResult: result := 'Request map highscores and rating result';
    miAddHero: result := 'Add hero';
    miSetHeroesToLevel: result := 'Set heroes to level';
    miYouWereKickedOut: result := 'You were kicked out';
    miCloseMap: result := 'Close map';

  End;
End;

Function MessageMapPropertyToString(Value: integer): String;
Begin
  result := IntToStr(value);
  Case value Of
    mpSaveMap: result := 'SaveMap';
    mpDescription: result := 'Description';
    mpMapType: result := 'MapType';
    mpMaxPlayer: result := 'MaxPlayer';
    mpLives: result := 'Lives';
    mpResize: result := 'Resize';
    mpCoord: result := 'Coord';
    mpAddWayPoint: result := 'AddWayPoint';
    mpDelWayPoint: result := 'DelWayPoint';
    mpDecPointOrder: result := 'DecPointOrder';
    mpDelPlacement: result := 'DelPlacement';
    mpAddPlacement: result := 'AddPlacement';
    mpAddBuyable: result := 'AddBuyable';
    // mpDelBuyable: result := 'DelBuyable';
    mpWaveCount: result := 'WaveCount';
    mpCashOnWaveStart: result := 'CashOnWaveStart';
    mpWaveHint: result := 'WaveHint';
    mpWaveOpponents: result := 'WaveOpponents';
    mpWaveOpponent: result := 'WaveOpponent';
    mpWaveOpponentCount: result := 'WaveOpponentCount';
    mpWaveOpponentCashPerUnit: result := 'WaveOpponentCashPerUnit';
    mpWaveOpponentUnitsPerSpawn: result := 'WaveOpponentUnitsPerSpawn';
    mpWaveOpponentSpawnDelay: result := 'WaveOpponentSpawnDelay';
    mpWaveOpponentSpawnDelta: result := 'WaveOpponentSpawnDelta';
    mpBackTex: result := 'BackTex';
    mpDC1Tex: result := 'Damageclass 1 image';
    mpDC2Tex: result := 'Damageclass 2 image';
    mpDC3Tex: result := 'Damageclass 3 image';
    mpDC4Tex: result := 'Damageclass 4 image';
    mpDelOppInWave: result := 'Delete Opponent in Wave';
    mpWCoord: result := 'Set / clear coordinate';
  End;
End;

End.


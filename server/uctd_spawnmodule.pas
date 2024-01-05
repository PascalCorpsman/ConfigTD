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
Unit uctd_spawnmodule;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils,

  uctd_map, uctd_opp;

Type

  { TSpawnModul }

  TSpawnModul = Class
  private
    fmap: TMap;
    fWave: integer;
    fStartTime: int64;
    flastUpdateTime: int64;
    FPausing: Boolean;
    fPauseTime: int64;
    FOpponentRoundInfo: Array Of integer;
    fFinished: Boolean;
    fPlayerCount: integer;
    fSpawnIndex: uint16;
  public
    Property Finished: Boolean read fFinished;
    Constructor create; virtual;
    Destructor destroy; override;
    Procedure Init(Const Map: TMap; Wave, PlayerCount: integer);
    Procedure Start();
    Procedure Clear;
    Procedure Pause(value: Boolean);
    // Wird in OnRender aufgerufen und kontrolliert damit dann das Erzeugen der Gegner
    // Ergebniss = True, wenn alle für diese Runde zu erzeugenden Gegner erzeugt wurden.
    Function Update(): Boolean;
  End;

Implementation

Uses
  lclintf
  , uvectormath
  , uctd_common;

{ TSpawnModul }

Constructor TSpawnModul.create;
Begin
  Inherited create;
End;

Destructor TSpawnModul.destroy;
Begin
  clear;
End;

Procedure TSpawnModul.Init(Const Map: TMap; Wave, PlayerCount: integer);
Var
  i: Integer;
Begin
  Clear;
  fmap := map;
  fWave := wave;
  fStartTime := 0;
  setlength(FOpponentRoundInfo, high(fmap.Waves[wave].Opponents) + 1);
  For i := 0 To high(FOpponentRoundInfo) Do Begin
    FOpponentRoundInfo[i] := 0;
  End;
  FPausing := false;
  fFinished := false;
  fPlayerCount := PlayerCount;
End;

Procedure TSpawnModul.Start;
Var
  n: int64;
Begin
  FPausing := False;
  fFinished := false;
  n := GetTick();
  fStartTime := n;
  flastUpdateTime := n;
  fSpawnIndex := 0;
End;

Procedure TSpawnModul.Clear;
Begin
  setlength(FOpponentRoundInfo, 0);
End;

Procedure TSpawnModul.Pause(value: Boolean);
Var
  i: int64;
Begin
  If FPausing Then Begin
    If Not value Then Begin
      i := GetTick() - fPauseTime;
      fStartTime := fStartTime + i; // Wir Verschieben die StartZeit um die Zeit der Pause in die Zukunft
      flastUpdateTime := flastUpdateTime + i;
    End;
  End
  Else Begin
    If value Then Begin
      fPauseTime := GetTick();
    End;
  End;
  FPausing := value;
End;

Function TSpawnModul.Update: Boolean;
Var
  FillCounter: Integer;
  startingPoints: Array Of TPoint;

  Procedure Fill(x, y: integer);
  Begin
    // Nur Koordinaten auf denen der Gegner nachher auch wirklich generiert werden kann
    If fmap.CoordIsWalkAble(x, y) Then Begin
      startingPoints[FillCounter].X := x;
      startingPoints[FillCounter].Y := y;
      inc(FillCounter);
    End;
  End;

Var
  PlayerIndex, i: Integer;
  t0, t: int64;
  op: TOpponent;
  b: Boolean;
  s: String;
  sp: Tpoint;
  x, j: Integer;
Begin
  If fFinished Then Begin
    result := true;
    exit;
  End;
  result := false;
  If FPausing Then exit;
  t := GetTick();
  If Speedup <> 1 Then Begin // Bei einer Geschwindigkeitssteigerung verschieben wir den Startzeitpunkt jeweils um Speedup in die Vergangenheit
    t0 := t - flastUpdateTime; // Soviel Zeit ist seit dem Letzten Frame vergangen
    t0 := t0 * (int64(Speedup) - 1); // berechnen der "zusätzlich" vergangen Zeit
    fStartTime := fStartTime - t0; // und ab in die Vergangenheit
  End;
  flastUpdateTime := t;
  t0 := t - fStartTime; // t0 = Zeit in ms, seit die Runde gestartet wurde..
  For i := 0 To high(fmap.Waves[fWave].Opponents) Do Begin
    t := t0 - fmap.Waves[fWave].Opponents[i].SpawnDelay;
    If t >= 0 Then Begin // Wenn die SpawnDelay Zeit Abgelaufen ist
      // Eine Spawn Delta Zeit ist abgelaufen
      If t >= int64(FOpponentRoundInfo[i]) * int64(fmap.Waves[fWave].Opponents[i].Spawndelta) Then Begin
        s := MapFolder + MapName + PathDelim + fmap.Waves[fWave].Opponents[i].opponent;
        For PlayerIndex := 0 To fPlayerCount - 1 Do Begin
          startingPoints := Nil;
          // Einheiten Erzeugen
          For j := 0 To fmap.Waves[fWave].Opponents[i].UnitsPerSpawn - 1 Do Begin
            // Begrenzung der Gesendeten Einheuten auf die Angabe des Benutzers
            If FOpponentRoundInfo[i] * fmap.Waves[fWave].Opponents[i].UnitsPerSpawn + j < fmap.Waves[fWave].Opponents[i].Count Then Begin
              sp := fmap.Waypoints[PlayerIndex, 0].Field[0];
              op := TOpponent.create();
              op.LoadFromFile(s);
              If op.HasAnimation Then Begin
                op.AnimationOffset := random(65536); // So sind alle Gegner mit einem "Zufälligen" Animationsstep -> es sieht nicht alles so Monoton aus...
              End
              Else Begin
                op.AnimationOffset := 0;
              End;
              op.RenderIndex := i; // Der Render, damit beim Update der Clients, die Richtigen Daten gefunden werden können.
              op.Identifier := fSpawnIndex; // Damit der Client eine Eindeutige Zuordnung der Opponents hat
              op.InitDamageByPlayers(fPlayerCount);
              fSpawnIndex := fSpawnIndex + 1;
              // Bei Mehreren Einheiten Per Spawn muss die Startposition gejittert werden.
              // Das hier ist noch nicht Perfekt, aber immerhin separiert es die meisten Objekte schon mal ein bischen.
              If fmap.fTerrain[sp.x, sp.Y].WArea Then Begin
                If Not assigned(startingPoints) Then Begin
                  setlength(startingPoints, length(fmap.Waypoints[PlayerIndex, 0].Field));
                  FillCounter := 0;
                  For x := 0 To high(fmap.Waypoints[PlayerIndex, 0].Field) Do Begin
                    Fill(fmap.Waypoints[PlayerIndex, 0].Field[x].X, fmap.Waypoints[PlayerIndex, 0].Field[x].Y);
                  End;
                  setlength(startingPoints, FillCounter);
                End;
                If Not assigned(startingPoints) Then Begin
                  // Irgendwas ging schief, dann Jittern wir wie schon immer ...
                  op.Position := sp + JitterCoord(j, fmap.Waves[fWave].Opponents[i].UnitsPerSpawn);
                End
                Else Begin
                  // Den Gegner irgendwo zufällig auf der Fläche erzeugen
                  op.Position := startingPoints[random(length(startingPoints))];
                End;
              End
              Else Begin
                op.Position := sp + JitterCoord(j, fmap.Waves[fWave].Opponents[i].UnitsPerSpawn);
              End;
              // Prüfen ob die Koordinate die wir gejittert haben überhaupt begehbar ist.
              If Not op.Canfly Then Begin
                If Not fmap.CoordIsWalkAble(round(op.Position.x), round(op.Position.y)) Then Begin
                  // das Jittern hat uns eine Unerlaubte Koordinate gegeben, also reset auf den "Ursprung"
                  op.Position := sp;
                End;
              End;
              op.Refund := fmap.Waves[fWave].Opponents[i].refund;
              fmap.AddOpponentObject(op, PlayerIndex); // Lassen wir den Kleinen Schatz los auf seine Welt ;)
            End
            Else Begin
              break;
            End;
          End;
          setlength(startingPoints, 0);
        End;
        FOpponentRoundInfo[i] := FOpponentRoundInfo[i] + 1; // Mit Zählen, wieviele Runden wir schon gefeuert haben.
      End;
    End;
  End;
  // Prüfen auf Fertig = Alle zu sendenden Einheiten Sind Raus.
  b := true;
  For i := 0 To high(FOpponentRoundInfo) Do Begin
    If FOpponentRoundInfo[i] * fmap.Waves[fWave].Opponents[i].UnitsPerSpawn < fmap.Waves[fWave].Opponents[i].Count Then Begin
      b := false;
      break;
    End;
  End;
  If b Then Begin
    fFinished := true;
    result := true;
  End;
End;

End.


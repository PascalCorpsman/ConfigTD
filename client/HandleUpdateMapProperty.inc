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
(*
 * !! ACHTUNG !!
 * Verwendete Variablen müssen im Client und Server Initialisiert werden
 *
 * Bisheriger Kopf ;

 Procedure xx.yy(MapProperty: integer; Const Data: TStream; UID: integer);
 Var
   s, t: String;
   i, j, c: integer;
   m: TMemorystream;
   tmp: TPoint;
   bk: TBuyAbleKind;

 *)
Case MapProperty Of
  mpDelOppInWave: Begin
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      fmap.DelOppInWave(i, j);
    End;
  mpBackTex: Begin
      s := data.ReadAnsiString;
      fmap.BackTex := s;
    End;
  mpDC1Tex: Begin
      s := data.ReadAnsiString;
      fmap.DamageClass1Tex := s;
    End;
  mpDC2Tex: Begin
      s := data.ReadAnsiString;
      fmap.DamageClass2Tex := s;
    End;
  mpDC3Tex: Begin
      s := data.ReadAnsiString;
      fmap.DamageClass3Tex := s;
    End;
  mpDC4Tex: Begin
      s := data.ReadAnsiString;
      fmap.DamageClass4Tex := s;
    End;
  mpSaveMap: Begin
      // Nop : Hier wird ja nichts geändert, es geht lediglich darum den llError Log zu verhindern
    End;
  mpWaveCount: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := high(fmap.Waves) + 1;
      setlength(fmap.Waves, i);
      For i := j To High(fMap.Waves) Do Begin
        // Reset der Wave, genau Gleich wie die LCL das macht
        fmap.Waves[i].ChashOnStart := 0;
        fmap.Waves[i].WaveHint := '';
        setlength(fmap.Waves[i].Opponents, 1);
        fmap.Waves[i].Opponents[0].opponent := '';
        fmap.Waves[i].Opponents[0].Count := 10;
        fmap.Waves[i].Opponents[0].refund := 1;
        fmap.Waves[i].Opponents[0].UnitsPerSpawn := 1;
        fmap.Waves[i].Opponents[0].SpawnDelay := 2500;
        fmap.Waves[i].Opponents[0].Spawndelta := 1000;
      End;
    End;
  mpWaveHint: Begin
      fmap.Change(false);
      i := -1;
      data.Read(i, sizeof(i));
      s := data.ReadAnsiString;
      fmap.Waves[i].WaveHint := s;
    End;
  mpWaveOpponentCount: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      fMap.Waves[i].Opponents[j].Count := c;
    End;
  mpWaveOpponentCashPerUnit: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      fMap.Waves[i].Opponents[j].refund := c;
    End;
  mpWaveOpponentUnitsPerSpawn: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      fMap.Waves[i].Opponents[j].UnitsPerSpawn := c;
    End;
  mpWaveOpponentSpawnDelay: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      fMap.Waves[i].Opponents[j].SpawnDelay := c;
    End;
  mpWaveOpponentSpawnDelta: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      fMap.Waves[i].Opponents[j].Spawndelta := c;
    End;
  mpWaveOpponent: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      s := Data.ReadAnsiString;
      fMap.Waves[i].Opponents[j].opponent := s;
    End;
  mpWaveOpponents: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := high(fMap.Waves[i].Opponents) + 1;
      setlength(fMap.Waves[i].Opponents, j);
      For j := c To high(fMap.Waves[i].Opponents) Do Begin
        fmap.Waves[i].Opponents[j].opponent := '';
        fmap.Waves[i].Opponents[j].Count := 10;
        fmap.Waves[i].Opponents[j].refund := 1;
        fmap.Waves[i].Opponents[j].UnitsPerSpawn := 1;
        fmap.Waves[i].Opponents[j].SpawnDelay := 2500;
        fmap.Waves[i].Opponents[j].Spawndelta := 1000;
      End;
    End;
  mpCashOnWaveStart: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      fMap.Waves[i].ChashOnStart := j;
    End;
  mpAddPlacement: Begin
      fmap.Change(false);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      s := Data.ReadAnsiString;
      fMap.addPlacement(i, j, s, c);
    End;
  {mpDelBuyable: Begin
      fmap.Change(true);
      s := Data.ReadAnsiString;
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      fmap.delBuyable(s, i, j);
    End;}
  mpUpdateBuyable: Begin
      s := Data.ReadAnsiString;
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      fmap.updateBuyable(s, i, j);
    End;
  mpAddBuyable: Begin
      fmap.Change(true);
      s := Data.ReadAnsiString;
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      bk := bkBuilding;
      data.read(bk, sizeof(bk));
      fmap.addBuyable(s, bk, i, j);
    End;
  mpDelPlacement: Begin
      fmap.Change(false);
      c := -1;
      data.Read(c, sizeof(c));
      j := -1;
      data.Read(j, sizeof(j));
      fmap.deletePlacement(c, j);
    End;
  mpDecPointOrder: Begin
      fmap.Change(true);
      c := -1;
      data.Read(c, sizeof(c));
      j := -1;
      data.Read(j, sizeof(j));
      tmp := fmap.Waypoints[c, j - 1].Point;
      fmap.Waypoints[c, j - 1] := fmap.Waypoints[c, j];
      fmap.Waypoints[c, j].Point := tmp;
    End;
  mpDelWayPoint: Begin
      fmap.Change(true);
      c := -1;
      data.Read(c, sizeof(c));
      j := -1;
      data.Read(j, sizeof(j));
      For i := j To high(fMap.Waypoints[c]) - 1 Do Begin
        fMap.Waypoints[c, i] := fMap.Waypoints[c, i + 1]
      End;
      setlength(fMap.Waypoints[c], high(fMap.Waypoints[c]));
    End;
  mpAddWayPoint: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      setlength(fmap.Waypoints[i], high(fmap.Waypoints[i]) + 2);
      fmap.Waypoints[i, high(fmap.Waypoints[i])].Point := point(j, c);
      fmap.Waypoints[i, high(fmap.Waypoints[i])].Field := Nil;
    End;
  mpCoord: Begin
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      If fMap.fTerrain[i, j].data <> c Then Begin
        fmap.Change(true);
        fMap.fTerrain[i, j].data := c;
{$IFDEF Client}
        fmap.UpdateBackTexCoord(i, j);
{$ENDIF}
      End;
    End;
  mpWCoord: Begin
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      c := -1;
      data.Read(c, sizeof(c));
      If fmap.fTerrain[i, j].WArea <> odd(c) Then Begin
        fmap.Change(true);
        fMap.fTerrain[i, j].WArea := odd(c);
      End;
    End;
  mpResize: Begin
      fmap.Change(true);
      i := -1;
      data.Read(i, sizeof(i));
      j := -1;
      data.Read(j, sizeof(j));
      fmap.Resize(i, j);
    End;
  mpMapType: Begin
      i := -1;
      data.Read(i, sizeof(i));
      fmap.MapType := IntToMapType(i);
    End;
  mpMaxPlayer: Begin
      i := -1;
      data.Read(i, sizeof(i));
      fMap.MaxPlayer := i;
      For j := high(fmap.Waypoints) Downto i Do
        setlength(fmap.Waypoints[j], 0);
      setlength(fmap.Waypoints, i);
    End;
  mpLives: Begin
      i := -1;
      data.Read(i, sizeof(i));
      fmap.Lives[0] := i;
      i := -1;
      data.Read(i, sizeof(i));
      fmap.Lives[1] := i;
      i := -1;
      data.Read(i, sizeof(i));
      fmap.Lives[2] := i;
    End;
  mpDescription: Begin
      s := Data.ReadAnsiString;
      fMap.Description := s;
    End
Else Begin
    log('Unknown map property : ' + inttostr(MapProperty), llError);
  End;
End;


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
Unit uctd_bullet;

{$MODE objfpc}{$H+}

{$I apptype.inc}

Interface

Uses
  Classes, SysUtils
  , uvectormath, uctd_mapobject, uctd_common
  , uctd_opp;

Type

  { TBulletObject }

  TBulletObject = Class(tctd_BulletObject)
  private

  public
    pPosition: TVector2; // hat das Bullet eine Negative Speed, bewegt es sich erst an diese Position bevor es Aktiv ist.
    Direction: integer; // Wird nur bei Bullets ohne Geschwindigkeit geschrieben sonst selbst berechnet
    Handled: Boolean; // Wurde das Bullet in diesem Schritt schon bearbeitet  (nur intern)?
    Target: TOpponent; // Das Ziel, welches Beschossen werden soll
    Fimage: integer; // OpenGL Textur
    AnimationOffset: uint16; // Animationen benötigen ein Animation Offset, damit die nicht alle "Gleich" aussehen
    Position: TVector2; // Unsere Aktuelle Position
    Width: Single; // In Pixel, weil quasi Statisch (wird beim erzeugen gesetzt und spart beim Rendern Berechnungen)
    Height: single; // In Pixel, weil quasi Statisch (wird beim erzeugen gesetzt und spart beim Rendern Berechnungen)
    Speed: Single; // Die Fluggeschwindigkeit
    Power: TPower; // Unsere
    Splash: single; // Suchen wir ein neues Ziel, wenn wir eingeschlagen sind und noch Power haben ?
    Earn: Integer; // Den Profit den der bullet macht, wenn er einschlägt
    Slowdown: TSlowDown;
    Owner: tctd_mapopbject; // Wenn der Bullet das Ziel Trifft, brauchen wir diese info's damit wir zuordnen können, wer die Belohnung bekommt.
    RenderIndex: integer;
    Strategy: TBuildingStrategy;
    PreverAir: Boolean;
    DamgeDealt: Array[0..3] Of integer; // -- Der Schaden den der Bullet je Schadensklasse angerichtet hat -> wird in jedem Update neu berechnet
    Constructor Create;
    Destructor destroy; override;
    (*
     * Rückgabewerte :
     *  0 = Nichts Passiert
     *  1 = Wir haben Eingeschlagen und müssen selbst vernichtet werden
     *  2 = Wir haben Eingeschlagen und das Ziel muss vernichtet werden
     *  3 = Beide haben es nicht überlebt
     *  4 = wir haben zwar noch Power, können aber an diesem Gegner nichts mehr ausrichten
     *)
    Function Update(delta: int64; Const Opponents: TOpponentInfoArray): integer; // TODO: Warum sieht das der Client ??
    Procedure GetMovingState(Const Stream: TSTream);
  End;

Implementation

Uses
  math;

{ TBulletObject }

Constructor TBulletObject.Create;
Begin

End;

Destructor TBulletObject.destroy;
Begin
  Inherited destroy;
End;

Function TBulletObject.Update(delta: int64; Const Opponents: TOpponentInfoArray
  ): integer;

  Function Hit: Integer;
  Var
    st, se, i: integer;
  Begin
    result := 0;
    st := 0;
    se := 0;
    // Berücksichtigen der Slowdowns
    target.SlowDown(Self);
    // Berücksichtigen der Schadensmodelle
    For i := 0 To 3 Do Begin
      If Power[i] > target.LifePoints[i] Then Begin
        DamgeDealt[i] := target.LifePoints[i];
        power[i] := power[i] - target.LifePoints[i];
        target.LifePoints[i] := 0;
      End
      Else Begin
        DamgeDealt[i] := power[i];
        target.LifePoints[i] := target.LifePoints[i] - power[i];
        power[i] := 0;
      End;
      st := st + target.LifePoints[i];
      se := se + Power[i];
    End;
{$IFDEF SERVER}
    // Mit zählen, welcher Spieler wie viel Schaden angerichtet hat.
    Target.DamageByPlayers[owner.Owner] := Target.DamageByPlayers[owner.Owner] + DamgeDealt[0] + DamgeDealt[1] + DamgeDealt[2] + DamgeDealt[3];
{$ENDIF}
    If st <= 0 Then result := result Or 2;
    If se <= 0 Then result := result Or 1;
    If (st > 0) And (se > 0) Then Begin // Wir haben den Gegener Beschossen, haben aber keine Schadensklasse mehr, die ihn Umbringt.
      result := result Or 4;
    End;
  End;

Var
  ll, l, dx, dy, dxb, dyb: single;
  j: integer;
  bool, bb: Boolean;
Begin
  result := 0;
  DamgeDealt[0] := 0;
  DamgeDealt[1] := 0;
  DamgeDealt[2] := 0;
  DamgeDealt[3] := 0;
  If Speed = 0 Then Begin
    // Der Bullet liegt nur so Rum und schaut ob ein Gegner ihn zufällig trifft ;)
    (*
     * Wenn der Bullet einen Gegner zum Abballern findet, dann setzt er ihn als Target
     * und setzt entsprechend Result
     *)
    l := sqr(Width + Splash) + sqr(Height + Splash);
    For j := 0 To high(Opponents) Do Begin
      dx := (Opponents[j].Obj.Position.x + Opponents[j].Obj.SizeX * MapBlockSize / 2) - Position.x;
      dy := (Opponents[j].Obj.Position.y + Opponents[j].Obj.Sizey * MapBlockSize / 2) - Position.y;
      If (sqr(dx) + sqr(dy) <= l) {$IFDEF SERVER}And (Opponents[j].Alive){$ENDIF}    Then Begin
        bool := (Opponents[j].Obj.isSlowing(Owner, bb) = -1); // True, wenn wir den Gegner noch nicht beschiesen
        bool := bool Or (Not bb); // Wenn wir ihn Beschießen, aber die Dynamic Zeit abgelaufen ist, kann er auch wieder beschossen werden.
        // Wenn der Bullet dem Gegner einen Schaden zuführen kann, nur dann wählen wir ihn als Target
        If ((Opponents[j].Obj.LifePoints[0] > 0) And (Power[0] > 0)) Or
          ((Opponents[j].Obj.LifePoints[1] > 0) And (Power[1] > 0)) Or
          ((Opponents[j].Obj.LifePoints[2] > 0) And (Power[2] > 0)) Or
          ((Opponents[j].Obj.LifePoints[3] > 0) And (Power[3] > 0) Or
          (Earn > 0) Or
          (SlowDown.slowdownstatic <> 1) Or ((SlowDown.slowdowndynamic <> 1) And Bool)
          ) Then Begin
          self.Target := Opponents[j].Obj;
          result := Hit();
          break;
        End;
      End;
    End;
  End
  Else Begin
    // Bewegt das Objekt und schlägt ggf ein.
    If Speed < 0 Then Begin
      dx := pPosition.x - Position.x;
      dy := pPosition.y - Position.y;
    End
    Else Begin
      // 2. Berechnen Delta
      dx := (target.Position.x + target.SizeX * MapBlockSize / 2) - Position.x;
      dy := (target.Position.y + target.Sizey * MapBlockSize / 2) - Position.y;
    End;
    //Bakup für Nachher
    dxb := dx;
    dyb := dy;
    // 3. Berechnen der Länge in Pixeln
    l := sqrt(sqr(dx) + sqr(dy));
    If l = 0 Then Begin
      l := 1;
    End;
    // 4. Normieren auf 1 Pixel
    dx := dx / l;
    dy := dy / l;
    // 5. Berechnen der Schaffbaren Wegstrecke, bei der Geschwindigkeit die unser Opponent hat
    ll := abs(Speed) * MapBlockSize; // GetMap_Block_Size Pro Sekunde
    ll := ll * delta / 1000; // Pixel in der Vergangenen Zeit.
    // 6. Wenn wir übers Ziel hinausschießen würden (weil der Benutzer den Opponent zu schnell gemacht hat
    If ll > l Then ll := l;
    // 7. Den Wegfortschritt Berechnen
    dx := dx * ll;
    dy := dy * ll;
    // 8. Tatsächlich fliegen
    Position.x := Position.x + dx;
    Position.y := Position.y + dy;
    Direction := round(radtodeg(arctan2(dy, dx)));
    // Kollision ?
    dx := dxb;
    dy := dyb;
    // Wenn das Geschoß in das Target eingeschlagen ist
    If Speed > 0 Then Begin
      If sqr(dx) + sqr(dy) <= sqr((target.SizeX + target.Sizey) * MapBlockSize / 4) Then Begin
        result := hit();
      End;
    End
    Else Begin // Oder wenn das Geschoss an seinem "Legepunkt" angekommen ist.
      If (sqr(dx) + sqr(dy) <= sqr((width + height) * MapBlockSize / 4)) Or (l <= min(width, height)) Then Begin
        Speed := 0;
      End;
    End;
  End;
End;

Procedure TBulletObject.GetMovingState(Const Stream: TSTream);
Var
  data: TRenderBullet;
Begin
  data.Index := renderindex;
  data.Position := Position;
  data.Angle := Direction;
  data.AnimationOffset := AnimationOffset;
  stream.Write(data, sizeof(data));
End;

End.


(******************************************************************************)
(* uquadtree                                                       29.07.2025 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a generic quadtree                         *)
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
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - bufferd queue function                                *)
(*               0.03 - ADD: Property Count                                   *)
(*                                                                            *)
(******************************************************************************)
Unit uquadtree;

{$MODE ObjFPC}{$H+}

Interface

(*
 * If you get a compiler error with missing file
 * just create a file namend "uquadtree.inc" in your project folder and
 * insert the following content:
 *
 * ---------- Content of file ----------
   // Mit diesem Schalter kann man optional die uVectormath.pas zur Typ
   // Definition nutzen.

   {$DEFINE UseVectormath}

   ---------- End content of file ----------
 *)

{$I uquadtree.inc}

Uses
  Classes, SysUtils
{$IFDEF UseVectormath}
  , uvectormath
{$ENDIF}
  ;

Type

{$IFNDEF UseVectormath}
  TVector2 = Record
    Case boolean Of
      false: (x, y: Single);
      true: (data: Array[0..1] Of Single);
  End;
{$ENDIF}

  TQuadTreeRect = Record
    TopLeft, BottomRight: TVector2;
  End;

  TQuadTreeDirection = (
    qtdNorthWest,
    qtdNorthEast,
    qtdSouthWest,
    qtdSouthEast
    );

  { TQuadTree }

  Generic TQuadTree < T > = Class
  private
    fBoundary: TQuadTreeRect;
    fCapacity: integer;
    fActualCapacity: integer;
  public
    Type
      TQuadTreeElement = Record
        Position: TVector2;
        Data: T;
      End;
      PQuadTreeElement = ^TQuadTreeElement;
      TQuadTreeElementArray = Array Of TQuadTreeElement;

      TOnFreeElement = Procedure(Sender: TObject; Var aElement: TQuadTreeElement) Of Object;
  private
    fElements: TQuadTreeElementArray;
    fSubtrees: Array[TQuadTreeDirection] Of TQuadTree;
    fdivided: Boolean;
    fCount: integer;
    Procedure Subdivide;
    Procedure Query2Internal(Const aRange: TQuadTreeRect; Const Buffer: PQuadTreeElement; Var resultElements: integer; Const maxAllowedElements: integer);
  public
    OnFreeElement: TOnFreeElement;
    Property Count: integer read fCount; // Number of elements stored in the Quadtree (and all its subtrees)

    Constructor Create(Const aBoundary: TQuadTreeRect; aCapacity: Integer = 4); virtual;
    Destructor Destroy; override;

    Function Add(Const aElement: TQuadTreeElement): Boolean;
    Procedure Clear;

    Function Query(Const aRange: TQuadTreeRect): TQuadTreeElementArray;

    (*
     * Wie Query, nur das keine Dynamischen Allokationen stattfinden und stattdessen Buffer verwendet wird
     * Dieser muss vorher allokiert worden sein und Platz für mindestens maxAllowedElements Elemente beinhalten.
     * In:
     *     aRange
     *     Buffer[maxAllowedElements]
     *     maxAllowedElements         - max size that can be used from Buffer
     * Out:
     *     Buffer[maxAllowedElements]
     *     resultElements         - number of actual "found" Elements
     *)
    Procedure Query2(Const aRange: TQuadTreeRect; Const Buffer: PQuadTreeElement; Out resultElements: integer; Const maxAllowedElements: integer);
  End;

  (*
   * Some Helper functions
   *)
Function QuadTreeRect(Const TopLeft, BottomRight: TVector2): TQuadTreeRect;
Function QuadTreeRectIntersects(Const A, B: TQuadTreeRect): Boolean;
Function QuadTreeRectContainsPoint(Const R: TQuadTreeRect; Const P: TVector2): Boolean;

{$IFNDEF UseVectormath}
Function V2(Const X, Y: Single): TVector2;
{$ENDIF}

Implementation

{$IFNDEF UseVectormath}
Function V2(Const X, Y: Single): TVector2;
Begin
  result.x := x;
  result.y := y;
End;
{$ENDIF}

Function QuadTreeRect(Const TopLeft, BottomRight: TVector2): TQuadTreeRect;
Begin
  result.TopLeft := TopLeft;
  result.BottomRight := BottomRight;
End;

Function QuadTreeRectIntersects(Const A, B: TQuadTreeRect): Boolean;
Begin
  result := Not (
    (A.TopLeft.x > B.BottomRight.x) Or
    (A.BottomRight.x < B.TopLeft.x) Or
    (A.TopLeft.y > B.BottomRight.y) Or
    (A.BottomRight.y < B.TopLeft.y)
    );
End;

Function QuadTreeRectContainsPoint(Const R: TQuadTreeRect; Const P: TVector2): Boolean;
Begin
  result :=
    (p.X >= r.TopLeft.x) And
    (p.X <= r.BottomRight.x) And
    (p.Y >= r.TopLeft.y) And
    (p.Y <= r.BottomRight.y);
End;

{ TQuadTree }

Constructor TQuadTree.Create(Const aBoundary: TQuadTreeRect; aCapacity: Integer);
Var
  i: TQuadTreeDirection;
Begin
  fBoundary := aBoundary;
  fCapacity := aCapacity;
  OnFreeElement := Nil;
  fActualCapacity := 0;
  setlength(fElements, fCapacity);
  For i In TQuadTreeDirection Do
    fSubtrees[i] := Nil;
  fdivided := false;
  fCount := 0;
End;

Destructor TQuadTree.Destroy;
Begin
  Clear();
  Inherited Destroy;
End;

Procedure TQuadTree.Clear;
Var
  i: TQuadTreeDirection;
  j: Integer;
Begin
  If fdivided Then Begin
    For i In TQuadTreeDirection Do Begin
      fSubtrees[i].free;
      fSubtrees[i] := Nil;
    End;
  End;
  If assigned(OnFreeElement) Then Begin
    For j := 0 To fActualCapacity - 1 Do Begin
      OnFreeElement(self, fElements[j]);
    End;
  End;
  fActualCapacity := 0;
  fdivided := false;
  fCount := 0;
End;

Procedure TQuadTree.Subdivide;
Var
  tl, br, M: TVector2;
Begin
  M.x := (fBoundary.TopLeft.x + fBoundary.BottomRight.x) / 2;
  M.y := (fBoundary.TopLeft.y + fBoundary.BottomRight.y) / 2;
  tl := fBoundary.TopLeft;
  br := fBoundary.BottomRight;
  fSubtrees[qtdNorthWest] := TQuadTree.Create(QuadTreeRect(tl, m), fCapacity);
  fSubtrees[qtdNorthEast] := TQuadTree.Create(QuadTreeRect(v2(m.x, tl.y), v2(br.x, m.y)), fCapacity);
  fSubtrees[qtdSouthWest] := TQuadTree.Create(QuadTreeRect(v2(tl.x, m.y), v2(m.x, br.y)), fCapacity);
  fSubtrees[qtdSouthEast] := TQuadTree.Create(QuadTreeRect(m, br), fCapacity);
  fdivided := true;
End;

Function TQuadTree.Add(Const aElement: TQuadTreeElement): Boolean;
Begin
  result := false;
  If (Not QuadTreeRectContainsPoint(fBoundary, aElement.Position)) Then exit;
  inc(fCount);
  If fActualCapacity < fCapacity Then Begin
    fElements[fActualCapacity] := aElement;
    inc(fActualCapacity);
    result := true;
  End
  Else Begin
    If Not fdivided Then Begin
      subdivide;
    End;
    result := fSubtrees[qtdNorthWest].Add(aElement);
    If result Then exit;
    result := fSubtrees[qtdNorthEast].Add(aElement);
    If result Then exit;
    result := fSubtrees[qtdSouthWest].Add(aElement);
    If result Then exit;
    result := fSubtrees[qtdSouthEast].Add(aElement);
    If result Then exit;
  End;
End;

Function TQuadTree.Query(Const aRange: TQuadTreeRect): TQuadTreeElementArray;
Var
  i: Integer;
  j: TQuadTreeDirection;
Begin
  result := Nil;
  If (Not QuadTreeRectIntersects(fBoundary, aRange)) Then exit;
  For i := 0 To fActualCapacity - 1 Do Begin
    If (QuadTreeRectContainsPoint(aRange, fElements[i].Position)) Then Begin
      setlength(result, high(result) + 2);
      result[high(result)] := fElements[i];
    End;
  End;
  If fdivided Then Begin
    For j In TQuadTreeDirection Do Begin
      result := Concat(result, fSubtrees[j].Query(aRange));
    End;
  End;
End;

Procedure TQuadTree.Query2Internal(Const aRange: TQuadTreeRect;
  Const Buffer: PQuadTreeElement; Var resultElements: integer;
  Const maxAllowedElements: integer);
Var
  i: Integer;
  j: TQuadTreeDirection;
Begin
  If (Not QuadTreeRectIntersects(fBoundary, aRange)) Then exit;
  If resultElements >= maxAllowedElements Then exit;
  For i := 0 To fActualCapacity - 1 Do Begin
    If (QuadTreeRectContainsPoint(aRange, fElements[i].Position)) Then Begin
      buffer[resultElements] := fElements[i];
      inc(resultElements);
      If resultElements >= maxAllowedElements Then exit;
    End;
  End;
  If fdivided Then Begin
    For j In TQuadTreeDirection Do Begin
      fSubtrees[j].Query2Internal(aRange, Buffer, resultElements, maxAllowedElements);
    End;
  End;
End;

Procedure TQuadTree.Query2(Const aRange: TQuadTreeRect;
  Const Buffer: PQuadTreeElement; Out resultElements: integer;
  Const maxAllowedElements: integer);
Begin
  resultElements := 0;
  Query2Internal(aRange, Buffer, resultElements, maxAllowedElements);
End;

End.


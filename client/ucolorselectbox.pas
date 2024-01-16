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
Unit ucolorselectbox;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ExtCtrls, Graphics, Dialogs;

Type

  { TColorSelectBox }

  TColorSelectBox = Class(TPaintBox)
  private
    fSelected: TColor;
    fDialog: TColorDialog;
    Procedure SetSelected(AValue: TColor);
    Procedure SelPaint(Sender: TObject);
    Procedure SelColor(Sender: TObject);
  public
    Property Selected: TColor read fSelected write SetSelected;
    Constructor Create(AOwner: TComponent); override;
    Destructor destroy; override;
  End;

Implementation

{ TColorSelectDialog }

Constructor TColorSelectBox.Create(AOwner: TComponent);
Begin
  Inherited create(AOwner);
  OnPaint := @SelPaint;
  OnClick := @SelColor;
  Selected := clBlack;
  fDialog := TColorDialog.Create(self);
End;

Destructor TColorSelectBox.destroy;
Begin
  fDialog.Free;
  Inherited destroy;
End;

Procedure TColorSelectBox.SetSelected(AValue: TColor);
Begin
  If fSelected = AValue Then Exit;
  fSelected := AValue;
  Invalidate;
End;

Procedure TColorSelectBox.SelPaint(Sender: TObject);
Begin
  Canvas.Brush.Color := fSelected;
  Canvas.Pen.Color := clblack;
  Canvas.Rectangle(0, 0, Width - 1, Height - 1);
End;

Procedure TColorSelectBox.SelColor(Sender: TObject);
Begin
  fDialog.Color := fSelected;
  If fDialog.Execute Then Begin
    fSelected := fDialog.Color;
    Invalidate;
  End;
End;

End.


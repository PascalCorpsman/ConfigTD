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
 * Ist dieser Switch gesetzt, dann werden WinXP inkompatible routinen durch "einfachere" ersetzt
 *
 * z.B.: GetTickCount64 durch die 32-Bit variante
 *
 * ! ACHTUNG !
 * Client und Server müssen bei Änderung beide neu kompiliert werden.
 *)
{.$DEFINE WinXPMode}


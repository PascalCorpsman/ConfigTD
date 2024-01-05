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
 * Zur Unterscheidung ob Server oder Client
 * Da die IDE hier durcheinander kommt ist dieses Define gesetzt unter
 * Projekt -> Projekteinstellungen -> Compilereinstellungen -> Benutzerdefinierte Einstellungen
 *      -dServer
 *)
{.$DEFINE Server}

(*
 * Wenn dieser Switch gesetzt ist, dann wird die sehr Hochfrequente Lognachricht
 * UpdateMoveables nicht im Trace geloggt.
 *
 * Ignoriert werden :
 *
 * - miHeartBeat
 * - miUpdateMoveables
 *)
{$DEFINE DoNotLog_CyclicMessages}

{$I ../globalsettings.inc}


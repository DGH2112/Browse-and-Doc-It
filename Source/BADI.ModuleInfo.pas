(**

  This module contains a class to represent module information stored in the Modules Dispatcher.

  @Author  David Hoyle
  @Version 1.1
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit BADI.ModuleInfo;

Interface

Uses
  BADI.Base.Module,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to hold the information required by the system for each registered module
      parser. **)
  TModuleInfo = Class
  Strict Private
    FExtensions: String;
    FCls:        TBaseLanguageModuleClass;
    FCanDoc:     Boolean;
    FBlockCmt:   TCommentType;
    FLineCmt:    TCommentType;
    FInSituCmt:  TCommentType;
  Strict Protected
  Public
    Constructor Create(Const Cls: TBaseLanguageModuleClass; Const strExtensions: String;
      Const boolCanDoc: Boolean; Const iBlockCmt, iLineCmt, iInSituCmt: TCommentType);
    Function  CanProcessExt(Const strExt : String) : Boolean;
    (**
      This property returns the extension associated with the registration.
      @precon  None.
      @postcon Returns the extension associated with the registration.
      @return  a String
    **)
    Property Extensions: String Read FExtensions Write FExtensions;
    (**
      This property returns the class reference for this registration.
      @precon  None.
      @postcon Returns the class reference for this registration.
      @return  a TBaseLanguageModuleClass
    **)
    Property Cls: TBaseLanguageModuleClass Read FCls;
    (**
      This property returns whether the registration supports documentation.
      @precon  None.
      @postcon Returns whether the registration supports documentation.
      @return  a Boolean
    **)
    Property CanDoc: Boolean Read FCanDoc;
    (**
      This property returns the comment type for blocks.
      @precon  None.
      @postcon Returns the comment type for blocks.
      @return  a TCommentType
    **)
    Property BlockCmt: TCommentType Read FBlockCmt;
    (**
      This property returns the comment type for lines.
      @precon  None.
      @postcon Returns the comment type for lines.
      @return  a TCommentType
    **)
    Property LineCmt: TCommentType Read FLineCmt;
    (**
      This property returns the comment type for In Situ.
      @precon  None.
      @postcon Returns the comment type for In Situ.
      @return  a TCommentType
    **)
    Property InSituCmt: TCommentType Read FInSituCmt;
  End;

Implementation

Uses
  SysUtils,
  BADI.Functions;

(**

  This method returns true if the given extension (with period separator) is handled by this module.

  @precon  None.
  @postcon Returns true if the given extension (with period separator) is handled by this module.

  @param   strExt as a String as a constant
  @return  a Boolean

**)
Function TModuleInfo.CanProcessExt(Const strExt: String): Boolean;

Var
  astrExts : TArray<String>;
  iExt: Integer;
  {$IFNDEF DXE30}
  i : Integer;
  {$ENDIF}

Begin
  Result := False;
  {$IFDEF DXE30}
  astrExts := FExtensions.Split([';']);
  {$ELSE}
  SetLength(astrExts, CharCount(';', FExtensions) + 1);
  For i := Low(astrExts) To High(astrExts) Do
    astrExts[i] := GetField(FExtensions, ';', Succ(i));
  {$ENDIF}
  For iExt := Low(astrExts) To High(astrExts) Do
    If CompareText(strExt, astrExts[iExt]) = 0 Then
      Begin
        Result := True;
        Break;
      End;
End;

(**

  A constructor for the TModuleInfo class.

  @precon  None.
  @postcon Initialises the class with information.

  @param   Cls           as a TBaseLanguageModuleClass as a constant
  @param   strExtensions as a String as a constant
  @param   boolCanDoc    as a Boolean as a constant
  @param   iBlockCmt     as a TCommentType as a constant
  @param   iLineCmt      as a TCommentType as a constant
  @param   iInSituCmt    as a TCommentType as a constant

**)
Constructor TModuleInfo.Create(Const Cls: TBaseLanguageModuleClass; Const strExtensions: String;
  Const boolCanDoc: Boolean; Const iBlockCmt, iLineCmt, iInSituCmt: TCommentType);

Begin
  FExtensions := strExtensions;
  FCls := Cls;
  FCanDoc := boolCanDoc;
  FBlockCmt := iBlockCmt;
  FLineCmt := iLineCmt;
  FInSituCmt := iInSituCmt;
End;

End.

(**
  
  This module contains a class which implements the IBADIModuleStats interface for managing the size
  change of a module.

  @Author  David Hoyle
  @Version 2.164
  @Date    09 Jul 2020
  
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
Unit BADI.ModuleStats;

Interface

Uses
  BADI.Interfaces;

Type
  (** A class that implements the IBADIModuleStats interface. **)
  TBADIModuleStats = Class(TInterfacedObject, IBADIModuleStats)
  Strict Private
    FSize      : Int64;
    FSizeDelta : Int64;
    FFileName  : String;
  Strict Protected
    Procedure Reset();
    Function  SizeChange: Int64;
    Procedure Update(Const iSize, iModifiedCount: Int64);
    Procedure Rename(Const strFileName : String);
  Public
    Constructor Create(Const strFileName : String);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils;

(**

  A constructor for the TBADIModuleStats class.

  @precon  None.
  @postcon Initialises the class.

  @param   strFileName as a String as a constant

**)
Constructor TBADIModuleStats.Create(Const strFileName : String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FSize := -1;
  FSizeDelta := 0;
  FFileName := strFileName;
End;

(**

  A destructor for the TBADIModuleStats class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TBADIModuleStats.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method updates the internal filename to that given.

  @precon  None.
  @postcon The internal filenam eis updated to reflect strFileName.

  @param   strFileName as a String as a constant

**)
Procedure TBADIModuleStats.Rename(Const strFileName: String);

Begin
  FFileName := strFileName;
End;

(**

  This method resets the sizwe delta to zero to signify no change.

  @precon  None.
  @postcon The size delta is reset to zero.

**)
Procedure TBADIModuleStats.Reset();

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Reset', tmoTiming);{$ENDIF}
  FSizeDelta := 0;
End;

(**

  This method returns the size delta for the module.

  @precon  None.
  @postcon Returns the size delta for the module.

  @return  an Int64

**)
Function TBADIModuleStats.SizeChange: Int64;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SizeChange', tmoTiming);{$ENDIF}
  Result := FSizeDelta;
  {$IFDEF DEBUG}
  CodeSite.SendFmtMsg(csmNote, 'Filename: %s, Size: %1.0n', [ExtractFileName(FFilename), Int(Result)]);
  {$ENDIF DEBUG}
End;

(**

  This method updates the size of the module along with incrementing the size delta with the change in
  module size.

  @precon  None.
  @postcon The size and size delta are updated.

  @param   iSize          as an Int64 as a constant
  @param   iModifiedCount as an Int64 as a constant

**)
Procedure TBADIModuleStats.Update(Const iSize, iModifiedCount: Int64);

Var
  iDelta: Int64;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Update', tmoTiming);{$ENDIF}
  If FSize > -1 Then
    Begin
      iDelta := Abs(FSize - iSize);
      Inc(FSizeDelta, iDelta);
      If (iDelta = 0) And (iModifiedCount > 0) Then
        Inc(FSizeDelta);
    End;
  {$IFDEF DEBUG}
  CodeSite.SendFmtMsg(csmNote, 'Filename: %s, Old Size: %1.0n, New Size: %1.0n, Delta: %1.0n', [
    ExtractFileName(FFileName), Int(FSize), Int(iSize), Int(FSizeDelta)]);
  {$ENDIF DEBUG}
  FSize := iSize;
End;

End.

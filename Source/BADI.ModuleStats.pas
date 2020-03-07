(**
  
  This module contains a class which implements the IBADIModuleStats interface for managing the size
  change of a module.

  @Author  David Hoyle
  @Version 1.094
  @Date    07 Mar 2020
  
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
    Procedure Update(Const iSize: Int64);
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
End;

(**

  This method updates the size of the module along with incrementing the size delta with the change in
  module size.

  @precon  None.
  @postcon The size and size delta are updated.

  @param   iSize as an Int64 as a constant

**)
Procedure TBADIModuleStats.Update(Const iSize: Int64);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Update', tmoTiming);{$ENDIF}
  If FSize > -1 Then
    Inc(FSizeDelta, Abs(FSize - iSize));
  FSize := iSize;
End;

End.






(**

  This module contains a module notifier to track when a file is saved so that we can update the date
  and version number before the file is saved.

  @Author  David Hoyle
  @Version 6.761
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.

    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.ModuleNotifier;

Interface

Uses
  ToolsAPI,
  System.RegularExpressions,
  BADI.Interfaces;

Type
  (** A class which implements the IOTAModuleNotifier interfaces. **)
  TBADIModuleNotifier = Class(TNotifierObject, IInterface, IOTANotifier, IOTAModuleNotifier80,
    IOTAModuleNotifier90, IOTAModuleNotifier)
  Strict Private
    FFileName             : String;
    FModuleRenameEvent    : TBADIModuleRenameEvent;
    FModuleStatsList      : IBADIModuleStatsList;
    FModuleHeader         : TRegEx;
    FModuleDateTime       : TRegEx;
    FModuleVersion        : TRegEx;
    FLastUpdatedTickCount : Cardinal;
    FModifiedCount        : Integer;
  Strict Protected
    // IOTAModuleNotifier
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
    Function  CheckOverwrite: Boolean;
    Procedure ModuleRenamed(Const NewName: String); Overload;
    // IOTAModuleNotifier80
    Function  AllowSave: Boolean;
    Function  GetOverwriteFileNameCount: Integer;
    Function  GetOverwriteFileName(Index: Integer): String;
    Procedure SetSaveFileName(Const FileName: String);
    // IOTAModuleNotifier90
    Procedure BeforeRename(Const OldFileName, NewFileName: String);
    Procedure AfterRename(Const OldFileName, NewFileName: String);
    // General Methods
    procedure UpdateDateTimeAndVersion;
    Function  ModuleSource : String;
    Procedure OutputMsg(Const strMsg: String; Const iLine, iColumn : Integer);
    Procedure CheckForLastSecondUpdates;
    Procedure UpdateModuleVersion(Const strComment : String; Const Match : TMatch; Var iLine, iColumn : Integer);
    Procedure UpdateModuleDate(Const strComment : String; Const Match : TMatch; Var iLine, iColumn : Integer);
    Procedure OutputSource(Const strText : String; Const iOffset, iLength : Integer);
    Procedure LineAndColumn(Const strText : String; Const iIndex : Integer; Var iLine, iColumn: Integer);
    // Properties
    (**
      This property provides access to the ModuleRenameEvent.
      @precon  None.
      @postcon Returns the ModuleRenameEvent.
      @return  a TBADIModuleRenameEvent
    **)
    Property ModuleRenameEvent : TBADIModuleRenameEvent Read FModuleRenameEvent;
    (**
      This property provides read / write access to the notifier filename.
      @precon  None.
      @postcon Gets and sets the notifier filename.
      @return  a String
    **)
    Property FileName : String Read FFileName Write FFileName;
  Public
    Constructor Create(Const ModuleStatsList : IBADIModuleStatsList; Const strFileName : String;
      Const ModuleRenameEvent: TBADIModuleRenameEvent);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  System.Math,
  WinAPI.Windows,
  BADI.Types,
  BADI.Functions,
  BADI.ToolsAPIUtils,
  BADI.Options;

(**

  This method is called after a file has been renamed.

  @precon  None.
  @postcon Updated the internal filename.

  @nocheck EmptyMethod
  @nohint  OldFileName NewFileName

  @param   OldFileName as a String as a constant
  @param   NewFileName as a String as a constant

**)
Procedure TBADIModuleNotifier.AfterRename(Const OldFileName, NewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterRename', tmoTiming);{$ENDIF}
End;

(**

  This method is called after a file has been saved.

  @precon  None.
  @postcon This method resets the module size counter.

**)
Procedure TBADIModuleNotifier.AfterSave;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterSave', tmoTiming);{$ENDIF}
  FModuleStatsList.ModuleStats[FFileName].Reset;
End;

(**

  This method is called to check whether a file can be saved.

  @precon  None.
  @postcon Returns true to ensure all files are saved.

  @return  a Boolean

**)
Function TBADIModuleNotifier.AllowSave: Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AllowSave', tmoTiming);{$ENDIF}
  Result := True;
End;

(**

  This method is called before a file is renamed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  OldFileName NewFileName

  @param   OldFileName as a String as a constant
  @param   NewFileName as a String as a constant

**)
Procedure TBADIModuleNotifier.BeforeRename(Const OldFileName, NewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeRename', tmoTiming);{$ENDIF}
End;

(**

  This method is called before a file is saved.

  @precon  None.
  @postcon Here are check whether the module date and version should be updated.

**)
Procedure TBADIModuleNotifier.BeforeSave;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeSave', tmoTiming);{$ENDIF}
  CheckForLastSecondUpdates;
  If FModuleStatsList.ModuleStats[FFileName].SizeChange > 0 Then
    UpdateDateTimeAndVersion;
End;

(**

  This method checks to last second changes to the size of the source code.

  @precon  None.
  @postcon The module statistics are updated.

**)
Procedure TBADIModuleNotifier.CheckForLastSecondUpdates;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckForLastSecondUpdates', tmoTiming);{$ENDIF}
  If ((FLastUpdatedTickCount > 0) And (FLastUpdatedTickCount <> GetTickCount)) Or (FModifiedCount > 0) Then
    Begin
      FModuleStatsList.ModuleStats[FFileName].Update(ModuleSource.Length, FModifiedCount);
      FLastUpdatedTickCount := 0;
      FModifiedCount := 0;
    End;
End;

(**

  This method is called to see if an check should be done for overwriting read only files.

  @precon  None.
  @postcon Returns true to ensure this is done.

  @return  a Boolean

**)
Function TBADIModuleNotifier.CheckOverwrite: Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckOverwrite', tmoTiming);{$ENDIF}
  Result := True;
End;

(**

  A constructor for the TBADIMOduleNotifier class.

  @precon  None.
  @postcon Stores the module filename and the module rename event.

  @param   ModuleStatsList   as an IBADIModuleStatsList as a constant
  @param   strFileName       as a String as a constant
  @param   ModuleRenameEvent as a TBADIModuleRenameEvent as a constant

**)
Constructor TBADIModuleNotifier.Create(Const ModuleStatsList : IBADIModuleStatsList;
  Const strFileName : String; Const ModuleRenameEvent: TBADIModuleRenameEvent);

Const strModuleHeaderRegEx = '^\s*[(/]\*\*.*?\*\*[)/]';
  strDateRegEx = '\@Date\s+(\w+[\s\\\/\-]+\w+[\s\\\/\-]+\w+)';
  strVersionRegEx = '\@version\s+(\d+.\d+)';

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FModuleStatsList := ModuleStatsList;
  FFileName := strFileName;
  FModuleRenameEvent := ModuleRenameEvent;
  FModuleHeader := TRegEx.Create(strModuleHeaderRegEx, [roIgnoreCase, roSingleLine, roCompiled]);
  FModuleDateTime := TRegEx.Create(strDateRegEx, [roIgnoreCase, roSingleLine, roCompiled]);
  FModuleVersion := TRegEx.Create(strVersionRegEx, [roIgnoreCase, roSingleLine, roCompiled]);
  FModuleStatsList.ModuleStats[FFileName].Update(ModuleSource.Length, 0);
  FLastUpdatedTickCount := 0;
  FModifiedCount := 0;
End;

(**

  A destructor for the TBADIModuleNotifier class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TBADIModuleNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FModuleStatsList.Remove(FFileName);
  Inherited;
End;

(**

  This method is called when the notifier is destroyed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TBADIModuleNotifier.Destroyed;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroyed', tmoTiming);{$ENDIF}
End;

(**

  This is a getter method for the Overwrite FileName property.

  @precon  None.
  @postcon Not Used.

  @nocheck MissingCONSTInParam
  @nohint  Index

  @param   Index as an Integer
  @return  a String

**)
Function TBADIModuleNotifier.GetOverwriteFileName(Index: Integer): String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetOverwriteFileName', tmoTiming);{$ENDIF}
  Result := '';
End;

(**

  This is a getter method for the Overwrite Filename Count property.

  @precon  None.
  @postcon Not Used.

  @return  an Integer

**)
Function TBADIModuleNotifier.GetOverwriteFileNameCount: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetOverwriteFileNameCount', tmoTiming);{$ENDIF}
  Result := 0;
End;

(**

  This method increments the determines the line and column of the index position into the given text.

  @precon  Line and Column are the position of the start of the given text.
  @postcon The Line and Column of the index into the string are returned through the var parameters.

  @param   strText as a String as a constant
  @param   iIndex  as an Integer as a constant
  @param   iLine   as an Integer as a reference
  @param   iColumn as an Integer as a reference

**)
Procedure TBADIModuleNotifier.LineAndColumn(Const strText : String; Const iIndex : Integer; Var iLine,
  iColumn: Integer);

Var
  i : Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LineAndColumn', tmoTiming);{$ENDIF}
  For i := 1 To Min(strText.Length, iIndex - 1) Do
    Case strText[i] Of
      #10:
        Begin
          Inc(iLine);
          iColumn := 1;
        End;
    Else
      Inc(iColumn);
    End;
End;

(**

  This method is called when a module is modified.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod

**)
Procedure TBADIModuleNotifier.Modified;

Const
  iUpdateInterval : Cardinal = 500;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Modified', tmoTiming);{$ENDIF}
  Inc(FModifiedCount);
  If (FLastUpdatedTickCount > 0) And (GetTickCount - FLastUpdatedTickCount >= iUpdateInterval) Then
    CheckForLastSecondUpdates
  Else
    FLastUpdatedTickCount := GetTickCount;
End;

(**

  This method is called when a module is renamed.

  @precon  None.
  @postcon Calls the Module Rename Event.

  @param   NewName as a String as a constant

**)
Procedure TBADIModuleNotifier.ModuleRenamed(Const NewName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRenamed', tmoTiming);{$ENDIF}
  FFileName := NewName;
  FModuleStatsList.ModuleStats[NewName].Rename(NewName);
End;

(**

  This method retrieves the source code for the modules filename.

  @precon  None.
  @postcon The modules source code is returned.

  @return  a String

**)
Function TBADIModuleNotifier.ModuleSource: String;

Var
  MS : IOTAModuleServices;
  M: IOTAModule;
  SE: IOTASourceEditor;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleSource', tmoTiming);{$ENDIF}
  Result := '';
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      M := MS.FindModule(FFileName);
      If Assigned(M) Then
        Begin
          SE := TBADIToolsAPIFunctions.SourceEditor(M);
          If Assigned(SE) Then
            Result := TBADIToolsAPIFunctions.EditorAsString(SE);
        End;
    End;
End;

(**

  This method outputs a message to the message window in the IDE.

  @precon  None.
  @postcon A message is output to the IDEs message window.

  @param   strMsg  as a String as a constant
  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Procedure TBADIModuleNotifier.OutputMsg(Const strMsg: String; Const iLine, iColumn : Integer);

Const
  strBADI = 'BADI';

Var
  MS : IOTAMessageServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OutputMsg', tmoTiming);{$ENDIF}
  {$IFDEF CODESITE}
  CodeSite.Send(csmNote, 'TBADIModuleNotifier.OutputMsg', strMsg);
  {$ENDIF CODESITE}
  If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    TBADIToolsAPIFunctions.OutputMessage(
      FFileName,
      strMsg,
      strBADI,
      iLine,
      iColumn
    );
End;

(**

  This method outputs the given text at the given offset into the module after deleting the text
  at the offset with the given length.

  @precon  None.
  @postcon The module version number is updated.

  @param   strText as a String as a constant
  @param   iOffset as an Integer as a constant
  @param   iLength as an Integer as a constant

**)
Procedure TBADIModuleNotifier.OutputSource(Const strText: String; Const iOffset, iLength: Integer);

Var
  MS: IOTAModuleServices;
  M: IOTAModule;
  SE: IOTASourceEditor;
  Writer: IOTAEditWriter;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OutputSource', tmoTiming);{$ENDIF}
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      M := MS.FindModule(FFileName);
      If Assigned(M) Then
        Begin
          SE := TBADIToolsAPIFunctions.SourceEditor(M);
          If Assigned(SE) Then
            Begin
              Writer := SE.CreateUndoableWriter;
              Writer.CopyTo(iOffset);
              Writer.DeleteTo(iOffset + iLength); //: @bug IDE AVs here IF editor is closed while modified
              Writer.Insert(PAnsiChar(UTF8Encode(strText)));
            End;
        End;
    End;
End;

(**

  This is a setter method for the Save FileName property.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  FileName

  @param   FileName as a String as a constant

**)
Procedure TBADIModuleNotifier.SetSaveFileName(Const FileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SetSaveFileName', tmoTiming);{$ENDIF}
End;

(**

  This method attempts to update the module version number and date time in the modules header comment.

  @precon  None.
  @postcon The modules header comment (if found) is updated with the latest date time and version number.

**)
Procedure TBADIModuleNotifier.UpdateDateTimeAndVersion;

ResourceString
  strNoModuleComment = 'This module does not have a header comment!';

Var
  strSource: String;
  MHMC: TMatchCollection;
  strHeaderComment: String;
  MM: TMatch;
  iLine, iColumn : Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateDateTimeAndVersion', tmoTiming);{$ENDIF}
  iLine := 1;
  iColumn := 1;
  If doAutoUpdateModuleVersion In TBADIOptions.BADIOptions.Options Then
    Begin
      strSource := ModuleSource;
      MHMC := FModuleHeader.Matches(strSource);
      If MHMC.Count > 0 Then
        Begin
          LineAndColumn(strSource, MHMC.Item[0].Groups[0].Index, iLine, iColumn);
          strHeaderComment := MHMC.Item[0].Groups[0].Value;
          MM := FModuleVersion.Match(strHeaderComment);
          UpdateModuleVersion(strHeaderComment, MM, iLine, iColumn);
        End;
    End;
  iLine := 1;
  iColumn := 1;
  If doAutoUpdateModuleVersion In TBADIOptions.BADIOptions.Options Then
    Begin
      strSource := ModuleSource;
      MHMC := FModuleHeader.Matches(strSource);
      If MHMC.Count > 0 Then
        Begin
          LineAndColumn(strSource, MHMC.Item[0].Groups[0].Index, iLine, iColumn);
          strHeaderComment := MHMC.Item[0].Groups[0].Value;
          MM := FModuleDateTime.Match(strHeaderComment);
          UpdateModuleDate(strHeaderComment, MM, iLine, iColumn);
        End Else
          OutputMsg(strNoModuleComment, 1, 1);
    End;
End;

(**

  This method updates the modules date.

  @precon  None.
  @postcon The module date is updated if the existing date is valid.

  @param   strComment as a String as a constant
  @param   Match      as a TMatch as a constant
  @param   iLine      as an Integer as a reference
  @param   iColumn    as an Integer as a reference

**)
Procedure TBADIModuleNotifier.UpdateModuleDate(Const strComment : String; Const Match : TMatch;
  Var iLine, iColumn : Integer);

ResourceString
  strModuleDateUpdated = 'Module date updated from %s to %s.';
  strNotValidDate = '"%s" is not a valid date!';

Var
  strDate: String;
  dtDate: TDateTime;
  strNewDate : String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateModuleDate', tmoTiming);{$ENDIF}
  If Match.Success Then
    Begin
      LineAndColumn(strComment, Match.Groups[1].Index, iLine, iColumn);
      strDate := Match.Groups[1].Value;
      Try
        dtDate := ConvertDate(strDate);
        If Trunc(dtDate) <> Trunc(Now()) Then
          Begin
            strNewDate := FormatDateTime(TBADIOptions.BADIOptions.ModuleDateFmt, Now());
            OutputSource(
              strNewDate,
              Match.Groups[1].Index - 1,
              Match.Groups[1].Length
            );
            OutputMsg(Format(strModuleDateUpdated, [strDate, strNewDate]), iLine, iColumn);
          End;
      Except
        On E : EBADIParserError Do
          OutputMsg(Format(strNotValidDate, [strDate]), iLine, iColumn);
      End;
    End;
End;

(**

  This method updates the modules version number.

  @precon  None.
  @postcon The module version number is updated if the existing version number is valid.

  @param   strComment as a String as a constant
  @param   Match      as a TMatch as a constant
  @param   iLine      as an Integer as a reference
  @param   iColumn    as an Integer as a reference

**)
Procedure TBADIModuleNotifier.UpdateModuleVersion(Const strComment : String; Const Match : TMatch;
  Var iLine, iColumn : Integer);

ResourceString
  strVerIncremented = 'Module version number incremented from %s to %s!';
  strNotValidVerNum = '"%s" is not a valid version number.';

Const
  dblDefaultIncrement = 0.001;

Var
  strVersion: String;
  dblVersion : Double;
  iErrorCode: Integer;
  iCharCount: Int64;
  strNewVersion: String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateModuleVersion', tmoTiming);{$ENDIF}
  If Match.Success Then
    Begin
      LineAndColumn(strComment, Match.Groups[1].Index, iLine, iColumn);
      strVersion := Match.Groups[1].Value;
      Val(strVersion, dblVersion, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          iCharCount := FModuleStatsList.ModuleStats[FFileName].SizeChange;
          dblVersion := dblVersion + Max(
            dblDefaultIncrement,
            Int(iCharCount) * TBADIOPtions.BADIOptions.ModuleVersionIncrement
          );
          strNewVersion := Format('%1.3f', [dblVersion]);
          OutputSource(
            strNewVersion,
            Match.Groups[1].Index - 1,
            Match.Groups[1].Length
          );
          OutputMsg(Format(strVerIncremented, [strVersion, strNewVersion]), iLine, iColumn);
        End Else
          OutputMsg(Format(strNotValidVerNum, [strVersion]), iLine, iColumn);
    End;
End;

End.





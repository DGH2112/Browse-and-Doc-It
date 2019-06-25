(**

  This module contains a base class for all documentation modules.

  @Author  David Hoyle
  @Version 1.0
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
Unit BADI.Base.Documentation;

Interface

Uses
  SysUtils,
  Classes,
  ProgressForm;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A base class for all documentation implementations. **)
  TBaseDocumentation = Class
    FFileNames : TStringList;
    FOutputDirectory : String;
    FProgressForm : TfrmProgress;
  Private
  Protected
    Function GetMainDocument : String; Virtual; Abstract;
  Public
    Constructor Create(Const strOutputDirectory, strTitle : String); Virtual;
    Destructor Destroy; Override;
    Procedure Add(Const strFileName : String);
    Procedure OutputDocumentation; Virtual; Abstract;
    Procedure Initialise(iMax : Integer; Const strTitle, strMsg : String); Virtual;
    Procedure Update(iPosition : Integer; Const strMsg : String); Virtual;
    Procedure Finalise; Virtual;
    (**
      This property returns the initial document which should be displayed by
      the calling application.
      @precon  None.
      @postcon Returns the initial document which should be displayed by
               the calling application.
      @return  a String
    **)
    Property MainDocument : String Read GetMainDocument;
  End;

Implementation

Uses
  BADI.Base.Module {$IFNDEF D0006},
  FileCtrl {$ENDIF},
  BADI.Module.Dispatcher;

(**


  This method simple adds the code module to the list of code modules.

  @precon  None.
  @postcon Simple adds the code module to the list of code modules.


  @param   strFileName as a String as a Constant

**)
Procedure TBaseDocumentation.Add(Const strFileName : String);

Begin
  If TBADIDispatcher.BADIDispatcher.CanDocumentDocument(strFileName) Then
    FFileNames.Add(strFileName);
End;

(**


  This is a constructor for the base documentation class.


  @precon  None.

  @postcon Initialises the class.


  @param   strOutputDirectory as a String as a Constant
  @param   strTitle           as a String as a Constant

**)
Constructor TBaseDocumentation.Create(Const strOutputDirectory, strTitle : String);

ResourceString
  strOutputDirectoryIsNull = 'The output directory is NULL!';

Begin
  FFileNames := TStringList.Create;
  FFileNames.Sorted := True;
  FProgressForm := TfrmProgress.Create(Nil);
  FOutputDirectory := strOutputDirectory;
  If Length(FOutputDirectory) = 0 Then
    Raise Exception.Create(strOutputDirectoryIsNull);
  If FOutputDirectory[Length(FOutputDirectory)] <> '\' Then
    FOutputDirectory := FOutputDirectory + '\';
  FOutputDirectory := FOutputDirectory + 'HTML\';
  ForceDirectories(FOutputDirectory);
End;

(**


  Destructor for the base documentation class.

  @precon  None.
  @postcon Frees the classes internals.


**)
Destructor TBaseDocumentation.Destroy;

Begin
  FProgressForm.Free;
  FFileNames.Free;
  Inherited Destroy;
End;

(**


  This method hides the progress dialogue.

  @precon  None.
  @postcon Hides the progress dialogue.


**)
procedure TBaseDocumentation.Finalise;
begin
  FProgressForm.Hide;
end;

(**


  This method initialises the progress dialogue with a maximum, a title and an
  initial message.

  @precon  None.
  @postcon Initialises the progress dialogue with a maximum, a title and an
           initial message.


  @param   iMax     as an Integer
  @param   strTitle as a String as a Constant
  @param   strMsg   as a String as a Constant

**)
procedure TBaseDocumentation.Initialise(iMax: Integer; Const strTitle, strMsg : String);
begin
  FProgressForm.Init(iMax, strTitle, strMsg);
end;

(**


  This method update the progress dialogue with a position and a message.

  @precon  None.
  @postcon Update the progress dialogue with a position and a message.


  @param   iPosition as an Integer
  @param   strMsg    as a String as a Constant

**)
procedure TBaseDocumentation.Update(iPosition: Integer; Const strMsg : String);
begin
  FProgressForm.UpdateProgress(iPosition, strMsg);
end;

End.
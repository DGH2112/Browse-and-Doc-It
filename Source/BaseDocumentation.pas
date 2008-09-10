(**

  This module contains a base class for all documentation modules.

  @Author  David Hoyle
  @Date    09 Sep 2008
  @Version 1.0

**)
Unit BaseDocumentation;

Interface

Uses
  SysUtils, Classes, ProgressForm;

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
    Constructor Create(strOutputDirectory, strTitle : String); Virtual;
    Destructor Destroy; Override;
    Procedure Add(strFileName : String);
    Procedure OutputDocumentation; Virtual; Abstract;
    Procedure Initialise(iMax : Integer; strTitle, strMsg : String); Virtual;
    Procedure Update(iPosition : Integer; strMsg : String); Virtual;
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
  ModuleDispatcher, BaseLanguageModule;

(**

  This method simple adds the code module to the list of code modules.

  @precon  None.
  @postcon Simple adds the code module to the list of code modules.

  @param   strFileName as a String

**)
Procedure TBaseDocumentation.Add(strFileName : String);
var
  i: Integer;

Begin
  For i := 0 To BrowseAndDocItOptions.ExcludeDocFiles.Count -1 Do
    If Pos(Lowercase(BrowseAndDocItOptions.ExcludeDocFiles[i]),
      Lowercase(strFileName)) > 0 Then
      Exit;
  If CanAddDocument(strFileName) Then
    FFileNames.Add(strFileName);
End;

(**

  This is a constructor for the base documentation class. 

  @precon  None. 
  @postcon Initialises the class. 

  @param   strOutputDirectory as a String
  @param   strTitle           as a String

**)
Constructor TBaseDocumentation.Create(strOutputDirectory, strTitle : String);

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
  @param   strTitle as a String
  @param   strMsg   as a String

**)
procedure TBaseDocumentation.Initialise(iMax: Integer; strTitle, strMsg : String);
begin
  FProgressForm.Init(iMax, strTitle, strMsg);
end;

(**

  This method update the progress dialogue with a position and a message.

  @precon  None.
  @postcon Update the progress dialogue with a position and a message.

  @param   iPosition as an Integer
  @param   strMsg    as a String

**)
procedure TBaseDocumentation.Update(iPosition: Integer; strMsg : String);
begin
  FProgressForm.UpdateProgress(iPosition, strMsg);
end;

End.
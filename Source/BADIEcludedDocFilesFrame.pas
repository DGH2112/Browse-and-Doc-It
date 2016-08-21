(**

  This module contains a class which represents a frame interface for excluded document
  files.

  @Version 1.0
  @Author  David Hoyle
  @Date    21 Aug 2016

**)
Unit BADIEcludedDocFilesFrame;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

Type
  (** A class to represent the frame interface. **)
  TfmBADIExcludedDocFilesFrame = Class(TFrame)
    mmoExcludeDocFiles: TMemo;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  BaseLanguageModule;

{ TfmBADIExcludedDocFilesFrame }

(**

  This method loads the frame with the excluded document files from the options.

  @precon  None.
  @postcon The excluded document files list is loaded from the options.

**)
Procedure TfmBADIExcludedDocFilesFrame.LoadSettings;

Begin
  mmoExcludeDocFiles.Text := BrowseAndDocItOptions.ExcludeDocFiles.Text;
End;

(**

  This method saves the frame with the excluded document files to the options.

  @precon  None.
  @postcon The excluded document files list is saved to the options.

**)
Procedure TfmBADIExcludedDocFilesFrame.SaveSettings;

Begin
  BrowseAndDocItOptions.ExcludeDocFiles.Text := mmoExcludeDocFiles.Text;
End;

End.

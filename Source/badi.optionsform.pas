(**

  This module provides an enumerate set for the visible display options and
  a dialogue for setting those options.

  @Date    01 Apr 2017
  @Version 1.0
  @Author  David Hoyle

**)
Unit BADI.OptionsForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ComCtrls,
  ExtCtrls,
  CheckLst,
  ImgList,
  {$WARN UNIT_PLATFORM OFF} // For the FileCtrl unit
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  BADI.Base.Module,
  BADI.GeneralOptionsFrame,
  BADI.SpecialTagsFrame,
  BADI.ModuleExlporerOpsFrame,
  BADI.CodeBrowsingFrame,
  BADI.ExcludedDocFilesFrame,
  BADI.MethodDescriptionsFrame,
  ImageList;


Type
  (** An enumerate to define the visisble tabs in the dialogue. **)
  TVisibleTab = (vtGeneralOptions, vtSpecialTags, vtModuleExplorer,
    vtCodeBrowsing, vtExcludeDocFiles, vtMethodDescriptions);
  (** A set of visible tabs. **)
  TVisibleTabs = Set Of TVisibleTab;

  (** This class represents an options dialogue where the user can change the
      display options of the application. **)
  TfrmOptions = Class(TForm)
    bbtnOK: TBitBtn;
    bbtnCancel: TBitBtn;
    OptionTab: TPageControl;
    tabGeneralOptions: TTabSheet;
    tabSpecialTags: TTabSheet;
    tabModuleExplorer: TTabSheet;
    tabCodeBrowsing: TTabSheet;
    tabExcludeDocFiles: TTabSheet;
    tabMethodDescriptions: TTabSheet;
    btnCheckForUpdates: TBitBtn;
    Procedure btnCheckForUpdatesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    { Private declarations }
  Strict Private
    FBADIGeneralOptionsFrame : TfmBADIGeneralOptions;
    FBADISpecialTagsFrame : TfmBADISpecialTagsFrame;
    FBADIModuleExplorerFrame : TfmBADIModuleExplorerFrame;
    FBADICodeBrowsingFrame : TfmBADICodeBrowsingFrame;
    FBADIExcludedDocFilesFrame : TfmBADIExcludedDocFilesFrame;
    FBADIMethodDescriptionsFrame : TfmBADIMethodDescriptionsFrame;
  Public
    { Public declarations }
    Class Function Execute(VisibleTabs: TVisibleTabs): Boolean;
  End;

Implementation

Uses
  CheckForUpdatesOptionsForm,
  BADI.Options;

{$R *.DFM}


(**

  This method creates an instance of the options dialogue and sets all the
  controls based on the passed Options parameter. If OK is selected then the
  Options parameter is updated to suit the new options.

  @precon  iInt is the timer interval to be represented in the dialogue,
           DocHelpFile is the directory of the modules help file.
  @postcon Returns true if the OK button on the dialogue was pressed.

  @param   VisibleTabs as a TVisibleTabs
  @return  a Boolean

**)
Class Function TfrmOptions.Execute(VisibleTabs: TVisibleTabs): Boolean;

Var
  F: TfrmOptions;

Begin
  Result := False;
  F := TfrmOptions.Create(Application.MainForm);
  Try
    F.OptionTab.ActivePage := F.tabGeneralOptions;
    F.tabGeneralOptions.TabVisible := vtGeneralOptions In VisibleTabs;
    F.tabSpecialTags.TabVisible := vtSpecialTags In VisibleTabs;
    F.tabModuleExplorer.TabVisible := vtModuleExplorer In VisibleTabs;
    F.tabCodeBrowsing.TabVisible := vtCodeBrowsing In VisibleTabs;
    F.tabExcludeDocFiles.TabVisible := vtExcludeDocFiles In VisibleTabs;
    F.tabMethodDescriptions.TabVisible := vtMethodDescriptions In VisibleTabs;
    If F.ShowModal = mrOK Then
      Begin
        Result := True;
        F.FBADIGeneralOptionsFrame.SaveSettings;
        F.FBADISpecialTagsFrame.SaveSettings;
        F.FBADIModuleExplorerFrame.SaveSettings;
        F.FBADICodeBrowsingFrame.SaveSettings;
        F.FBADIExcludedDocFilesFrame.SaveSettings;
        F.FBADIMethodDescriptionsFrame.SaveSettings;
        TBADIOptions.BADIOptions.SaveSettings;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmOptions class.

  @precon  None.
  @postcon Creates the frames for insertion into the page control tabs.

  @param   Sender as a TObject

**)
procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  FBADIGeneralOptionsFrame := TfmBADIGeneralOptions.Create(Self);
  FBADIGeneralOptionsFrame.Parent := tabGeneralOptions;
  FBADIGeneralOptionsFrame.Align := alClient;
  FBADIGeneralOptionsFrame.LoadSettings;
  FBADISpecialTagsFrame := TfmBADISpecialTagsFrame.Create(Self);
  FBADISpecialTagsFrame.Parent := tabSpecialTags;
  FBADISpecialTagsFrame.Align := alClient;
  FBADISpecialTagsFrame.LoadSettings;
  FBADIModuleExplorerFrame := TfmBADIModuleExplorerFrame.Create(Self);
  FBADIModuleExplorerFrame.Parent := tabModuleExplorer;
  FBADIModuleExplorerFrame.Align := alClient;
  FBADIModuleExplorerFrame.LoadSettings;
  FBADICodeBrowsingFrame := TfmBADICodeBrowsingFrame.Create(Self);
  FBADICodeBrowsingFrame.Parent := tabCodeBrowsing;
  FBADICodeBrowsingFrame.Align := alClient;
  FBADICodeBrowsingFrame.LoadSettings;
  FBADIExcludedDocFilesFrame := TfmBADIExcludedDocFilesFrame.Create(Self);
  FBADIExcludedDocFilesFrame.Parent := tabExcludeDocFiles;
  FBADIExcludedDocFilesFrame.Align := alClient;
  FBADIExcludedDocFilesFrame.LoadSettings;
  FBADIMethodDescriptionsFrame := TfmBADIMethodDescriptionsFrame.Create(Self);
  FBADIMethodDescriptionsFrame.Parent := tabMethodDescriptions;
  FBADIMethodDescriptionsFrame.Align := alClient;
  FBADIMethodDescriptionsFrame.LoadSettings;
end;

(**

  This is an on click event handler for the CheckforUpdates button.

  @precon  None.
  @postcon Displays the dialogue for configuring Check for Updates.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnCheckForUpdatesClick(Sender: TObject);
Begin
  TfrmCheckForUpdatesOptions.Execute(TBADIOptions.BADIOptions.INIFileName);
End;

End.

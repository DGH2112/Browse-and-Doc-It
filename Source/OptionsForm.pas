(**

  This module provides an enumerate set for the visible display options and
  a dialogue for setting those options.

  @Date    21 Aug 2016
  @Version 1.0
  @Author  David Hoyle

**)
Unit OptionsForm;

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
  BaseLanguageModule,
  BADIGeneralOptionsFrame,
  ImageList,
  BADISpecialTagsFrame, BADIModuleExlporerOpsFrame, BADICodeBrowsingFrame,
  BADIEcludedDocFilesFrame, BADIMethodDescriptionsFrame;


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
    BADIGeneralOptionsFrame: TfmBADIGeneralOptions;
    BADISpecialTagsFrame: TfmBADISpecialTagsFrame;
    BADIModuleExplorerFrame: TfmBADIModuleExplorerFrame;
    BADICodeBrowsingFrame: TfmBADICodeBrowsingFrame;
    BADIExcludedDocFilesFrame: TfmBADIExcludedDocFilesFrame;
    BADIMethodDescriptionsFrame: TfmBADIMethodDescriptionsFrame;
    Procedure btnCheckForUpdatesClick(Sender: TObject);
    { Private declarations }
  Private
  Public
    { Public declarations }
    Class Function Execute(VisibleTabs: TVisibleTabs): Boolean;
  End;

Implementation

Uses
  CheckForUpdatesOptionsForm;

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

Begin
  Result := False;
  With TfrmOptions.Create(Application.MainForm) Do
    Try
      BADIGeneralOptionsFrame.LoadSettings;
      BADISpecialTagsFrame.LoadSettings;
      BADIModuleExplorerFrame.LoadSettings;
      BADICodeBrowsingFrame.LoadSettings;
      BADIExcludedDocFilesFrame.LoadSettings;
      BADIMethodDescriptionsFrame.LoadSettings;
      OptionTab.ActivePage := tabGeneralOptions;
      tabGeneralOptions.TabVisible := vtGeneralOptions In VisibleTabs;
      tabSpecialTags.TabVisible := vtSpecialTags In VisibleTabs;
      tabModuleExplorer.TabVisible := vtModuleExplorer In VisibleTabs;
      tabCodeBrowsing.TabVisible := vtCodeBrowsing In VisibleTabs;
      tabExcludeDocFiles.TabVisible := vtExcludeDocFiles In VisibleTabs;
      tabMethodDescriptions.TabVisible := vtMethodDescriptions In VisibleTabs;
      If ShowModal = mrOK Then
        Begin
          Result := True;
          BADIGeneralOptionsFrame.SaveSettings;
          BADISpecialTagsFrame.SaveSettings;
          BADIModuleExplorerFrame.SaveSettings;
          BADICodeBrowsingFrame.SaveSettings;
          BADIExcludedDocFilesFrame.SaveSettings;
          BADIMethodDescriptionsFrame.SaveSettings;
          BrowseAndDocItOptions.SaveSettings;
        End;
    Finally
      Free;
    End;
End;

(**

  This is an on click event handler for the CheckforUpdates button.

  @precon  None.
  @postcon Displays the dialogue for configuring Check for Updates.

  @param   Sender as a TObject

**)
Procedure TfrmOptions.btnCheckForUpdatesClick(Sender: TObject);
Begin
  TfrmCheckForUpdatesOptions.Execute(BrowseAndDocItOptions.INIFileName);
End;

End.

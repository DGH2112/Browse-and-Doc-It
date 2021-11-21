(**

  This module provides an enumerate set for the visible display options and
  a dialogue for setting those options.

  @Author  David Hoyle
  @Version 1.016
  @Date    21 Nov 2021

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
  {$IFDEF RS100}
  ImageList,
  {$ENDIF RS100}
  {$WARN UNIT_PLATFORM OFF} // For the FileCtrl unit
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  BADI.Base.Module,
  BADI.GeneralOptionsFrame,
  BADI.SpecialTagsFrame,
  BADI.ModuleExplorerOpsFrame,
  BADI.CodeBrowsingFrame,
  BADI.ExcludedDocFilesFrame,
  BADI.MethodDescriptionsFrame,
  BADI.MenuShortcutsFrame,
  BADI.ModuleExtensionsFrame;

Type
  (** An enumerate to define the visible tabs in the dialogue. **)
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
    tabMenuShortcuts: TTabSheet;
    tabModuleExtensions: TTabSheet;
    procedure FormCreate(Sender: TObject);
    { Private declarations }
  Strict Private
    FBADIGeneralOptionsFrame : TfmBADIGeneralOptions;
    FBADISpecialTagsFrame : TfmBADISpecialTagsFrame;
    FBADIModuleExplorerFrame : TfmBADIModuleExplorerFrame;
    FBADICodeBrowsingFrame : TfmBADICodeBrowsingFrame;
    FBADIExcludedDocFilesFrame : TfmBADIExcludedDocFilesFrame;
    FBADIMethodDescriptionsFrame : TfmBADIMethodDescriptionsFrame;
    FBADIMenuShortcutsFrame : TfmBADIMenuShortcuts;
    FBADIModuleExtensionsFrame : TfmBADIModuleExtensionsFrame;
  Public
    { Public declarations }
    Class Function Execute(Const VisibleTabs: TVisibleTabs): Boolean;
  End;

Implementation

Uses
  BADI.Options;

{$R *.DFM}


(**

  This method creates an instance of the options dialogue and sets all the controls based on the passed 
  Options parameter. If OK is selected then the Options parameter is updated to suit the new options.

  @precon  None.
  @postcon Returns true if the OK button on the dialogue was pressed.

  @param   VisibleTabs as a TVisibleTabs as a constant
  @return  a Boolean

**)
Class Function TfrmOptions.Execute(Const VisibleTabs: TVisibleTabs): Boolean;

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
    F.tabMenuShortcuts.TabVisible := vtMethodDescriptions In VisibleTabs;
    F.tabModuleExtensions.TabVisible := vtMethodDescriptions In VisibleTabs;
    If F.ShowModal = mrOK Then
      Begin
        Result := True;
        F.FBADIGeneralOptionsFrame.SaveSettings;
        F.FBADISpecialTagsFrame.SaveSettings;
        F.FBADIModuleExplorerFrame.SaveSettings;
        F.FBADICodeBrowsingFrame.SaveSettings;
        F.FBADIExcludedDocFilesFrame.SaveSettings;
        F.FBADIMethodDescriptionsFrame.SaveSettings;
        F.FBADIMenuShortcutsFrame.SaveSettings;
        F.FBADIModuleExtensionsFrame.SaveSettings;
        TBADIOptions.BADIOptions.SaveSettings;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This is an On Form Create Event Handler for the TfrmOptions class.

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
  FBADIMenuShortcutsFrame := TfmBADIMenuShortcuts.Create(Self);
  FBADIMenuShortcutsFrame.Parent := tabMenuShortcuts;
  FBADIMenuShortcutsFrame.Align := alClient;
  FBADIMenuShortcutsFrame.LoadSettings;
  FBADIModuleExtensionsFrame := TfmBADIModuleExtensionsFrame.Create(Self);
  FBADIModuleExtensionsFrame.Parent := tabModuleExtensions;
  FBADIModuleExtensionsFrame.Align := alClient;
  FBADIModuleExtensionsFrame.LoadSettings;
end;

End.

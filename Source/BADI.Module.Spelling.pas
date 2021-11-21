(**
  
  This module contains Open Tools API code to create an editor view to display the BADI Spelling
  (Spelling).

  @Author  David Hoyle
  @Version 8.906
  @Date    06 Sep 2020

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
Unit BADI.Module.Spelling;

Interface

Uses
  ToolsAPI,
  DesignIntf,
  Classes, 
  Forms,
  ComCtrls,
  Themes,
  Windows,
  BADI.Base.Module,
  BADI.Types,
  BADI.FileInfo.Manager,
  BADI.Frame.Manager;
   
{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to implement an editor view for displaying module spelling. **)
  TBADIModuleSpellingEditorView = Class(TInterfacedObject, IInterface, INTACustomEditorView,
    INTACustomEditorView150, INTACustomEditorViewStatusPanel)
  Strict Private
    Type
      (** An enumerate to define the status panels to be shown with the spelling. **)
      TBADISpellingStatusPanel = (
        sspModules,
        sspComment,
        sspTags,
        sspConstants,
        sspResourceStrings,
        sspLiterals
      );
    Const
      (** A set to contain the spelling options. **)
      setSpellingOptions = [doShowSpelling..doSpellCheckDFMLiterals];
    Class Var
      (** A single class var reference to the editor view. **)
      FEditorViewRef : INTACustomEditorView;
  Strict Private
    FFrameManager     : TBADIFrameManager;
    FFileInfoMgr      : TBADIFileInfoManager;
    FImageIndex       : Integer;
    FViewIdent        : String;
    FModulePanels     : Array[TBADISpellingStatusPanel] Of TStatusPanel;
    FCount            : Integer;
    FSourceStrings    : TStringList;
    FSource           : String;
    FModified         : Boolean;
    FFileDate         : TDateTime;
    FLastDocOptions   : TDocOptions;
    FDocExclusions    : TStringList;
  Strict Protected
    // INTACustomEditorView
    Function  CloneEditorView: INTACustomEditorView;
    Procedure CloseAllCalled(Var ShouldClose: Boolean);
    Procedure DeselectView;
    Function  EditAction(Action: TEditAction): Boolean;
    Procedure FrameCreated(AFrame: TCustomFrame);
    Function  GetCanCloneView: Boolean;
    Function  GetCaption: String;
    Function  GetEditState: TEditState;
    Function  GetEditorWindowCaption: String;
    Function  GetFrameClass: TCustomFrameClass;
    Function  GetViewIdentifier: String;
    Procedure SelectView;
    // INTACustomEditorView150
    Procedure Close(Var Allowed: Boolean);
    Function  GetImageIndex: Integer;
    Function  GetTabHintText: String;
    // INTACustomEditorViewStatusPanel
    Procedure ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);
    Procedure DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);
    Function  GetStatusPanelCount: Integer;
    // General Methods
    Procedure ParseAndRender(Const strFileName : String);
    Procedure UpdateStatusPanels;
    Procedure ExtractSourceFromModule(Const Module : IOTAModule; Const strFileName : String);
    Procedure ExtractSourceFromFile(Const strFileName: String);
    Procedure LastModifiedDateFromModule(Const Module: IOTAModule);
    Procedure LastModifiedDateFromFile(Const strFileName: String);
    Function  CurrentEditWindow : String;
    Procedure ProcessModule(Const strFileName: String);
    Function  CheckSettings: Boolean;
    Procedure UpdateSettings;
  Public
    Class Function CreateEditorView: INTACustomEditorView;
    Constructor Create(Const strViewIdentifier : String);
    Destructor Destroy; Override;
  End;

  Procedure RegisterSpellingEditorView;
  Procedure UnregisterSpellingEditorView;
  
Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils, 
  Controls, 
  Vcl.Graphics,
  BADI.Interfaces,
  BADI.ProgressForm, 
  BADI.Module.Dispatcher,
  BADI.ToolsAPIUtils,
  BADI.Module.Spelling.EditorView.Frame,
  BADI.Options,
  BADI.Constants;

Const
  (** A unique name for the editor view. **)
  strBADISpellingEditorView = 'BADISpellingEditorView';
  (** A caption for the editor view. **)
  strBADISpelling = 'BADI Spelling';
  (** A default edit window name if one cannot be determined. **)
  strUnknown = 'Unknown';

(**

  This method returns an instance of the custom editor view and passed in the registration of the view
  so that a view can be created when a desktop is loaded.

  @precon  None.
  @postcon An instance of this custom editor view is returned.

  @return  an INTACustomEditorView

**)
Function RecreateBADIStatisticEditorView: INTACustomEditorView;

Begin
  Result := TBADIModuleSpellingEditorView.CreateEditorView;
End;

(**

  This method is called from the main wizard constructor to register this custom editor view.

  @precon  None.
  @postcon The custom editor view is registered with the IDE.

**)
Procedure RegisterSpellingEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.RegisterEditorView(strBADISpellingEditorView, RecreateBADIStatisticEditorView);
End;

(**

  This method is called from the main wizard destructor to un-register this custom editor view.

  @precon  None.
  @postcon The custom editor view is unregistered from the IDE.

**)
Procedure UnregisterSpellingEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorView(strBADISpellingEditorView);
End;

(**

  This method checks whether any of the settings that affect spelling has changed.

  @precon  None.
  @postcon Returns true if any of the options have changed and the list needs to be completely rebuilt.

  @return  a Boolean

**)
Function TBADIModuleSpellingEditorView.CheckSettings: Boolean;

Var
  Exclusions: IBADIExclusions;
  i: Integer;
  sl : TStringList;

Begin
  Result := FLastDocOptions <> TBADIOptions.BADIOptions.Options * setSpellingOptions;
  sl := TStringList.Create;
  Try
    sl.Duplicates := dupIgnore;
    sl.Sorted := True;
    Exclusions := TBADIOptions.BADIOptions.Exclusions;
    For i := 0 To Exclusions.Count - 1 Do
      If etSpelling In Exclusions[i].FExclusions Then
        sl.Add(Exclusions[i].FExclusionPattern);
    Result := Result Or (CompareText(sl.Text, FDocExclusions.Text) <> 0);
  Finally
    sl.Free;
  End;
End;

(**

  This method is called when the IDE wants to clone the view and if the GetCanClose method returns true.

  @precon  None.
  @postcon You should return a cloned instance of your view if requested. I have not been able to get the
           IDE to ever call this method.

  @return  an INTACustomEditorView

**)
Function TBADIModuleSpellingEditorView.CloneEditorView: INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.CloseActiveEditorView;
  Result := RecreateBADIStatisticEditorView;
End;

(**

  This method is called when this view tab in the editor is being requested to close. Return true to
  allow if the close else return false for it to persist.

  @precon  None.
  @postcon I return true here so it can be closed.

  @param   Allowed as a Boolean as a reference

**)
Procedure TBADIModuleSpellingEditorView.Close(Var Allowed: Boolean);

Begin
  Allowed := True;
End;

(**

  This method is called when all the views in the editor are being requested to close. Return true to 
  allow if the close else return false for it to persist.

  @precon  None.
  @postcon I return true here so it can be closed.

  @param   ShouldClose as a Boolean as a reference

**)
Procedure TBADIModuleSpellingEditorView.CloseAllCalled(Var ShouldClose: Boolean);

Begin
  ShouldClose := True;
End;

(**

  This method is called when each editor status panel is created.

  @precon  None.
  @postcon References to the panels are stored for later use and each panel is configured.

  @nocheck MissingCONSTInParam
  @nohint  StatusBar

  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel

**)
Procedure TBADIModuleSpellingEditorView.ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);

Const
  iPanelWidth = 125;

Begin
  FModulePanels[TBADISpellingStatusPanel(Panel.Index)] := Panel;
  FModulePanels[TBADISpellingStatusPanel(Panel.Index)].Alignment := taCenter;
  FModulePanels[TBADISpellingStatusPanel(Panel.Index)].Width := iPanelWidth;
  //: @note Problems with first panel if you do not explicitly set this
  FModulePanels[TBADISpellingStatusPanel(Panel.Index)].Style := psOwnerDraw; // psText; 
End;

(**

  A constructor for the TBADIModuleSpellingEditorView class.

  @precon  None.
  @postcon Adds an image to the editor image list to be displayed against this editor view.

  @param   strViewIdentifier as a String as a constant

**)
Constructor TBADIModuleSpellingEditorView.Create(Const strViewIdentifier : String);

Const
  strBADISpellingImage = 'BADISpellingImage';

Var
  EVS : INTAEditorViewServices;
  ImageList : TImageList;
  BM: TBitmap;
  
Begin
  Inherited Create;
  FFrameManager := TBADIFrameManager.Create;
  FFileInfoMgr := TBADIFileInfoManager.Create;
  FSourceStrings := TStringList.Create;
  FDocExclusions := TStringList.Create;
  FDocExclusions.Duplicates := dupIgnore;
  FDocExclusions.Sorted := True;
  FViewIdent := strViewIdentifier;
  FCount := 0;
  If Supports(BorlandIDEServices, INTAEditorViewServices, EVS) Then
    Begin
      ImageList := TImageList.Create(Nil);
      Try
        BM := TBitMap.Create;
        Try
          BM.LoadFromResourceName(HInstance, strBADISpellingImage);
          ImageList.AddMasked(BM, clLime);
          FImageIndex := EVS.AddImages(ImageList, strBADISpellingEditorView);
        Finally
          BM.Free;
        End;
      Finally
        ImageList.Free;
      End;
    End;
End;

(**

  This is a class method to create a singleton instance of this editor view.

  @precon  None.
  @postcon Create the editor view is it does not already exist else returned the existing instance 
           reference.

  @return  an INTACustomEditorView

**)
Class Function TBADIModuleSpellingEditorView.CreateEditorView : INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    Begin
      If Not Assigned(FEditorViewRef) Then
        FEditorViewRef := TBADIModuleSpellingEditorView.Create('');
      Result := FEditorViewRef;
      EVS.ShowEditorView(Result);
    End;
End;

(**

  This method returns the name of the current top editor window.

  @precon  None.
  @postcon The name for the top editor window is returned.

  @return  a String

**)
Function TBADIModuleSpellingEditorView.CurrentEditWindow: String;

Var
  ES : INTAEditorServices;
  
Begin
  Result := strUnknown;
  If Supports(BorlandIDEServices, INTAEditorServices, ES) Then
    Result := ES.TopEditWindow.Form.Name;
End;

(**

  This method is called when the editor view loses focus.

  @nocheck EmptyMethod
  
  @precon  None.
  @postcon Does nothing.

**)
Procedure TBADIModuleSpellingEditorView.DeselectView;

Begin
  // Does nothing
End;

(**

  A destructor for the TBADIModuleSpellingEditorView class.

  @precon  None.
  @postcon Frees the memory used by the module (if not nil).

**)
Destructor TBADIModuleSpellingEditorView.Destroy;

Begin
  FDocExclusions.Free;
  FSourceStrings.Free;
  FFileInfoMgr.Free;
  FFrameManager.Free;
  Inherited Destroy;
End;

(**

  This method is called for each status panel if it is set to owner draw.

  @precon  None.
  @postcon Draw each panel with a blue number and black bold text.

  @nocheck MissingCONSTInParam
  
  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TBADIModuleSpellingEditorView.DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);

  (**

    This method renders the background of the status bar panel.

    @precon  None.
    @postcon The background of the status panel is rendered.

    @param   strNum        as a String as a constant

  **)
  Procedure DrawBackground(Const strNum : String);

  Var
    iColour : TColor;

  Begin
    Case TBADISpellingStatusPanel(Panel.Index) Of
      sspModules: iColour := iLightYellow;
      sspComment..sspLiterals: iColour := iLightGreen;
    Else
      iColour := StatusBar.Color;
    End;
    If strNum <> '' Then
      Case TBADISpellingStatusPanel(Panel.Index) Of
        sspComment..sspLiterals:
          If StrToInt(strNum) > 0 Then
            iColour := iLightRed;
      End;
    StatusBar.Canvas.Brush.Color := iColour;
    StatusBar.Canvas.FillRect(Rect);
  End;

  (**

    This width of the text in the status panel is calculated.

    @precon  None.
    @postcon The width of the text in the status panel is returned taking into account the font styles.

    @param   strNum   as a String as a constant
    @param   strSpace as a String as a constant
    @param   strText  as a String as a constant
    @return  an Integer

  **)
  Function CalcWidth(Const strNum, strSpace, strText : String) : Integer;

  Begin  
    StatusBar.Canvas.Font.Style := [];
    Result := StatusBar.Canvas.TextWidth(strNum);
    Inc(Result, StatusBar.Canvas.TextWidth(strSpace));
    StatusBar.Canvas.Font.Style := [fsBold];
    Inc(Result, StatusBar.Canvas.TextWidth(strText));
  End;
  
  (**

    This method renders the text on the status panel.

    @precon  None.
    @postcon The text of the status bar is rendered.

    @param   strNum        as a String as a reference
    @param   strSpace      as a String as a reference
    @param   strText       as a String as a reference
    @param   iWidth        as an Integer as a constant

  **)
  Procedure DrawText(Var strNum, strSpace, strText : String; Const iWidth : Integer);

  Const
    iDivisor = 2;
    iTopPadding = 4;

  Var
    R : TRect;
    
  Begin
    R := Rect;
    // Draw Number
    Inc(R.Left, (R.Right - R.Left - iWidth) Div iDivisor);
    Inc(R.Top, iTopPadding);
    StatusBar.Canvas.Font.Assign(StatusBar.Font);
    StatusBar.Canvas.Font.Color := clBlue;
    StatusBar.Canvas.Font.Style := [];
    StatusBar.Canvas.TextRect(R, strNum, [tfLeft,tfBottom]);
    // Draw Space
    Inc(R.Left, StatusBar.Canvas.TextWidth(strNum));
    StatusBar.Canvas.TextRect(R, strSpace, [tfLeft,tfBottom]);
    // Draw Text Label
    StatusBar.Canvas.Font.Color := clWindowText;
    StatusBar.Canvas.Font.Style := [fsBold];
    Inc(R.Left, StatusBar.Canvas.TextWidth(strSpace));
    StatusBar.Canvas.TextRect(R, strText, [tfLeft,tfBottom]);
  End;

Var
  strNum, strSpace, strText : String;
  iPos : Integer;
  
Begin
  // Split text by first space
  iPos := Pos(#32, Panel.Text);
  strNum := Copy(Panel.Text, 1, Pred(iPos));
  strSpace := #32;
  strText := Copy(Panel.Text, Succ(iPos), Length(Panel.Text) - iPos);
  DrawBackground(strNum);
  DrawText(strNum, strSpace, strText, CalcWidth(strNum, strSpace, strText));
End;

(**

  This method is called for the given editor action that you have said is supported by the editor view.

  @precon  None.
  @postcon The treeview text is copied to the clipboard if that action is invoked.

  @nocheck MissingCONSTInParam
  
  @param   Action as a TEditAction
  @return  a Boolean

**)
Function TBADIModuleSpellingEditorView.EditAction(Action: TEditAction): Boolean;

Var
  AFrame: Tframe;

Begin
  Result := False;
  Case Action Of
    eaCopy:
      Begin
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) And (AFrame Is TframeBADIModuleSpellingEditorView) Then
          (AFrame As TframeBADIModuleSpellingEditorView).CopyToClipboard;
        Result := True;
      End;
  End;
End;

(**

  This method extracts the source code, filename and date information from a disk file.

  @precon  ModuleInfo must be a valid instance.
  @postcon The source code, filename and date information is retrieved from the disk file.

  @param   strFileName as a String as a constant

**)
Procedure TBADIModuleSpellingEditorView.ExtractSourceFromFile(Const strFileName: String);

Begin
  FSource := '';
  If FileExists(strFileName) Then
    Begin
      FSourceStrings.LoadFromFile(strFileName);
      FSource := FSourceStrings.Text;
      FSourceStrings.Clear;
    End;
End;

(**

  This method extracts the source code, filename and date information from an in memory module.

  @precon  Module and ModuleInfo must be a valid instance.
  @postcon The source code, filename and date information is retrieved from the in memory module.

  @param   Module      as an IOTAModule as a constant
  @param   strFileName as a String as a constant

**)
Procedure TBADIModuleSpellingEditorView.ExtractSourceFromModule(Const Module : IOTAModule;
  Const strFileName : String);

Const
  strDFMExt = '.dfm';

Var
  strExt: String;
  SE: IOTASourceEditor;

Begin
  strExt := LowerCase(ExtractFileExt(strFileName));
  If strExt = strDFMExt Then
    ExtractSourceFromFile(strFileName)
  Else
    Begin
      SE := TBADIToolsAPIFunctions.SourceEditor(Module);
      FSource := TBADIToolsAPIFunctions.EditorAsString(SE);
    End;
End;

(**

  This method is called when the frame is first created.

  @precon  None.
  @postcon Stores a reference to the frame so that a modules Spelling can be rendered

  @nocheck MissingCONSTInParam
  
  @param   AFrame as a TCustomFrame

**)
Procedure TBADIModuleSpellingEditorView.FrameCreated(AFrame: TCustomFrame);

Const
  strTEditWindow = 'TEditWindow';

Var
  ES : INTAEditorServices;
  C : TWinControl;
  strEditWindowName : String;

Begin
  FFileInfoMgr.Clear;
  If Supports(BorlandIDEServices, INTAEditorServices, ES) Then
    Begin
      strEditWindowName := strUnknown;
      C := AFrame;
      While Assigned(C) Do
        Begin
          If C.ClassName = strTEditWindow Then
            Begin
              strEditWindowName := C.Name;
              Break;
            End;
          C := C.Parent;
        End;
      FFrameManager.Add(strEditWindowName, AFrame As TframeBADIModuleSpellingEditorView);
    End;
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Returns false as this editor view should not be cloned (think singleton view).

  @return  a Boolean

**)
Function TBADIModuleSpellingEditorView.GetCanCloneView: Boolean;

Begin
  Result := False;
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon The method returns the caption  for the editor view. It is also used as the editor sub view
           tab description.

  @return  a String

**)
Function TBADIModuleSpellingEditorView.GetCaption: String;

ResourceString
  strSpelling = 'Spelling';

Const
  iDivisor = 2;

Begin
  Inc(FCount);
  If FCount Mod iDivisor = 0 Then
    Result := strSpelling
  Else
    Result := strBADISpelling;
End;

(**

  This is a getter method for the Editor Window Caption property.

  @precon  None.
  @postcon Returns the text to be displayed in the Editor Window (you can only see this when the editor
           is floating).

  @return  a String

**)
Function TBADIModuleSpellingEditorView.GetEditorWindowCaption: String;

Begin
  Result := strBADISpelling;
End;

(**

  This is a getter method for the Edit State property.

  @precon  None.
  @postcon This method is called to tell the IDE what editor state can be invoked on the data in the
           view (cut, copy, paste, etc).

  @return  a TEditState

**)
Function TBADIModuleSpellingEditorView.GetEditState: TEditState;

Begin
  Result := [esCanCopy];
End;

(**

  This is a getter method for the Frame Class property.

  @precon  None.
  @postcon The method returns the frame class that the IDE should create when creating the editor view.

  @return  a TCustomFrameClass

**)
Function TBADIModuleSpellingEditorView.GetFrameClass: TCustomFrameClass;

Begin
  Result := TframeBADIModuleSpellingEditorView;
End;

(**

  This is a getter method for the Image Index property.

  @precon  None.
  @postcon Returns the image index of the image in the editor image list for this editor view.

  @return  an Integer

**)
Function TBADIModuleSpellingEditorView.GetImageIndex: Integer;

Begin
  Result := FImageIndex;
End;

(**

  This is a getter method for the Status Panel Count property.

  @precon  None.
  @postcon Returns the number of status panels to create for the editor view.

  @return  an Integer

**)
Function TBADIModuleSpellingEditorView.GetStatusPanelCount: Integer;

Begin
  Result := Ord(High(TBADISpellingStatusPanel)) - Ord(Low(TBADISpellingStatusPanel)) + 1;
End;

(**

  This is a getter method for the Tab Hint Text property.

  @precon  None.
  @postcon Returns the text to be displayed when the mouse is hovered over the editor tab.

  @return  a String

**)
Function TBADIModuleSpellingEditorView.GetTabHintText: String;

Begin
  Result := strBADISpelling;
End;

(**

  This is a getter method for the View Identifier property.

  @precon  None.
  @postcon Returns a unique identifier for this view (must be unique within the IDE - think singleton
           instance).

  @return  a String

**)
Function TBADIModuleSpellingEditorView.GetViewIdentifier: String;

Begin
  Result := Format('%s.%s', [strBADISpellingEditorView, FViewIdent]);
End;

(**

  This method retrieves the last modified date of the module from disk.

  @precon  strFileName must be a valid instance.
  @postcon The FFileDate and FModified fields are updated.

  @param   strFileName as a String as a constant

**)
Procedure TBADIModuleSpellingEditorView.LastModifiedDateFromFile(Const strFileName: String);

Begin
  FileAge(strFileName, FFileDate);
  FModified := False;
End;

(**

  This method retrieves the last modified date of the module from the IDE.

  @precon  Module must be a valid instance.
  @postcon The FFileDate and FModified fields are updated.

  @param   Module as an IOTAModule as a constant

**)
Procedure TBADIModuleSpellingEditorView.LastModifiedDateFromModule(Const Module: IOTAModule);

Var
  SE : IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  FModified := SE.Modified;
  If Not FModified Then
    FileAge(Module.FileName, FFileDate)
  Else
    FFileDate := Now();
End;

(**

  This method parses the source code and renders the module in the Spelling frame.

  @precon  None.
  @postcon The source code is parsed and rendered.

  @param   strFileName as a String as a constant

**)
Procedure TBADIModuleSpellingEditorView.ParseAndRender(Const strFileName : String);

Var
  Module : TBaseLanguageModule;
  AFrame: Tframe;

Begin
  FFileInfoMgr.Add(strFileName, FFileDate);
  If (Length(FSource) > 0) And (Length(strFileName) > 0) Then
    Begin
      Module := TBADIDispatcher.BADIDispatcher.Dispatcher(FSource, strFileName, FModified, [moParse]);
      Try
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) And (AFrame Is TframeBADIModuleSpellingEditorView) Then
          (AFrame As TframeBADIModuleSpellingEditorView).RenderModule(
            Module,
            [roAutoExpand, roAutoExpandOnError]
          );
      Finally
        Module.Free;
      End;
    End;
End;

(**

  This method process the module extracting the filename, date time and source code and the pass it for 
  parsing.

  @precon  strFileName must be a valid instance.
  @postcon Process the module extracting the filename, date time and source code and the pass it for 
           parsing.

  @param   strFileName as a String as a constant

**)
Procedure TBADIModuleSpellingEditorView.ProcessModule(Const strFileName: String);

Var
  Module: IOTAModule;

Begin
  FModified := False;
  Module := (BorlandIDEServices As IOTAModuleServices).FindModule(strFileName);
  If Assigned(Module) Then
    LastModifiedDateFromModule(Module)
  Else
    LastModifiedDateFromFile(strFileName);
  If FFileInfoMgr.ShouldUpdate(strFileName, FFileDate) Then
    Begin
      If Assigned(Module) Then
        ExtractSourceFromModule(Module, strFileName)
      Else
        ExtractSourceFromFile(strFileName);
      ParseAndRender(strFileName);
    End;
End;

(**

  This method is called when the editor view is selected either when its created or when it regains
  focus.

  @precon  None.
  @postcon Renders the modules Spelling in the frame.

**)
Procedure TBADIModuleSpellingEditorView.SelectView;

ResourceString
  strParsingProjectModules = 'Parsing project modules';
  strPleaseWait = 'Please wait...';
  strParsing = 'Parsing: %s...';

Const
  setModuleTypesToParse = [omtForm, omtDataModule, omtProjUnit, omtUnit];
  strDFMExt = '.dfm';
  
Var
  P: IOTAProject;
  iModule: Integer;
  frmProgress : TfrmProgress;
  ModuleInfo: IOTAModuleInfo;
  AFrame: Tframe;

Begin
  P := TBADIToolsAPIFunctions.ActiveProject;
  If Assigned(P) Then
    Begin
      If CheckSettings Then
        Begin
          FFileInfoMgr.Clear;
          UpdateSettings;
        End;
      frmProgress := TfrmProgress.Create(Application.MainForm);
      Try
        frmProgress.Init(P.GetModuleCount, strParsingProjectModules, strPleaseWait);
        For iModule := 0 To P.GetModuleCount - 1 Do
          Begin
            ModuleInfo := P.GetModule(iModule);
            If ModuleInfo.ModuleType In setModuleTypesToParse Then
              If ModuleInfo.FileName.Length > 0 Then
                Begin
                  ProcessModule(ModuleInfo.FileName);
                  If ModuleInfo.ModuleType = omtForm Then
                    ProcessModule(ChangeFileExt(ModuleInfo.FileName, strDFMExt));
                  frmProgress.UpdateProgress(Succ(iModule), Format(strParsing,
                    [ExtractFileName(ModuleInfo.FileName)]));
                End
          End;
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) And (AFrame Is TframeBADIModuleSpellingEditorView) Then
          (AFrame As TframeBADIModuleSpellingEditorView).FocusResults;
      Finally
        frmProgress.Free;
      End;
      UpdateStatusPanels;
    End;
End;

(**

  This method updates the internal copy of the settings that are used for rendering the editor view.

  @precon  None.
  @postcon The internal copy of the settings is updated so later renderings can check whether the
           settings have changed.

**)
Procedure TBADIModuleSpellingEditorView.UpdateSettings;

Var
  Exclusions: IBADIExclusions;
  i: Integer;

Begin
  FLastDocOptions := TBADIOptions.BADIOptions.Options * setSpellingOptions;
  // Build a string list of spelling exclusions
  Exclusions := TBADIOptions.BADIOptions.Exclusions;
  For i := 0 To Exclusions.Count - 1 Do
    If etSpelling In Exclusions[i].FExclusions Then
      FDocExclusions.Add(Exclusions[i].FExclusionPattern);
End;

(**

  This method updates the status panels with the information from the frame.

  @precon  None.
  @postcon The status panels are updated.

**)
Procedure TBADIModuleSpellingEditorView.UpdateStatusPanels;

ResourceString
  strModules = '%d Modules';
  strComment = '%d Comments';
  strTag = '%d Tags';
  strResString = '%d Resource String';
  strConstant = '%d Constant';
  strLiteral = '%d Literal';

Var
  AFrame: Tframe;
  F: TframeBADIModuleSpellingEditorView;
  
Begin
  AFrame := FFrameManager.Frame[CurrentEditWindow];
  If Assigned(AFrame) And (AFrame Is TframeBADIModuleSpellingEditorView) Then
    Begin
      F := (AFrame As TframeBADIModuleSpellingEditorView);
      FModulePanels[sspModules].Text :=         Format(strModules,   [F.ModuleCount]);
      FModulePanels[sspComment].Text :=         Format(strComment,   [F.SpellingIssue[sitComment]]);
      FModulePanels[sspTags].Text :=            Format(strTag,       [F.SpellingIssue[sitTag]]);
      FModulePanels[sspConstants].Text :=       Format(strResString, [F.SpellingIssue[sitResourceString]]);
      FModulePanels[sspResourceStrings].Text := Format(strConstant,  [F.SpellingIssue[sitConstant]]);
      FModulePanels[sspLiterals].Text :=        Format(strLiteral,   [F.SpellingIssue[sitLiteral]]);
    End;
End;

End.

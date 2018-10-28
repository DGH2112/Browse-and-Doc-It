(**
  
  This module contains Open Tools API code to create an editor view to display the BADI Statistics
  (Checks).

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Oct 2018
  
**)
Unit BADI.Module.Checks;

Interface

Uses
  ToolsAPI,
  DesignIntf,
  Forms, 
  ComCtrls,
  Windows,
  Classes, 
  BADI.Module.Checks.EditorView.Frame, 
  BADI.Base.Module,
  Generics.Collections,
  Themes, 
  BADI.Types;
   
{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to implement an editor view for displaying module Checks. @noChecks MissingCONSTInParam **)
  TBADIModuleChecksEditorView = Class(TInterfacedObject, INTACustomEditorView, INTACustomEditorView150,
    INTACustomEditorViewStatusPanel)
  Strict Private
    Type
      (** This class managed a list of file name and their last modified date so that the view only
          updated modules that have changed. **)
      TBADIFileInfoManager = Class
      Strict Private
        Type
          (** A record to describe the information stored in the collection. **)
          TBADIModuleUpdateRecord = Record
            FFileName    : String;
            FLastUpdated : TDateTime;
          End;
        Strict Private
          FFileInfo : TList<TBADIModuleUpdateRecord>;
      Strict Protected
        Function  Find(Const strFileName : String) : Integer;
      Public
        Constructor Create;
        Destructor Destroy; Override;
        Procedure Add(Const strFileName : String; Const dtFileDate : TDateTime);
        Function  ShouldUpdate(Const strFileName : String; Const dtDateTime : TDateTime) : Boolean;
        Procedure Clear;
      End;
      (** An enumerate to define the status panels to be shown with the Checks. **)
      TBADICheckStatusPanel = (mspModules, mspMethods, mspUnderLimit, mspOverLimit);
      (** A class to manage the frames against the editor windows. **)
      TBADIFrameManager = Class
      Strict Private
        Type
          (** A record to stored information about each view. **)
          TBADIFrameManagerRecord  = Record
            FEditWindowName : String;
            FFrameReference : TframeBADIModuleChecksEditorView;
          End;
      Strict Private
        FFrames : TList<TBADIFrameManagerRecord>;
      Strict Protected
        Function  GetFrame(Const strEditWindowName : String) : TframeBADIModuleChecksEditorView;
        Function  Find(Const strEditWindowName :String) : Integer;
      Public
        Constructor Create;
        Destructor Destroy; Override;
        Procedure Add(Const strEditWindowName : String; Const AFrame : TframeBADIModuleChecksEditorView);
        (**
          A property to returned the frame associated with the given edit window.
          @precon  None.
          @postcon Returned the frame associated with the given edit window.
          @param   strEditWindowName as a String as a constant
          @return  a TframeBADIModuleChecksEditorView
        **)
        Property Frame[Const strEditWindowName : String] : TframeBADIModuleChecksEditorView Read GetFrame;
      End;
    Class Var
      (** A single class var reference to the editor view. **)
      FEditorViewRef : INTACustomEditorView;
  Strict Private
    FFrameManager     : TBADIFrameManager;
    FFileInfoMgr      : TBADIFileInfoManager;
    FImageIndex       : Integer;
    FViewIdent        : String;
    FModulePanels     : Array[Low(TBADICheckstatusPanel)..High(TBADICheckstatusPanel)] Of TStatusPanel;
    FCount            : Integer;
    FSourceStrings    : TStringList;
    FSource           : String;
    FFileName         : String;
    FModified         : Boolean;
    FFileDate         : TDateTime;
    FLastRenderedList : TBADIModuleChecks;
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
    Procedure ParseAndRender;
    Procedure UpdateStatusPanels;
    Procedure ExtractSourceFromModule(Const Module : IOTAModule);
    Procedure ExtractSourceFromFile;
    Procedure LastModifiedDateFromModule(Const Module: IOTAModule);
    Procedure LastModifiedDateFromFile(Const ModuleInfo: IOTAModuleInfo);
    Function  CurrentEditWindow : String;
    Procedure ProcesModule(Const ModuleInfo : IOTAModuleInfo);
    Function  RenderedList : TBADIModuleChecks;
  Public
    Class Function CreateEditorView: INTACustomEditorView;
    Constructor Create(Const strViewIdentifier : String);
    Destructor Destroy; Override;
  End;

  Procedure RegisterChecksEditorView;
  Procedure UnregisterChecksEditorView;
  
Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils, 
  BADI.Module.Dispatcher, 
  BADI.ToolsAPIUtils,
  Controls, 
  Vcl.Graphics,
  ProgressForm, 
  BADI.Options;

Const
  (** A unique name for the editor view. **)
  strBADIChecksEditorView = 'BADIChecksEditorView';
  (** A caption for the editor view. **)
  strBADIChecks = 'BADI Checks';
  (** A default edit window name if one cannot be determined. **)
  strUnknown = 'Unknown';

(**

  This method returns an instance of the custom editor view and passed in the registration of the view
  so that a view can be created when a desktop is loaded.

  @precon  None.
  @postcon An instance of this custom editoe view is returned.

  @return  an INTACustomEditorView

**)
Function RecreateBADIChecksEditorView: INTACustomEditorView;

Begin
  Result := TBADIModuleChecksEditorView.CreateEditorView;
End;

(**

  This method is called from the main wizard constructor to regsiter this custom editor view.

  @precon  None.
  @postcon The custom editor view is registered with the IDE.

**)
Procedure RegisterChecksEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.RegisterEditorView(strBADIChecksEditorView, RecreateBADIChecksEditorView);
End;

(**

  This method is called from the main wizard destructor to unregsiter this custom editor view.

  @precon  None.
  @postcon The custom editor view is unregistered from the IDE.

**)
Procedure UnregisterChecksEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorView(strBADIChecksEditorView);
End;

{ TBADIFileUpdateManager }

(**

  This method either adds a new filename / date record to the collection if it does not exists else
  updated the date of the existing file record.

  @precon  None.
  @postcon Either a new record is added else and existing one is updated.

  @param   strFileName as a String as a constant
  @param   dtFileDate  as a TDateTime as a constant

**)
Procedure TBADIModuleChecksEditorView.TBADIFileInfoManager.Add(Const strFileName: String;
  Const dtFileDate: TDateTime);

Var
  iIndex : Integer;
  recFileInfo : TBADIModuleUpdateRecord;
  
Begin
  iIndex := Find(strFileName);
  If iIndex < 0 Then
    Begin
      recFileInfo.FFileName := strFileName;
      recFileInfo.FLastUpdated := dtFileDate;
      FFileInfo.Add(recFileInfo);
    End Else
    Begin
      recFileInfo := FFileInfo[iIndex];
      recFileInfo.FLastUpdated := dtFileDate;
      FFileInfo[iIndex] := recFileInfo;
    End;
End;

(**

  This method clears the file collection.

  @precon  None.
  @postcon The file collection is empty.

**)
Procedure TBADIModuleChecksEditorView.TBADIFileInfoManager.Clear;

Begin
  FFileInfo.Clear;
End;

(**

  A constructor for the TBADIFileInfoManager class.

  @precon  None.
  @postcon Creates an empty collection.

**)
Constructor TBADIModuleChecksEditorView.TBADIFileInfoManager.Create;

Begin
  FFileInfo := TList<TBADIModuleUpdateRecord>.Create;
End;

(**

  A destructor for the TBADIFileInfoManager class.

  @precon  None.
  @postcon Fress the memory used by the collection.

**)
Destructor TBADIModuleChecksEditorView.TBADIFileInfoManager.Destroy;

Begin
  FFileInfo.Free;
  Inherited Destroy;
End;

(**

  This method used a sequential search to find an existing record with the given filename and returns
  its index if found else return -1.

  @precon  None.
  @postcon Returns the index of an existing record with the files name else returns -1.

  @param   strFileName as a String as a constant
  @return  an Integer

**)
Function TBADIModuleChecksEditorView.TBADIFileInfoManager.Find(Const strFileName: String): Integer;

Var
  iFile : Integer;
  
Begin
  Result :=  -1;
  For iFile := 0 To FFileInfo.Count - 1 Do
    If CompareText(strFileName, FFileInfo[iFile].FFileName) = 0 Then
      Begin
        Result := iFile;
        Break;
      End;
End;

(**

  This method determines whether a module with a given filename needs to be updated.

  @precon  None.
  @postcon Returns true if the given filename is newer than the one stored against the filename in the
           collection.

  @param   strFileName as a String as a constant
  @param   dtDateTime  as a TDateTime as a constant
  @return  a Boolean

**)
Function TBADIModuleChecksEditorView.TBADIFileInfoManager.ShouldUpdate(Const strFileName: String;
  Const dtDateTime: TDateTime): Boolean;

Var
  iIndex : Integer;
  
Begin
  Result :=  True;
  iIndex := Find(strFileName);
  If iIndex >= 0 Then
    Result := dtDateTime > FFileInfo[iIndex].FLastUpdated;
End;

(**

  This method is called when the IDE wants to clone the view and if the GetCanClose method returns true.

  @precon  None.
  @postcon You should return a cloned instance of your view if requested. I have not been able to get the
           IDE to ever call this method.

  @return  an INTACustomEditorView

**)
Function TBADIModuleChecksEditorView.CloneEditorView: INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.CloseActiveEditorView;
  Result := RecreateBADIChecksEditorView;
End;

{ TBADIModuleChecks.TBADIViewManager }

(**

  This method adds the given edit window name and frame referencce pair to the collection is it does not 
  already exists else it updates the existing records frame reference.

  @precon  AFrame must be a valid instance.
  @postcon Either a new reference is added to the collection if it does not exist else the existing 
           reference is updated.

  @param   strEditWindowName as a String as a constant
  @param   AFrame            as a TframeBADIModuleChecksEditorView as a constant

**)
Procedure TBADIModuleChecksEditorView.TBADIFrameManager.Add(Const strEditWindowName: String;
  Const AFrame: TframeBADIModuleChecksEditorView);

Var
  iIndex: Integer;
  R: TBADIFrameManagerRecord;

Begin
  iIndex := Find(strEditWindowName);
  If iIndex = -1 Then
    Begin // Create new view
      R.FEditWindowName := strEditWindowName;
      R.FFrameReference := AFrame;
      FFrames.Add(R);
    End Else
    Begin // Update existing reference
      R := FFrames[iIndex];
      R.FFrameReference := AFrame;
      FFrames[iIndex] := R;
    End;
End;

(**

  A constructor for the TBADIViewManager class.

  @precon  None.
  @postcon Creates an empty collection.

**)
Constructor TBADIModuleChecksEditorView.TBADIFrameManager.Create;

Begin
  FFrames := TList<TBADIFrameManagerRecord>.Create;
End;

(**

  A destructor for the TBADIViewManager class.

  @precon  None.
  @postcon Frees the collection.

**)
Destructor TBADIModuleChecksEditorView.TBADIFrameManager.Destroy;

Begin
  FFrames.Free;
  Inherited Destroy;
End;

(**

  This method attempts to find the given view name in the collection and if foud returns the index
  else returns -1 for not found.

  @precon  None.
  @postcon Returns the index of the named view if found else -1.

  @param   strEditWindowName as a String as a constant
  @return  an Integer

**)
Function TBADIModuleChecksEditorView.TBADIFrameManager.Find(Const strEditWindowName: String): Integer;

Var
  iView: Integer;

Begin
  Result := -1;
  For iView := 0 To FFrames.Count - 1 Do
    If CompareText(FFrames[iView].FEditWindowName, strEditWindowName) = 0 Then
      Begin
        Result := iView;
        Break;
      End;
End;

(**

  This is a getter method for the Frame property.

  @precon  None.
  @postcon If the named edit window is found in the collection then the associated frame is returned 
           else nil is returned for not found.

  @param   strEditWindowName as a String as a constant
  @return  a TframeBADIModuleChecksEditorView

**)
Function TBADIModuleChecksEditorView.TBADIFrameManager.GetFrame(
  Const strEditWindowName: String): TframeBADIModuleChecksEditorView;

Var
  iIndex: Integer;
  
Begin
  Result := Nil;
  iIndex := Find(strEditWindowName);
  If iIndex > -1 Then
    Result := FFrames[iiNdex].FFrameReference;
End;

(**

  This method is called when this view tab in the editor is being requested to close. Return true to
  allow if the close else return false for it to persist.

  @precon  None.
  @postcon I return true here so it can be closed.

  @param   Allowed as a Boolean as a reference

**)
Procedure TBADIModuleChecksEditorView.Close(Var Allowed: Boolean);

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
Procedure TBADIModuleChecksEditorView.CloseAllCalled(Var ShouldClose: Boolean);

Begin
  ShouldClose := True;
End;

(**

  This method is called when each editor status panel is created.

  @precon  None.
  @postcon References to the panels are storede for later use and each panel is configured.

  @nocheck MissingCONSTInParam
  @nohint  StatusBar

  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel

**)
Procedure TBADIModuleChecksEditorView.ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);

Const
  iPanelWidth = 100;

Begin
  FModulePanels[TBADICheckstatusPanel(Panel.Index)] := Panel;
  FModulePanels[TBADICheckstatusPanel(Panel.Index)].Alignment := taCenter;
  FModulePanels[TBADICheckstatusPanel(Panel.Index)].Width := iPanelWidth;
  // Problems with first panel if you do not explicitly set this
  FModulePanels[TBADICheckstatusPanel(Panel.Index)].Style := psOwnerDraw; // psText; 
End;

(**

  A constructor for the TBADIModuleChecks class.

  @precon  None.
  @postcon Adds an image to the editor image list to be displayed against this editor view.

  @param   strViewIdentifier as a String as a constant

**)
Constructor TBADIModuleChecksEditorView.Create(Const strViewIdentifier : String);

Const
  strBADIChecksImage = 'BADIChecksImage';

Var
  EVS : INTAEditorViewServices;
  ImageList : TImageList;
  BM: TBitmap;
  
Begin
  Inherited Create;
  FFrameManager := TBADIFrameManager.Create;
  FFileInfoMgr := TBADIFileInfoManager.Create;
  FSourceStrings := TStringList.Create;
  FViewIdent := strViewIdentifier;
  FCount := 0;
  If Supports(BorlandIDEServices, INTAEditorViewServices, EVS) Then
    Begin
      ImageList := TImageList.Create(Nil);
      Try
        BM := TBitMap.Create;
        Try
          BM.LoadFromResourceName(HInstance, strBADIChecksImage);
          ImageList.AddMasked(BM, clFuchsia);
          FImageIndex := EVS.AddImages(ImageList, strBADIChecksEditorView);
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
Class Function TBADIModuleChecksEditorView.CreateEditorView : INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    Begin
      If Not Assigned(FEditorViewRef) Then
        FEditorViewRef := TBADIModuleChecksEditorView.Create('');
      Result := FEditorViewRef;
      EVS.ShowEditorView(Result);
    End;
End;

(**

  This method returns the name of the current top editor window.

  @precon  None.
  @postcon The name fo the top editor window is returned.

  @return  a String

**)
Function TBADIModuleChecksEditorView.CurrentEditWindow: String;

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
Procedure TBADIModuleChecksEditorView.DeselectView;

Begin
  // Does nothing
End;

(**

  A destructor for the TBADIModuleChecks class.

  @precon  None.
  @postcon Frees the memory used by the module (if not nil).

**)
Destructor TBADIModuleChecksEditorView.Destroy;

Begin
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
Procedure TBADIModuleChecksEditorView.DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);

  (**

    This method renders the background of the status bar panel.

    @precon  StyleServices must be a valid instance or Nil.
    @postcon The background of the status panel is rendered.

    @param   strNum        as a String as a constant
    @param   StyleServices as a TCustomStyleServices as a constant

  **)
  Procedure DrawBackground(Const strNum : String; Const StyleServices : TCustomStyleServices);

  Var
    iColour : TColor;

  Begin
    If TBADICheckstatusPanel(Panel.Index) In [mspModules..mspMethods] Then
      Begin
        iColour := clBtnFace;
        If Assigned(StyleServices) Then
          iColour := StyleServices.GetSystemColor(clBtnFace);
      End Else
        iColour := iLightGreen;
    If strNum <> '' Then
      Case TBADICheckstatusPanel(Panel.Index) Of
        mspOverLimit:
          If StrToInt(strNum) > 0 Then
            iColour := iLightRed;
      End;
    StatusBar.Canvas.Brush.Color := iColour;
    StatusBar.Canvas.FillRect(Rect);
  End;

  (**

    This width of the text in the status panel is calulated.

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

    @precon  StyleServeices must be a valid instance of Nil.
    @postcon The text of the status bar is rendered.

    @param   strNum        as a String as a reference
    @param   strSpace      as a String as a reference
    @param   strText       as a String as a reference
    @param   iWidth        as an Integer as a constant
    @param   StyleServices as a TCustomStyleServices as a constant

  **)
  Procedure DrawText(Var strNum, strSpace, strText : String; Const iWidth : Integer;
    Const StyleServices : TCustomStyleServices);

  Const
    iDivisor = 2;

  Var
    R : TRect;
    
  Begin
    R := Rect;
    // Draw Number
    Inc(R.Left, (R.Right - R.Left - iWidth) Div iDivisor);
    Inc(R.Top);
    StatusBar.Canvas.Font.Assign(StatusBar.Font);
    StatusBar.Canvas.Font.Color := clBlue;
    StatusBar.Canvas.Font.Style := [];
    StatusBar.Canvas.TextRect(R, strNum, [tfLeft, tfVerticalCenter]);
    // Draw Space
    Inc(R.Left, StatusBar.Canvas.TextWidth(strNum));
    StatusBar.Canvas.TextRect(R, strSpace, [tfLeft, tfVerticalCenter]);
    // Draw Text Label
    StatusBar.Canvas.Font.Color := clWindowText;
    If Assigned(StyleServices) Then
      StatusBar.Canvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
    StatusBar.Canvas.Font.Style := [fsBold];
    Inc(R.Left, StatusBar.Canvas.TextWidth(strSpace));
    StatusBar.Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
  End;

Var
  strNum, strSpace, strText : String;
  iPos : Integer;
  StyleServices : TCustomStyleServices;
  {$IFDEF DXE102}
  ITS : IOTAIDEThemingServices;
  {$ENDIF}
  
Begin
  StyleServices := Nil;
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      StyleServices := ITS.StyleServices;
  {$ENDIF}
  // Split text by first space
  iPos := Pos(#32, Panel.Text);
  strNum := Copy(Panel.Text, 1, Pred(iPos));
  strSpace := #32;
  strText := Copy(Panel.Text, Succ(iPos), Length(Panel.Text) - iPos);
  DrawBackground(strNum, StyleServices);
  DrawText(strNum, strSpace, strText, CalcWidth(strNum, strSpace, strText), StyleServices);
End;

(**

  This method is called for the given editor action that you have said is supported by the editor view.

  @precon  None.
  @postcon The treeview text is copied to the clipboard if that action is invoked.

  @nocheck MissingCONSTInParam
  
  @param   Action as a TEditAction
  @return  a Boolean

**)
Function TBADIModuleChecksEditorView.EditAction(Action: TEditAction): Boolean;

Var
  AFrame: TframeBADIModuleChecksEditorView;

Begin
  Result := False;
  Case Action Of
    eaCopy:
      Begin
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) Then
          AFrame.CopyToClipboard;
        Result := True;
      End;
  End;
End;

(**

  This method extracts the source code, filename and date information from a disk file.

  @precon  ModuleInfo must be a valid instance.
  @postcon The source code, filename and date information is retreived from the disk file.

**)
Procedure TBADIModuleChecksEditorView.ExtractSourceFromFile;

Begin
  FSource := '';
  If FileExists(FFileName) Then
    Begin
      FSourceStrings.LoadFromFile(FFileName);
      FSource := FSourceStrings.Text;
      FSourceStrings.Clear;
    End;
End;

(**

  This method extracts the source code, filename and date information from an in memory module.

  @precon  Module and ModuleInfo must be a valid instance.
  @postcon The source code, filename and date information is retreived from the in memory module.

  @param   Module     as an IOTAModule as a constant

**)
Procedure TBADIModuleChecksEditorView.ExtractSourceFromModule(Const Module : IOTAModule);

Var
  SE: IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  FSource := TBADIToolsAPIFunctions.EditorAsString(SE);
End;

(**

  This method is called when the frame is first created.

  @precon  None.
  @postcon Stores a reference to the frame so that a modules Checks can be rendered

  @nocheck MissingCONSTInParam
  
  @param   AFrame as a TCustomFrame

**)
Procedure TBADIModuleChecksEditorView.FrameCreated(AFrame: TCustomFrame);

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
      FFrameManager.Add(strEditWindowName, AFrame As TframeBADIModuleChecksEditorView);
    End;
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Returns false as this editor view should not be cloned (think singleton view).

  @return  a Boolean

**)
Function TBADIModuleChecksEditorView.GetCanCloneView: Boolean;

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
Function TBADIModuleChecksEditorView.GetCaption: String;

ResourceString
  strChecks = 'Checks';

Const
  iDivisor = 2;

Begin
  Inc(FCount);
  If FCount Mod iDivisor = 0 Then
    Result := strChecks
  Else
    Result := strBADIChecks;
End;

(**

  This is a getter method for the EditorWindowCaption property.

  @precon  None.
  @postcon Returns the text to be displayed in the Editor Window (you can only see this when the editor
           is floating).

  @return  a String

**)
Function TBADIModuleChecksEditorView.GetEditorWindowCaption: String;

Begin
  Result := strBADIChecks;
End;

(**

  This is a getter method for the EditState property.

  @precon  None.
  @postcon This method is called to tell the IDE what editor state can be invoked on the data in the
           view (cut, copy, paste, etc).

  @return  a TEditState

**)
Function TBADIModuleChecksEditorView.GetEditState: TEditState;

Begin
  Result := [esCanCopy];
End;

(**

  This is a getter method for the FrameClass property.

  @precon  None.
  @postcon The method returns the frame class that the IDE should create when creating the editor view.

  @return  a TCustomFrameClass

**)
Function TBADIModuleChecksEditorView.GetFrameClass: TCustomFrameClass;

Begin
  Result := TframeBADIModuleChecksEditorView;
End;

(**

  This is a getter method for the ImageIndex property.

  @precon  None.
  @postcon Returns the image index of the image in the editor image list for this editor view.

  @return  an Integer

**)
Function TBADIModuleChecksEditorView.GetImageIndex: Integer;

Begin
  Result := FImageIndex;
End;

(**

  This is a getter method for the StatusPanelCount property.

  @precon  None.
  @postcon Returns the number of status panels to create for the editor view.

  @return  an Integer

**)
Function TBADIModuleChecksEditorView.GetStatusPanelCount: Integer;

Begin
  Result := Ord(High(TBADICheckstatusPanel)) - Ord(Low(TBADICheckstatusPanel)) + 1;
End;

(**

  This is a getter method for the TabHintText property.

  @precon  None.
  @postcon Returns the text to be displayed when the mouse is hovered over the editor tab.

  @return  a String

**)
Function TBADIModuleChecksEditorView.GetTabHintText: String;

Begin
  Result := strBADIChecks;
End;

(**

  This is a getter method for the ViewIdentifer property.

  @precon  None.
  @postcon Returns a unique identifier for this view (must be unique within the IDE - think singlton
           instance).

  @return  a String

**)
Function TBADIModuleChecksEditorView.GetViewIdentifier: String;

Begin
  Result := Format('%s.%s', [strBADIChecksEditorView, FViewIdent]);
End;

(**

  This method retrieves the last modified date of the module from disk.

  @precon  ModuleInfo must be a valid instance.
  @postcon The FFileName, FFileDate and FModified fields are updated.

  @param   ModuleInfo as an IOTAModuleInfo as a constant

**)
Procedure TBADIModuleChecksEditorView.LastModifiedDateFromFile(Const ModuleInfo: IOTAModuleInfo);

Begin
  FFileName := ModuleInfo.FileName;
  FileAge(FFileName, FFileDate);
  FModified := False;
End;

(**

  This method retrieves the last modified date of the module from the IDE.

  @precon  Module must be a valid instance.
  @postcon The FFileName, FFileDate and FModified fields are updated.

  @param   Module as an IOTAModule as a constant

**)
Procedure TBADIModuleChecksEditorView.LastModifiedDateFromModule(Const Module: IOTAModule);

Var
  SE : IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  FModified := SE.Modified;
  FFileName := Module.FileName;
  If Not FModified Then
    FileAge(FFileName, FFileDate)
  Else
    FFileDate := Now();
End;

(**

  This method parses the source code and renders the module in the Checks frame.

  @precon  None.
  @postcon The source code is parsed and rendered.

**)
Procedure TBADIModuleChecksEditorView.ParseAndRender;

Var
  Module : TBaseLanguageModule;
  AFrame: TframeBADIModuleChecksEditorView;

Begin
  FFileInfoMgr.Add(FFileName, FFileDate);
  If (Length(FSource) > 0) And (Length(FFileName) > 0) Then
    Begin
      Module := TBADIDispatcher.BADIDispatcher.Dispatcher(FSource, FFileName, FModified, [moParse]);
      Try
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) Then
          AFrame.RenderModule(Module, [croAutoExpand, croAutoExpandOnError]);
      Finally
        Module.Free;
      End;
    End;
End;

(**

  This method process the module extracting the filename, date time and source code and the pass it
  for parsing.

  @precon  ModuleInfo must be a valid instance.
  @postcon Process the module extracting the filename, date time and source code and the pass it
           for parsing.

  @param   ModuleInfo as an IOTAModuleInfo as a constant

**)
Procedure TBADIModuleChecksEditorView.ProcesModule(Const ModuleInfo : IOTAModuleInfo);

Var
  Module: IOTAModule;

Begin
  FModified := False;
  Module := (BorlandIDEServices As IOTAModuleServices).FindModule(ModuleInfo.FileName);
  If Assigned(Module) Then
    LastModifiedDateFromModule(Module)
  Else
    LastModifiedDateFromFile(ModuleInfo);
  If FFileInfoMgr.ShouldUpdate(FFileName, FFileDate) Then
    Begin
      If Assigned(Module) Then
        ExtractSourceFromModule(Module)
      Else
        ExtractSourceFromFile;
      ParseAndRender;
    End;
End;

(**

  This method extracts the current module checks into a set so that we can see if the last render was
  done with a different set of options and thus we need to force a complete reparsing of the modules.

  @precon  None.
  @postcon The current check options are returned.

  @return  a TBADIModuleChecks

**)
Function TBADIModuleChecksEditorView.RenderedList: TBADIModuleChecks;

Var
  eCheck: TBADIModuleCheck;

Begin
  Result := [];
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    If TBADIOptions.BADIOptions.ModuleCheck[eCheck].FEnabled Then
      Include(Result, eCheck);
End;

(**

  This method is called when the editor view is selected either when its created or when it regains
  focus.

  @precon  None.
  @postcon Renders the modules Checks in the frame.

**)
Procedure TBADIModuleChecksEditorView.SelectView;

ResourceString
  strParsingProjectModules = 'Parsing project modules';
  strPleaseWait = 'Please wait...';
  strParsing = 'Parsing: %s...';

Const
  setModuleTypesToParse = [omtForm, omtDataModule, omtProjUnit, omtUnit];
  
Var
  P: IOTAProject;
  iModule: Integer;
  frmProgress : TfrmProgress;
  ModuleInfo: IOTAModuleInfo;
  AFrame: TframeBADIModuleChecksEditorView;

Begin
  P := TBADIToolsAPIFunctions.ActiveProject;
  If Assigned(P) Then
    Begin
      If FLastRenderedList <> RenderedList Then
        FFileInfoMgr.Clear;
      FLastRenderedList := RenderedList;
      frmProgress := TfrmProgress.Create(Application.MainForm);
      Try
        frmProgress.Init(P.GetModuleCount, strParsingProjectModules, strPleaseWait);
        For iModule := 0 To P.GetModuleCount - 1 Do
          Begin
            ModuleInfo := P.GetModule(iModule);
            If ModuleInfo.ModuleType In setModuleTypesToParse Then
              Begin
                ProcesModule(ModuleInfo);
                frmProgress.UpdateProgress(Succ(iModule), Format(strParsing,
                  [ExtractFileName(FFileName)]));
              End
          End;
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) Then
          AFrame.FocusResults;
      Finally
        frmProgress.Free;
      End;
      UpdateStatusPanels;
    End;
End;

(**

  This method updates the status panels with the information from the frame.

  @precon  None.
  @postcon The status panels are updated.

**)
Procedure TBADIModuleChecksEditorView.UpdateStatusPanels;

ResourceString
  strModules = '%d Modules';
  strMethods = '%d Methods';
  strUnderLimit = '%d < Limit';
  strOverLimit = '%d > Limit';

Var
  AFrame: TframeBADIModuleChecksEditorView;
  
Begin
  AFrame := FFrameManager.Frame[CurrentEditWindow];
  If Assigned(AFrame) Then
    Begin
      FModulePanels[mspModules].Text :=     Format(strModules,     [AFrame.ModuleCount]);
      FModulePanels[mspMethods].Text :=     Format(strMethods,     [AFrame.MethodCount]);
      FModulePanels[mspUnderLimit].Text :=  Format(strUnderLimit,  [AFrame.UnderLimit]);
      FModulePanels[mspOverLimit].Text :=   Format(strOverLimit,   [AFrame.OverLimit]);
    End;
End;

End.

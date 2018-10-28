(**
  
  This module contains Open Tools API code to create an editor view to display the BADI Metrics
  (metrics).

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Oct 2018
  
**)
Unit BADI.Module.Metrics;

Interface

Uses
  ToolsAPI,
  DesignIntf,
  Forms, 
  ComCtrls,
  Windows,
  Classes, 
  BADI.Module.Metrics.EditorView.Frame, 
  BADI.Base.Module,
  Generics.Collections,
  Themes, 
  BADI.Types;
   
{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to implement an editor view for displaying module metrics. @nometrics MissingCONSTInParam **)
  TBADIModuleMetricsEditorView = Class(TInterfacedObject, INTACustomEditorView, INTACustomEditorView150,
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
      (** An enumerate to define the status panels to be shown with the metrics. **)
      TBADIMetricStatusPanel = (mspModules, mspMethods, mspLinesOfCode, mspUnderLimit, mspAtLimit,
        mspOverLimit);
      (** A class to manage the frames against the editor windows. **)
      TBADIFrameManager = Class
      Strict Private
        Type
          (** A record to stored information about each view. **)
          TBADIFrameManagerRecord  = Record
            FEditWindowName : String;
            FFrameReference : TframeBADIModuleMetricsEditorView;
          End;
      Strict Private
        FFrames : TList<TBADIFrameManagerRecord>;
      Strict Protected
        Function  GetFrame(Const strEditWindowName : String) : TframeBADIModuleMetricsEditorView;
        Function  Find(Const strEditWindowName :String) : Integer;
      Public
        Constructor Create;
        Destructor Destroy; Override;
        Procedure Add(Const strEditWindowName : String; Const AFrame : TframeBADIModuleMetricsEditorView);
        (**
          A property to returned the frame associated with the given edit window.
          @precon  None.
          @postcon Returned the frame associated with the given edit window.
          @param   strEditWindowName as a String as a constant
          @return  a TframeBADIModuleMetricsEditorView
        **)
        Property Frame[Const strEditWindowName : String] : TframeBADIModuleMetricsEditorView Read GetFrame;
      End;
    Class Var
      (** A single class var reference to the editor view. **)
      FEditorViewRef : INTACustomEditorView;
  Strict Private
    FFrameManager     : TBADIFrameManager;
    FFileInfoMgr      : TBADIFileInfoManager;
    FImageIndex       : Integer;
    FViewIdent        : String;
    FModulePanels     : Array[Low(TBADIMetricStatusPanel)..High(TBADIMetricStatusPanel)] Of TStatusPanel;
    FCount            : Integer;
    FSourceStrings    : TStringList;
    FSource           : String;
    FFileName         : String;
    FModified         : Boolean;
    FFileDate         : TDateTime;
    FLastRenderedList : TBADIModuleMetrics;
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
    Function  RenderedList : TBADIModuleMetrics;
  Public
    Class Function CreateEditorView: INTACustomEditorView;
    Constructor Create(Const strViewIdentifier : String);
    Destructor Destroy; Override;
  End;

  Procedure RegisterMetricsEditorView;
  Procedure UnregisterMetricsEditorView;
  
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
  strBADIMetricsEditorView = 'BADIMetricsEditorView';
  (** A caption for the editor view. **)
  strBADIMetrics = 'BADI Metrics';
  (** A default edit window name if one cannot be determined. **)
  strUnknown = 'Unknown';

(**

  This method returns an instance of the custom editor view and passed in the registration of the view
  so that a view can be created when a desktop is loaded.

  @precon  None.
  @postcon An instance of this custom editoe view is returned.

  @return  an INTACustomEditorView

**)
Function RecreateBADIStatisticEditorView: INTACustomEditorView;

Begin
  Result := TBADIModuleMetricsEditorView.CreateEditorView;
End;

(**

  This method is called from the main wizard constructor to regsiter this custom editor view.

  @precon  None.
  @postcon The custom editor view is registered with the IDE.

**)
Procedure RegisterMetricsEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.RegisterEditorView(strBADIMetricsEditorView, RecreateBADIStatisticEditorView);
End;

(**

  This method is called from the main wizard destructor to unregsiter this custom editor view.

  @precon  None.
  @postcon The custom editor view is unregistered from the IDE.

**)
Procedure UnregisterMetricsEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorView(strBADIMetricsEditorView);
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
Procedure TBADIModuleMetricsEditorView.TBADIFileInfoManager.Add(Const strFileName: String;
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
Procedure TBADIModuleMetricsEditorView.TBADIFileInfoManager.Clear;

Begin
  FFileInfo.Clear;
End;

(**

  A constructor for the TBADIFileInfoManager class.

  @precon  None.
  @postcon Creates an empty collection.

**)
Constructor TBADIModuleMetricsEditorView.TBADIFileInfoManager.Create;

Begin
  FFileInfo := TList<TBADIModuleUpdateRecord>.Create;
End;

(**

  A destructor for the TBADIFileInfoManager class.

  @precon  None.
  @postcon Fress the memory used by the collection.

**)
Destructor TBADIModuleMetricsEditorView.TBADIFileInfoManager.Destroy;

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
Function TBADIModuleMetricsEditorView.TBADIFileInfoManager.Find(Const strFileName: String): Integer;

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
Function TBADIModuleMetricsEditorView.TBADIFileInfoManager.ShouldUpdate(Const strFileName: String;
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
Function TBADIModuleMetricsEditorView.CloneEditorView: INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.CloseActiveEditorView;
  Result := RecreateBADIStatisticEditorView;
End;

{ TBADIModuleMetrics.TBADIViewManager }

(**

  This method adds the given edit window name and frame referencce pair to the collection is it does not 
  already exists else it updates the existing records frame reference.

  @precon  AFrame must be a valid instance.
  @postcon Either a new reference is added to the collection if it does not exist else the existing 
           reference is updated.

  @param   strEditWindowName as a String as a constant
  @param   AFrame            as a TframeBADIModuleMetricsEditorView as a constant

**)
Procedure TBADIModuleMetricsEditorView.TBADIFrameManager.Add(Const strEditWindowName: String;
  Const AFrame: TframeBADIModuleMetricsEditorView);

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
Constructor TBADIModuleMetricsEditorView.TBADIFrameManager.Create;

Begin
  FFrames := TList<TBADIFrameManagerRecord>.Create;
End;

(**

  A destructor for the TBADIViewManager class.

  @precon  None.
  @postcon Frees the collection.

**)
Destructor TBADIModuleMetricsEditorView.TBADIFrameManager.Destroy;

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
Function TBADIModuleMetricsEditorView.TBADIFrameManager.Find(Const strEditWindowName: String): Integer;

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
  @return  a TframeBADIModuleMetricsEditorView

**)
Function TBADIModuleMetricsEditorView.TBADIFrameManager.GetFrame(
  Const strEditWindowName: String): TframeBADIModuleMetricsEditorView;

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
Procedure TBADIModuleMetricsEditorView.Close(Var Allowed: Boolean);

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
Procedure TBADIModuleMetricsEditorView.CloseAllCalled(Var ShouldClose: Boolean);

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
Procedure TBADIModuleMetricsEditorView.ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);

Const
  iPanelWidth = 80;

Begin
  FModulePanels[TBADIMetricStatusPanel(Panel.Index)] := Panel;
  FModulePanels[TBADIMetricStatusPanel(Panel.Index)].Alignment := taCenter;
  FModulePanels[TBADIMetricStatusPanel(Panel.Index)].Width := iPanelWidth;
  // Problems with first panel if you do not explicitly set this
  FModulePanels[TBADIMetricStatusPanel(Panel.Index)].Style := psOwnerDraw; // psText; 
End;

(**

  A constructor for the TBADIModuleMetrics class.

  @precon  None.
  @postcon Adds an image to the editor image list to be displayed against this editor view.

  @param   strViewIdentifier as a String as a constant

**)
Constructor TBADIModuleMetricsEditorView.Create(Const strViewIdentifier : String);

Const
  strBADIMetricsImage = 'BADIMetricsImage';

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
          BM.LoadFromResourceName(HInstance, strBADIMetricsImage);
          ImageList.AddMasked(BM, clLime);
          FImageIndex := EVS.AddImages(ImageList, strBADIMetricsEditorView);
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
Class Function TBADIModuleMetricsEditorView.CreateEditorView : INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    Begin
      If Not Assigned(FEditorViewRef) Then
        FEditorViewRef := TBADIModuleMetricsEditorView.Create('');
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
Function TBADIModuleMetricsEditorView.CurrentEditWindow: String;

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
Procedure TBADIModuleMetricsEditorView.DeselectView;

Begin
  // Does nothing
End;

(**

  A destructor for the TBADIModuleMetrics class.

  @precon  None.
  @postcon Frees the memory used by the module (if not nil).

**)
Destructor TBADIModuleMetricsEditorView.Destroy;

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
Procedure TBADIModuleMetricsEditorView.DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);

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
    If TBADIMetricStatusPanel(Panel.Index) In [mspModules..mspLinesOfCode] Then
      Begin
        iColour := clBtnFace;
        If Assigned(StyleServices) Then
          iColour := StyleServices.GetSystemColor(clBtnFace);
      End Else
        iColour := iLightGreen;
    If strNum <> '' Then
      Case TBADIMetricStatusPanel(Panel.Index) Of
        mspAtLimit:
          If StrToInt(strNum) > 0 Then
            iColour := iLightAmber;
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
Function TBADIModuleMetricsEditorView.EditAction(Action: TEditAction): Boolean;

Var
  AFrame: TframeBADIModuleMetricsEditorView;

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
Procedure TBADIModuleMetricsEditorView.ExtractSourceFromFile;

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
Procedure TBADIModuleMetricsEditorView.ExtractSourceFromModule(Const Module : IOTAModule);

Var
  SE: IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  FSource := TBADIToolsAPIFunctions.EditorAsString(SE);
End;

(**

  This method is called when the frame is first created.

  @precon  None.
  @postcon Stores a reference to the frame so that a modules metrics can be rendered

  @nocheck MissingCONSTInParam
  
  @param   AFrame as a TCustomFrame

**)
Procedure TBADIModuleMetricsEditorView.FrameCreated(AFrame: TCustomFrame);

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
      FFrameManager.Add(strEditWindowName, AFrame As TframeBADIModuleMetricsEditorView);
    End;
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Returns false as this editor view should not be cloned (think singleton view).

  @return  a Boolean

**)
Function TBADIModuleMetricsEditorView.GetCanCloneView: Boolean;

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
Function TBADIModuleMetricsEditorView.GetCaption: String;

ResourceString
  strMetrics = 'Metrics';

Const
  iDivisor = 2;

Begin
  Inc(FCount);
  If FCount Mod iDivisor = 0 Then
    Result := strMetrics
  Else
    Result := strBADIMetrics;
End;

(**

  This is a getter method for the EditorWindowCaption property.

  @precon  None.
  @postcon Returns the text to be displayed in the Editor Window (you can only see this when the editor
           is floating).

  @return  a String

**)
Function TBADIModuleMetricsEditorView.GetEditorWindowCaption: String;

Begin
  Result := strBADIMetrics;
End;

(**

  This is a getter method for the EditState property.

  @precon  None.
  @postcon This method is called to tell the IDE what editor state can be invoked on the data in the
           view (cut, copy, paste, etc).

  @return  a TEditState

**)
Function TBADIModuleMetricsEditorView.GetEditState: TEditState;

Begin
  Result := [esCanCopy];
End;

(**

  This is a getter method for the FrameClass property.

  @precon  None.
  @postcon The method returns the frame class that the IDE should create when creating the editor view.

  @return  a TCustomFrameClass

**)
Function TBADIModuleMetricsEditorView.GetFrameClass: TCustomFrameClass;

Begin
  Result := TframeBADIModuleMetricsEditorView;
End;

(**

  This is a getter method for the ImageIndex property.

  @precon  None.
  @postcon Returns the image index of the image in the editor image list for this editor view.

  @return  an Integer

**)
Function TBADIModuleMetricsEditorView.GetImageIndex: Integer;

Begin
  Result := FImageIndex;
End;

(**

  This is a getter method for the StatusPanelCount property.

  @precon  None.
  @postcon Returns the number of status panels to create for the editor view.

  @return  an Integer

**)
Function TBADIModuleMetricsEditorView.GetStatusPanelCount: Integer;

Begin
  Result := Ord(High(TBADIMetricStatusPanel)) - Ord(Low(TBADIMetricStatusPanel)) + 1;
End;

(**

  This is a getter method for the TabHintText property.

  @precon  None.
  @postcon Returns the text to be displayed when the mouse is hovered over the editor tab.

  @return  a String

**)
Function TBADIModuleMetricsEditorView.GetTabHintText: String;

Begin
  Result := strBADIMetrics;
End;

(**

  This is a getter method for the ViewIdentifer property.

  @precon  None.
  @postcon Returns a unique identifier for this view (must be unique within the IDE - think singlton
           instance).

  @return  a String

**)
Function TBADIModuleMetricsEditorView.GetViewIdentifier: String;

Begin
  Result := Format('%s.%s', [strBADIMetricsEditorView, FViewIdent]);
End;

(**

  This method retrieves the last modified date of the module from disk.

  @precon  ModuleInfo must be a valid instance.
  @postcon The FFileName, FFileDate and FModified fields are updated.

  @param   ModuleInfo as an IOTAModuleInfo as a constant

**)
Procedure TBADIModuleMetricsEditorView.LastModifiedDateFromFile(Const ModuleInfo: IOTAModuleInfo);

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
Procedure TBADIModuleMetricsEditorView.LastModifiedDateFromModule(Const Module: IOTAModule);

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

  This method parses the source code and renders the module in the metrics frame.

  @precon  None.
  @postcon The source code is parsed and rendered.

**)
Procedure TBADIModuleMetricsEditorView.ParseAndRender;

Var
  Module : TBaseLanguageModule;
  AFrame: TframeBADIModuleMetricsEditorView;

Begin
  FFileInfoMgr.Add(FFileName, FFileDate);
  If (Length(FSource) > 0) And (Length(FFileName) > 0) Then
    Begin
      Module := TBADIDispatcher.BADIDispatcher.Dispatcher(FSource, FFileName, FModified, [moParse]);
      Try
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) Then
          AFrame.RenderModule(Module, [mroAutoExpand, mroAutoExpandOnError]);
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
Procedure TBADIModuleMetricsEditorView.ProcesModule(Const ModuleInfo : IOTAModuleInfo);

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

  This method extracts the current module metrics into a set so that we can see if the last render was
  done with a different set of options and thus we need to force a complete reparsing of the modules.

  @precon  None.
  @postcon The current metric options are returned.

  @return  a TBADIModuleMetrics

**)
Function TBADIModuleMetricsEditorView.RenderedList: TBADIModuleMetrics;

Var
  eMetric: TBADIModuleMetric;

Begin
  Result := [];
  For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
    If TBADIOptions.BADIOptions.ModuleMetric[eMetric].FEnabled Then
      Include(Result, eMetric);
End;

(**

  This method is called when the editor view is selected either when its created or when it regains
  focus.

  @precon  None.
  @postcon Renders the modules metrics in the frame.

**)
Procedure TBADIModuleMetricsEditorView.SelectView;

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
  AFrame: TframeBADIModuleMetricsEditorView;

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
Procedure TBADIModuleMetricsEditorView.UpdateStatusPanels;

ResourceString
  strModules = '%d Modules';
  strMethods = '%d Methods';
  strLinesOfCode = '%d Lines';
  strUnderLimit = '%d < Limit';
  strAtLimit = '%d @ Limit';
  strOverLimit = '%d > Limit';

Var
  AFrame: TframeBADIModuleMetricsEditorView;
  
Begin
  AFrame := FFrameManager.Frame[CurrentEditWindow];
  If Assigned(AFrame) Then
    Begin
      FModulePanels[mspModules].Text :=     Format(strModules,     [AFrame.ModuleCount]);
      FModulePanels[mspMethods].Text :=     Format(strMethods,     [AFrame.MethodCount]);
      FModulePanels[mspLinesOfCode].Text := Format(strLinesOfCode, [AFrame.LinesOfCode]);
      FModulePanels[mspUnderLimit].Text :=  Format(strUnderLimit,  [AFrame.UnderLimit]);
      FModulePanels[mspAtLimit].Text :=     Format(strAtLimit,     [AFrame.AtLimit]);
      FModulePanels[mspOverLimit].Text :=   Format(strOverLimit,   [AFrame.OverLimit]);
    End;
End;

End.

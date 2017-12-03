(**
  
  This module contains Open Tools API code to create an editor view to display the BADI Statistics
  (metrics).

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Dec 2017
  
**)
Unit BADI.Module.Statistics;

Interface

Uses
  ToolsAPI,
  DesignIntf,
  Forms, 
  ComCtrls,
  Windows,
  BADI.Module.Statistics.Frame, 
  BADI.Base.Module;
   
Type
  (** An interface define a render method for displaying the module metrics. **)
  IBADIModuleStatistics = Interface
  ['{AE1808CC-1201-4CAA-BB4A-2B0405A5FA87}']
    Procedure RenderModule(Const SourceEditor : IOTASourceEditor);
  End;

  (** An enumerate to define the status panels to be shown with the metrics. **)
  TBADIMetricStatusPanel = (mspModules, mspMethods, mspUnderLimit, mspAtLimit, mspOverLimit);

  (** A class to implement an editor view for displaying module metrics. @nometrics MissingCONSTInParam **)
  TBADIModuleStatistics = Class(TInterfacedObject, INTACustomEditorView, INTACustomEditorView150,
    INTACustomEditorViewStatusPanel, IBADIModuleStatistics)
  Strict Private
    FFrame         : TframeBADIModuleStatistics;
    FModule        : TBaseLanguageModule;
    FImageIndex    : Integer;
    FViewIdent     : String;
    FModulePanels  : Array[Low(TBADIMetricStatusPanel)..High(TBADIMetricStatusPanel)] Of TStatusPanel;
    FCount : Integer;
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
    // IBADIModuleStatistics
    Procedure RenderModule(Const SourceEditor : IOTASourceEditor);
  Public
    Class Function CreateEditorView(Const SourceEditor: IOTASourceEditor) : INTACustomEditorView;
    Constructor Create(Const strViewIdentifier : String);
    Destructor Destroy; Override;
  End;

  Procedure RegisterStatisticsEditorView;
  Procedure UnregisterStatisticsEditorView;
  
Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils, 
  BADI.Module.Dispatcher, 
  BADI.Types, 
  BADI.ToolsAPIUtils,
  Controls, 
  Vcl.Graphics,
  Generics.Collections,
  Classes;

Type
  (** A record to stored information about each view. **)
  TViewManagerRecord  = Record
    FViewName      : String;
    FViewReference : INTACustomEditorView;
  End;
  
Const
  (** A unique name for the editor view. **)
  strBADIStatisticEditorView = 'BADIStatisticEditorView';
  (** A caption for the editor view. **)
  strBADIStatistics = 'BADI Statistics';

Var
  (** A private variable to hold a reference to the view manager. **)
  ViewManager : TList<TViewManagerRecord>;

(**

  This method returns an instance of the custom editor view and passed in the registration of the view
  so that a view can be created when a desktop is loaded.

  @precon  None.
  @postcon An instance of this custom editoe view is returned.

  @return  an INTACustomEditorView

**)
Function RecreateBADIStatisticEditorView: INTACustomEditorView;

Begin
  Result := TBADIModuleStatistics.CreateEditorView(Nil);
End;

(**

  This method is called from the main wizard constructor to regsiter this custom editor view.

  @precon  None.
  @postcon The custom editor view is registered with the IDE.

**)
Procedure RegisterStatisticsEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.RegisterEditorView(strBADIStatisticEditorView, RecreateBADIStatisticEditorView);
End;

(**

  This method is called from the main wizard destructor to unregsiter this custom editor view.

  @precon  None.
  @postcon The custom editor view is unregistered from the IDE.

**)
Procedure UnregisterStatisticsEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorView(strBADIStatisticEditorView);
End;

(**

  This method is called when the IDE wants to clone the view and if the GetCanClose method returns true.

  @precon  None.
  @postcon You should return a cloned instance of your view if requested. I have not been able to get the
           IDE to ever call this method.

  @return  an INTACustomEditorView

**)
Function TBADIModuleStatistics.CloneEditorView: INTACustomEditorView;

Begin
  Result := Nil;
End;

(**

  This method is called when this view tab in the editor is being requested to close. Return true to
  allow if the close else return false for it to persist.

  @precon  None.
  @postcon I return true here so it can be closed.

  @param   Allowed as a Boolean as a reference

**)
Procedure TBADIModuleStatistics.Close(Var Allowed: Boolean);

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
Procedure TBADIModuleStatistics.CloseAllCalled(Var ShouldClose: Boolean);

Begin
  ShouldClose := True;
End;

(**

  This method is called when each editor status panel is created.

  @precon  None.
  @postcon References to the panels are storede for later use and each panel is configured.

  @nometric MissingCONSTInParam @nohint
  
  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel

**)
Procedure TBADIModuleStatistics.ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);

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

  A constructor for the TBADIModuleStatistics class.

  @precon  None.
  @postcon Adds an image to the editor image list to be displayed against this editor view.

  @param   strViewIdentifier as a String as a constant

**)
Constructor TBADIModuleStatistics.Create(Const strViewIdentifier : String);

Const
  strBADIStatisticsImage = 'BADIStatisticsImage';

Var
  EVS : INTAEditorViewServices;
  ImageList : TImageList;
  BM: TBitmap;
  
Begin
  Inherited Create;
  FViewIdent := strViewIdentifier;
  FCount := 0;
  If Supports(BorlandIDEServices, INTAEditorViewServices, EVS) Then
    Begin
      ImageList := TImageList.Create(Nil);
      Try
        BM := TBitMap.Create;
        Try
          BM.LoadFromResourceName(HInstance, strBADIStatisticsImage);
          ImageList.AddMasked(BM, clLime);
          FImageIndex := EVS.AddImages(ImageList, strBADIStatisticEditorView);
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

  @param   SourceEditor as an IOTASourceEditor as a constant
  @return  an INTACustomEditorView

**)
Class Function TBADIModuleStatistics.CreateEditorView(
  Const SourceEditor: IOTASourceEditor) : INTACustomEditorView;

Var
  ES : INTAEditorServices;
  EVS : IOTAEditorViewServices;
  strEditorWindowName: String;
  MS : IBADIModuleStatistics;
  ViewRecord : TVIewManagerRecord;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    Begin
      If Supports(BorlandIDEServices, INTAEditorServices, ES) Then
        If Assigned(ES.TopEditWindow) Then
          strEditorWindowName := ES.TopEditWindow.Form.Name;
      Result := Nil;
      For ViewRecord In ViewManager Do
        If ViewRecord.FViewName = strEditorWindowName Then
          Begin
            Result := ViewRecord.FViewReference;
            Break;
          End;
      If Not Assigned(Result) Then
        Begin
          ViewRecord.FViewName := strEditorWindowName;
          ViewRecord.FViewReference := TBADIModuleStatistics.Create(strEditorWindowName);
          ViewManager.Add(ViewRecord);
          Result := ViewRecord.FViewReference;
        End;
      If Supports(Result, IBADIModuleStatistics, MS) Then
        MS.RenderModule(SourceEditor);
      EVS.ShowEditorView(Result);
    End;
End;

(**

  This method is called when the editor view loses focus.

  @nometric EmptyMethod
  
  @precon  None.
  @postcon Does nothing.

**)
Procedure TBADIModuleStatistics.DeselectView;

Begin
  // Does nothing
End;

(**

  A destructor for the TBADIModuleStatistics class.

  @precon  None.
  @postcon Frees the memory used by the module (if not nil).

**)
Destructor TBADIModuleStatistics.Destroy;

Var
  iView : Integer;

Begin
  FModule.Free;
  For iView := 0 To ViewManager.Count - 1 Do
    If ViewManager[iView].FViewName = FViewIdent Then
      Begin
        ViewManager.Delete(iView);
        Break;
      End;
  Inherited Destroy;
End;

(**

  This method is called for each status panel if it is set to owner draw.

  @precon  None.
  @postcon Draw each panel with a blue number and black bold text.

  @nometric MissingCONSTInParam
  
  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TBADIModuleStatistics.DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);

Const
  iDivisor = 2;

Var
  strNum, strSpace, strText : String;
  iPos : Integer;
  iWidth : Integer;
  R : TRect;
  iColour : TColor;
  
Begin
  // Split text by first space
  iPos := Pos(#32, Panel.Text);
  strNum := Copy(Panel.Text, 1, Pred(iPos));
  strSpace := #32;
  strText := Copy(Panel.Text, Succ(iPos), Length(Panel.Text) - iPos);
  // Draw background
  If TBADIMetricStatusPanel(Panel.Index) In [mspModules..mspMethods] Then
    iColour := clBtnFace
  Else
    iColour := iLightGreen;
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
  // Get width of overall text so we can centre
  StatusBar.Canvas.Font.Color := clBlue;
  StatusBar.Canvas.Font.Style := [];
  iWidth := StatusBar.Canvas.TextWidth(strNum);
  Inc(iWidth, StatusBar.Canvas.TextWidth(strSpace));
  StatusBar.Canvas.Font.Color := clBlack;
  StatusBar.Canvas.Font.Style := [fsBold];
  Inc(iWidth, StatusBar.Canvas.TextWidth(strText));
  // Draw each bit of text
  R := Rect;
  Inc(R.Left, (R.Right - R.Left - iWidth) Div iDivisor);
  Inc(R.Top);
  StatusBar.Canvas.Font.Color := clBlue;
  StatusBar.Canvas.Font.Style := [];
  StatusBar.Canvas.TextRect(R, strNum, [tfLeft, tfVerticalCenter]);
  Inc(R.Left, StatusBar.Canvas.TextWidth(strNum));
  StatusBar.Canvas.TextRect(R, strSpace, [tfLeft, tfVerticalCenter]);
  StatusBar.Canvas.Font.Color := clBlack;
  StatusBar.Canvas.Font.Style := [fsBold];
  Inc(R.Left, StatusBar.Canvas.TextWidth(strSpace));
  StatusBar.Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
End;

(**

  This method is called for the given editor action that you have said is supported by the editor view.

  @precon  None.
  @postcon The treeview text is copied to the clipboard if that action is invoked.

  @nometric MissingCONSTInParam
  
  @param   Action as a TEditAction
  @return  a Boolean

**)
Function TBADIModuleStatistics.EditAction(Action: TEditAction): Boolean;

Begin
  Result := False;
  Case Action Of
    eaCopy:
      Begin
        FFrame.CopyToClipboard;
        Result := True;
      End;
  End;
End;

(**

  This method is called when the frame is first created.

  @precon  None.
  @postcon Stores a reference to the frame so that a modules metrics can be rendered

  @nometric MissingCONSTInParam
  
  @param   AFrame as a TCustomFrame

**)
Procedure TBADIModuleStatistics.FrameCreated(AFrame: TCustomFrame);

Begin
  FFrame := AFrame As TframeBADIModuleStatistics;
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Returns false as this editor view should not be cloned (think singleton view).

  @return  a Boolean

**)
Function TBADIModuleStatistics.GetCanCloneView: Boolean;

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
Function TBADIModuleStatistics.GetCaption: String;

ResourceString
  strMetrics = 'Metrics';

Const
  iDivisor = 2;

Begin
  Inc(FCount);
  If FCount Mod iDivisor = 0 Then
    Result := strMetrics
  Else
    Result := strBADIStatistics;
End;

(**

  This is a getter method for the EditorWindowCaption property.

  @precon  None.
  @postcon Returns the text to be displayed in the Editor Window (you can only see this when the editor
           is floating).

  @return  a String

**)
Function TBADIModuleStatistics.GetEditorWindowCaption: String;

Begin
  Result := strBADIStatistics;
End;

(**

  This is a getter method for the EditState property.

  @precon  None.
  @postcon This method is called to tell the IDE what editor state can be invoked on the data in the
           view (cut, copy, paste, etc).

  @return  a TEditState

**)
Function TBADIModuleStatistics.GetEditState: TEditState;

Begin
  Result := [esCanCopy];
End;

(**

  This is a getter method for the FrameClass property.

  @precon  None.
  @postcon The method returns the frame class that the IDE should create when creating the editor view.

  @return  a TCustomFrameClass

**)
Function TBADIModuleStatistics.GetFrameClass: TCustomFrameClass;

Begin
  Result := TframeBADIModuleStatistics;
End;

(**

  This is a getter method for the ImageIndex property.

  @precon  None.
  @postcon Returns the image index of the image in the editor image list for this editor view.

  @return  an Integer

**)
Function TBADIModuleStatistics.GetImageIndex: Integer;

Begin
  Result := FImageIndex;
End;

(**

  This is a getter method for the StatusPanelCount property.

  @precon  None.
  @postcon Returns the number of status panels to create for the editor view.

  @return  an Integer

**)
Function TBADIModuleStatistics.GetStatusPanelCount: Integer;

Begin
  Result := Ord(High(TBADIMetricStatusPanel)) - Ord(Low(TBADIMetricStatusPanel)) + 1;
End;

(**

  This is a getter method for the TabHintText property.

  @precon  None.
  @postcon Returns the text to be displayed when the mouse is hovered over the editor tab.

  @return  a String

**)
Function TBADIModuleStatistics.GetTabHintText: String;

Begin
  Result := strBADIStatistics;
End;

(**

  This is a getter method for the ViewIdentifer property.

  @precon  None.
  @postcon Returns a unique identifier for this view (must be unique within the IDE - think singlton
           instance).

  @return  a String

**)
Function TBADIModuleStatistics.GetViewIdentifier: String;

Begin
  Result := Format('%s.%s', [strBADIStatisticEditorView, FViewIdent]);
End;

(**

  This method is implemented from the interface and allows the caller to pass the source editor to the
  editor view for rendering. The module is parsed but not rendered until te editor view is selected.

  @precon  None.
  @postcon The source editor is parsed ready for rendering.

  @param   SourceEditor as an IOTASourceEditor as a constant

**)
Procedure TBADIModuleStatistics.RenderModule(Const SourceEditor: IOTASourceEditor);

Begin
  FModule := Nil;
  If Assigned(SourceEditor) Then
    FModule := TBADIDispatcher.BADIDispatcher.Dispatcher(
      EditorAsString(SourceEditor),
      SourceEditor.FileName,
      SourceEditor.Modified,
      [moParse]
    );
End;

(**

  This method is called when the editor view is selected either when its created or when it regains
  focus.

  @precon  None.
  @postcon Renders the modules metrics in the frame.

**)
Procedure TBADIModuleStatistics.SelectView;

ResourceString
  strModules = '%d Modules';
  strMethods = '%d Methods';
  strUnderLimit = '%d < Limit';
  strAtLimit = '%d @ Limit';
  strOverLimit = '%d > Limit';

Begin
  FFrame.RenderModule(FModule);
  FModulePanels[mspModules].Text := Format(strModules, [FFrame.ModuleCount]);
  FModulePanels[mspMethods].Text := Format(strMethods, [FFrame.MethodCount]);
  FModulePanels[mspUnderLimit].Text := Format(strUnderLimit, [FFrame.UnderLimit]);
  FModulePanels[mspAtLimit].Text := Format(strAtLimit, [FFrame.AtLimit]);
  FModulePanels[mspOverLimit].Text := Format(strOverLimit, [FFrame.OverLimit]);
End;

(** This section creates a view manager (Editor window name paired with the editor view interface. **)
Initialization
  ViewManager := TList<TViewManagerRecord>.Create;
(** This finalization section frees the view manager. **)
Finalization
  ViewManager.Free;
End.

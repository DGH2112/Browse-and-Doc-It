(**
  
  A module to provide an custom editor sub view for displaying the metrics for a module.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Dec 2017
  
**)
Unit BADI.Module.Statistics.SubView;

Interface

Uses
  ToolsAPI,
  StdActns,
  DesignIntf,
  Forms, 
  BADI.Module.Statistics.SubView.Frame;

Type
  (** A class to implement the INTACustomEditorSubView interface for a subview for metrics. **)
  TBADIModuleStatisticsSubView = Class(TInterfacedObject, INTACustomEditorSubView)
  Strict Private
    FFrame: TframeBADIModuleStatisticsSubView;
  Strict Protected
    // INTACustomEditorSubView
    Procedure Display(Const AContext: IInterface; AViewObject: TObject);
    Function EditAction(Const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;
    Procedure FrameCreated(AFrame: TCustomFrame);
    Function GetCanCloneView: Boolean;
    Function GetCaption: String;
    Function GetEditState(Const AContext: IInterface; AViewObject: TObject): TEditState;
    Function GetFrameClass: TCustomFrameClass;
    Function GetPriority: Integer;
    Function GetViewIdentifier: String;
    Function Handles(Const AContext: IInterface): Boolean;
    Procedure Hide(Const AContext: IInterface; AViewObject: TObject);
    Procedure ViewClosed(Const AContext: IInterface; AViewObject: TObject);
  Public
    Class Function CreateEditorMetricsSubView : INTACustomEditorSubView;
  End;

  Procedure RegisterEditorMetricsSubView;
  Procedure UnregisterEditorMetricsSubView;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils, 
  BADI.ToolsAPIUtils, 
  BADI.Module.Dispatcher, 
  BADI.Base.Module, 
  BADI.Types;

Var
  (** A private pointer to the regsitered subview so it can be removed. **)
  ptrEditorMetricsSubView : Pointer;

(**

  This method registers the custom editor subview with the IDE.

  @precon  None.
  @postcon The sub-view is regsitered with the IDE.

**)
Procedure RegisterEditorMetricsSubView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    ptrEditorMetricsSubView := EVS.RegisterEditorSubView(
      TBADIModuleStatisticsSubView.CreateEditorMetricsSubView);
End;

(**

  This method unregisters the custom editor subview from the IDE.

  @precon  None.
  @postcon The sub-view is unregsitered from the IDE.

**)
Procedure UnregisterEditorMetricsSubView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorSubView(ptrEditorMetricsSubView);
End;

{ TBADIModuleStatisticsSubView }

(**

  This is a class function to create the custom editor subview for displaying module metric information.

  @precon  None.
  @postcon Returns a new instance of the subview.

  @return  an INTACustomEditorSubView

**)
Class Function TBADIModuleStatisticsSubView.CreateEditorMetricsSubView: INTACustomEditorSubView;

Begin
  Result := TBADIModuleStatisticsSubView.Create;
End;

(**

  This method is called when a subview is displayed or hidden (AContext is nil).

  @precon  None.
  @postcon If the context is valid the modules editor text is parsed and passed to the subview for
           rendering.

  @nometric MissingCONSTInParam
  @nohint

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject

**)
Procedure TBADIModuleStatisticsSubView.Display(Const AContext: IInterface; AViewObject: TObject);

Var
  EVS : IOTAEditorViewServices;
  OTAModule : IOTAModule;
  Module : TBaseLanguageModule;
  strSource: String;
  SE : IOTASourceEditor;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    If Assigned(AContext) Then
      If EVS.ContextToModule(AContext, OTAModule) Then
        Begin
          SE := SourceEditor(OTAModule);
          strSource := EditorAsString(SE);
          Module := TBADIDispatcher.BADIDispatcher.Dispatcher(strSource, SE.FileName, SE.Modified,
            [moParse]);
          Try
            FFrame.RenderModule(Module, True);
          Finally
            Module.Free;
          End;
        End;
End;

(**

  This method is called by the IDE to undertake an editor actions the subview supports.

  @precon  None.
  @postcon If copy is the action the subview asks the frame to copy the metric information to the
           clipboard.

  @nometric MissingCONSTInParam
  @nohint

  @param   AContext    as an IInterface as a constant
  @param   Action      as a TEditAction
  @param   AViewObject as a TObject
  @return  a Boolean

**)
Function TBADIModuleStatisticsSubView.EditAction(Const AContext: IInterface; Action: TEditAction;
  AViewObject: TObject): Boolean;

Begin
  Result := False;
  Case Action Of
    eaCopy:
      Begin
        FFrame.CopyToClipboard;
        REsult := True;
      End;
  End;
End;

(**

  This method is called byt he IDE when it creates the frame instance.

  @precon  None.
  @postcon Store a reference to the frame instance so it can be used later in Display.

  @nometric MissingCONSTInParam

  @param   AFrame as a TCustomFrame

**)
Procedure TBADIModuleStatisticsSubView.FrameCreated(AFrame: TCustomFrame);

Begin
  FFrame := AFrame As TframeBADIModuleStatisticsSubView;
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Called by the IDE to know wehther the view can be closed. We return false to let the IDE know
           if cannot be cloned.

  @return  a Boolean

**)
Function TBADIModuleStatisticsSubView.GetCanCloneView: Boolean;

Begin
  Result := False;
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon Called by the IDE to get the subview tab caption.

  @return  a String

**)
Function TBADIModuleStatisticsSubView.GetCaption: String;

ResourceString
  strMetrics = 'Metrics';

Begin
  Result := strMetrics;
End;

(**

  This is a getter method for the EditState property.

  @precon  None.
  @postcon Called by the IDE to find out which editor actions can be undertaken byt he subview. We return
           a set with esCanCopy to signify that the subview can copy the mreic data to the clipboard.

  @nometric MissingCONSTInParam
  @nohint

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject
  @return  a TEditState

**)
Function TBADIModuleStatisticsSubView.GetEditState(Const AContext: IInterface;
  AViewObject: TObject): TEditState;

Begin
  Result := [esCanCopy];
End;

(**

  This is a getter method for the FrameClass property.

  @precon  None.
  @postcon This method is called by the IDE to get the class of the frame to be created by the IDE for
           the subview. We returns the class of our custom frame for viewing metrics.

  @return  a TCustomFrameClass

**)
Function TBADIModuleStatisticsSubView.GetFrameClass: TCustomFrameClass;

Begin
  Result := TframeBADIModuleStatisticsSubView;
End;

(**

  This is a getter method for the Priority property.

  @precon  None.
  @postcon This method is called by the IDE to find out the priority of the subview caption tab. Our
           metrics tab is low priority so that it is on the right of all other tabs.

  @return  an Integer

**)
Function TBADIModuleStatisticsSubView.GetPriority: Integer;

Begin
  Result := svpLow;
End;

(**

  This is a getter method for the ViewIdentifier property.

  @precon  None.
  @postcon Returns a unique name fo the subview.

  @return  a String

**)
Function TBADIModuleStatisticsSubView.GetViewIdentifier: String;

Const
  strBADIMetricsSubView = 'BADICustomEditorMetricsSubView';

Begin
  Result := strBADIMetricsSubView;
End;

(**

  This method is called by the IDE to understand which contexts are handled by the subview.

  @precon  None.
  @postcon We need to IOTAModule context for our subview so we return true for that context only.

  @param   AContext as an IInterface as a constant
  @return  a Boolean

**)
Function TBADIModuleStatisticsSubView.Handles(Const AContext: IInterface): Boolean;

Var
  EVS : IOTAEditorViewServices;
  Module: IOTAModule;
  
Begin
  Result := False;
  If Assigned(AContext) Then
    If Supports(BorlandIDEServices, IOTAEditorVIewServices, EVS) Then
      Result := EVS.ContextToModule(AContext, Module);
End;

(**

  This method is called when a subview is hidden by another subview.

  @precon  None.
  @postcon Not used.

  @nometric EmptyMethod MissingCONSTInParam
  @nohint

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject

**)
Procedure TBADIModuleStatisticsSubView.Hide(Const AContext: IInterface; AViewObject: TObject);

Begin
  // Do nothing
End;

(**

  This method is called then the IDE closes down and closes all the subviews all at once.

  @precon  None.
  @postcon Not used.

  @nometric MissingCONSTInParam EmptyMethod
  @nohint

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject

**)
Procedure TBADIModuleStatisticsSubView.ViewClosed(Const AContext: IInterface; AViewObject: TObject);

Begin
  // Do nothing
End;

End.


